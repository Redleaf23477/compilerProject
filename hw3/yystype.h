#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>

// #define YYSTYPE Node*

constexpr int WORD_SIZE = 8;

enum Tag {
    NOTAG,  // should not be assigned tag
    SDEC,   // scalar declaration
    ADEC,   // array declaration
    FDEC,   // function declaration
    FDEF,   // function definition
    EXPR,   // expr
    STMT    // stmt
};

std::string tag2str(Tag t);

// Operators

enum Operator {
    // Function Call
    op_call,
    // Arithmetic
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    // Assignment
    op_assign
};

std::string get_op_name(Operator op);

// AST Nodes Declaration

struct Node;
struct Type;
struct Declaration;
struct FuncDecl;
struct FuncDefn;
struct ScalarDecl;
struct TranslationUnit;
struct Statement;
struct ExpressionStatement;
struct Expression;
struct UnaryExpression;
struct BinaryExpression;
struct CallExpression;
struct Identifier;
struct Literal;

template<typename T = Node*> struct NodeList;

// Symbol Table (No type support yet)

enum VarMode {
    M_LOCAL,
    M_GLOBAL,
    M_PARAMETER
};

enum DataType {
    T_INT,
    T_PTR
};

std::string get_type_name(DataType type);

struct Symbol {
    std::string name;
    int scope;
    int offset;     // w.r.t. frame base pointer
    VarMode mode;   // local var or parameters
    DataType type;
};

struct SymbolTable {

    std::vector<Symbol> table;
    int frame_cnt;  // number of stuffs in current stack frame

    void set_frame_cnt(int size) { frame_cnt = size; }
    int push_stack(int size) { return frame_cnt += size; }
    void pop_stack(int size) { frame_cnt -= size; }

    void push(char *name, int scope, int size, VarMode mode, DataType type) {
        table.emplace_back((Symbol){name, scope, frame_cnt * WORD_SIZE, mode, type});
        frame_cnt += size;
    }
    Symbol* lookup(std::string name) {
        std::vector<Symbol>::reverse_iterator it;
        for (it = table.rbegin(); it != table.rend(); it++) {
            if (it->name == name) break;
        }
        if (it == table.rend()) return nullptr;
        else return &(*it);
    }
    void clear_to_scope(int scope) {
        while (table.size() && table.back().scope != scope) table.pop_back(), frame_cnt--;
    }
};

struct Scope {
    int scope;

    Scope():scope(0) {}

    int enter() { return ++scope; }
    int leave() { return --scope; }
    int get_scope() { return scope; }
};

// Visitor Declaration

struct Visitor {
    // output file discriptor
    std::ofstream AST, ASM;

    // ast related member
    int ast_indent;

    void inc_indent() { ast_indent++; }
    void dec_indent() { ast_indent--; }
    std::string indent() { return std::string(ast_indent*2, ' '); }

    // codegen related member
    SymbolTable symbol_table;
    Scope scope;
    void save_regs_on_stack(std::string whom, std::vector<std::string> &regs);
    void restore_regs_from_stack(std::string whom, std::vector<std::string> &regs);

    // constructor & visitor pattern
    Visitor():AST("ast.txt"), ASM("codegen.S"), ast_indent(0) {}

    void visit(Node &);
    void visit(TranslationUnit &);
    void visit(Declaration &);
    void visit(FuncDecl &);
    void visit(FuncDefn &);
    void visit(ScalarDecl &);
    void visit(Statement &);
    void visit(ExpressionStatement &);
    void visit(Expression &);
    void visit(UnaryExpression &);
    void visit(BinaryExpression &);
    void visit(CallExpression &);
    void visit(Identifier &);
    void visit(Literal &);
};

// AST Nodes Definition
    
enum ValueType { lvalue, rvalue, no_value };

std::string get_value_type_name(ValueType type);

struct CodegenDest {
    enum Dest { reg, mem, no_gen };
    Dest dest;
    ValueType value_type;
    std::string reg_name;
    int mem_offset;  // relative to stack pointer

    bool is_reg() { return dest == reg; }
    bool is_mem() { return dest == mem; }
    void set_reg(std::string _reg) { dest = reg, std::swap(_reg, reg_name); }
    void set_mem(int _offset) { dest = mem, mem_offset = _offset; }

    bool is_lvalue() { return value_type == lvalue; }
    bool is_rvalue() { return value_type == rvalue; }
    bool set_lvalue() { return value_type = lvalue; }
    bool set_rvalue() { return value_type = rvalue; }

    CodegenDest():dest(no_gen), value_type(no_value) {}
};

// The very base class

struct Node {
    char *token;    // NULL if non-terminal, assigned in lex
    Tag tag;        // NULL if terminal, assigned in yacc
    Tag hint;       // NULL if non-declaration
    Node* child[15];

    virtual void accept(Visitor &visitor) { visitor.visit(*this); }

    Node();
    Node(char*);    // construct from yytext
    virtual ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

// Recursive list to array

template<typename T>
struct NodeList : public Node {
    std::vector<T> arr;

    void push(T elem) { arr.emplace_back(elem); }
};

// Type class

struct Type : public Node {
    DataType type;

    Type(DataType _type):type(_type) {}
};

// Translation Unit Class

struct TranslationUnit : public Node {
    std::vector<Node*> decl_func_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    void add_extern_decl(Node *node) { decl_func_list.emplace_back(node); }

    ~TranslationUnit();
};

// Declaration base class

struct Declaration : public Node {
    Type *type; // scalar, atomic element of array, return type of function
    Expression *initializer;

    Declaration(char *txt):Node(txt), initializer(nullptr) {}
    virtual ~Declaration();

    void accept(Visitor &visitor) { visitor.visit(*this); }

    void set_type(Type* _type) { type = _type; }
    DataType get_data_type() { return type->type; }

    void set_initializer(Expression *init) { initializer = init; }
};

// Function Declaration

struct FuncDecl : public Declaration {
    std::vector<Declaration*> parameter_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDecl(char* str):Declaration(str) {}
    virtual ~FuncDecl();
};

struct FuncDefn : public FuncDecl {
    std::vector<Node*> func_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDefn(Type* _type, char *str, NodeList<Node*> *body = nullptr):FuncDecl(str) { 
        set_type(_type); 
        if (body) std::swap(body->arr, func_body);
    }
    ~FuncDefn();
};

// variable declaration

struct ScalarDecl : public Declaration {

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ScalarDecl(char* str):Declaration(str) {}
};

// Statement Base Class

struct Statement : public Node {

    void accept(Visitor &visitor) { visitor.visit(*this); }
    virtual ~Statement() {}
};

// Expression Statement

struct ExpressionStatement : public Statement {
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ExpressionStatement(Expression *_expr):expr(_expr) {}
    ~ExpressionStatement();
};

// Expression

struct Expression : public Node {
    CodegenDest dest;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    virtual ~Expression() {}

    void set_save_to_reg(std::string reg) { dest.set_reg(reg); }
    void set_save_to_mem(int offset) { dest.set_mem(offset); }
    void set_gen_lvalue() { dest.set_lvalue(); }
    void set_gen_rvalue() { dest.set_rvalue(); }
};

struct UnaryExpression : public Expression {
    Operator op;
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    UnaryExpression(Operator _op, Expression *_expr):op(_op), expr(_expr) {}
    virtual ~UnaryExpression();
};

struct BinaryExpression : public Expression { 
    Operator op;
    Expression *lhs, *rhs;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    BinaryExpression(Operator _op, Expression *_lhs, Expression *_rhs):op(_op), lhs(_lhs), rhs(_rhs) {}
    virtual ~BinaryExpression(); 
};

struct CallExpression : public UnaryExpression {
    std::vector<Expression*> argument_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    void set_argument_list(NodeList<Expression*> *nl) { std::swap(nl->arr, argument_list); }

    CallExpression(Expression *_expr, NodeList<Expression*> *nl = nullptr):UnaryExpression(op_call, _expr) {
        if (nl != nullptr) set_argument_list(nl); 
    }
    ~CallExpression();
};

struct Identifier : public Expression {
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Identifier(char *str);
};

struct Literal : public Expression { 
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Literal(char *str);
};


#endif // YYSTYPE_H_
