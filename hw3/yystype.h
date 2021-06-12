#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <string>
#include <vector>
#include <iostream>
#include <fstream>

// #define YYSTYPE Node*

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

// AST Nodes Declaration

struct Node;
struct Type;
struct Declaration;
struct FuncDecl;
struct FuncDefn;
struct TranslationUnit;
struct Statement;
struct ExpressionStatement;
struct Expression;
struct UnaryExpression;
struct CallExpression;
struct Identifier;

template<typename T = Node*> struct NodeList;

// Visitor Declaration

struct Visitor {
    std::ofstream AST;
    int ast_indent;

    Visitor():AST("ast.txt"), ast_indent(0) {}

    void inc_indent() { ast_indent++; }
    void dec_indent() { ast_indent--; }
    std::string indent() { return std::string(ast_indent*2, ' '); }

    void visit(Node &);
    void visit(TranslationUnit &);
    void visit(Declaration &);
    void visit(FuncDecl &);
    void visit(FuncDefn &);
    void visit(Statement &);
    void visit(ExpressionStatement &);
    void visit(Expression &);
    void visit(UnaryExpression &);
    void visit(CallExpression &);
    void visit(Identifier &);
};

// AST Nodes Definition


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

enum DataType {
    int_type
};

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

    Declaration(char *txt):Node(txt){}
    virtual ~Declaration();

    void accept(Visitor &visitor) { visitor.visit(*this); }

    void set_type(Type* _type) { type = _type; }
    DataType get_data_type() { return type->type; }
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

    // TODO: support more than one stmt
    FuncDefn(Type* _type, char *str, NodeList<Node*> *body = nullptr):FuncDecl(str) { 
        set_type(_type); 
        if (body) std::swap(body->arr, func_body);
    }
    ~FuncDefn();
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

    void accept(Visitor &visitor) { visitor.visit(*this); }
    virtual ~Expression() {}
};

struct UnaryExpression : public Expression {
    int op;
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    UnaryExpression(int _op, Expression *_expr):op(_op), expr(_expr) {}
    virtual ~UnaryExpression();
};

struct CallExpression : public UnaryExpression {
    std::vector<Expression*> argument_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    void set_argument_list(NodeList<Expression*> *nl) { std::swap(nl->arr, argument_list); }

    CallExpression(Expression *_expr, NodeList<Expression*> *nl = nullptr):UnaryExpression(7122, _expr) {
        if (nl != nullptr) set_argument_list(nl); 
    }
    ~CallExpression();
};

struct Identifier : public Expression {
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Identifier(char *str);
};


#endif // YYSTYPE_H_