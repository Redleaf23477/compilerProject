#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <string>
#include <vector>
#include <iostream>

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
struct TranslationUnit;

// Visitor Declaration

struct Visitor {
    void visit(Node &);
    void visit(TranslationUnit &);
    void visit(Declaration &);
    void visit(FuncDecl &);
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
    ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

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

    void add_declaration(Declaration *decl) { decl_func_list.emplace_back(decl); }
    // void add_function_definition();
};

// Declaration base class

struct Declaration : public Node {
    Type *type; // scalar, atomic element of array, return type of function

    Declaration(char *txt):Node(txt){}

    void accept(Visitor &visitor) { visitor.visit(*this); }

    void set_type(Type* _type) { type = _type; }
    DataType get_data_type() { return type->type; }
};

// Function Declaration

struct FuncDecl : public Declaration {
    std::vector<Declaration> parameter_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDecl(char *str):Declaration(str) {}
};

#endif // YYSTYPE_H_
