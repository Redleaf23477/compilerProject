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
struct N_List;

// Visitor Declaration

struct Visitor {
};

// AST Nodes Definition


// Base Class

struct Node {
    char *token;    // NULL if non-terminal, assigned in lex
    Tag tag;        // NULL if terminal, assigned in yacc
    Tag hint;       // NULL if non-declaration
    Node* child[15];

    Node();
    Node(char*);    // construct from yytext
    ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

#endif // YYSTYPE_H_
