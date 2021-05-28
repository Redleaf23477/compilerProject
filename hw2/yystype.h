#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <string>
#include <iostream>

#define YYSTYPE Node*

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

struct Node {
    char *token;    // NULL if non-terminal, assigned in lex
    Tag tag;        // NULL if terminal, assigned in yacc
    Node* child[10];

    Node();
    Node(char*);    // construct from yytext
    ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

#endif // YYSTYPE_H_
