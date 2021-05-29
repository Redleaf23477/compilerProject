#include <string>
#include <iostream>
#include <cstring>
#include <cassert>
#include "yystype.h"

std::string tag2str(Tag t) {
    switch (t) {
    case SDEC: return "scalar_decl";
    case ADEC: return "array_decl";
    case FDEC: return "func_decl";
    case FDEF: return "func_def";
    case EXPR: return "expr";
    case STMT: return "stmt";
    case NOTAG: return "X";
    default: 
        std::cerr << "unknown tag " << t << std::endl; 
        assert(false && "unreachable"); 
        return "unknown";
    }
}

Node::Node():token(NULL), tag(NOTAG), hint(NOTAG) {
    memset(child, 0, sizeof(child));
}

Node::Node(char *yytext):token(NULL), tag(NOTAG), hint(NOTAG) {
    int buffsz = strlen(yytext) + 1; // note the '\0'
    token = new char[buffsz];
    memcpy(token, yytext, buffsz);
    memset(child, 0, sizeof(child));
}

Node::~Node() {
    if (token != NULL) delete[] token;
}

std::ostream &operator << (std::ostream &out, Node &nd) {
    if (nd.token != NULL) {
        out << nd.token;
    }
    return out;
}

