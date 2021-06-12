#include <string>
#include <iostream>
#include <cstring>
#include <cassert>
#include "yystype.h"

// utils

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

// Node Class

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

// Vistor

void Visitor::visit(Node &node) {
    std::cerr << "visiting Node" << std::endl;
}

void Visitor::visit(TranslationUnit &unit) {
    std::cerr << "visiting TranslationUnit" << std::endl;
    for (auto x : unit.decl_func_list) {
        x->accept(*this);
    }
}

void Visitor::visit(Declaration &decl) {
    std::cerr << "visiting declaration" << std::endl;
    if (decl.get_data_type() == int_type) std::cerr << "[type: int]";
    std::cerr << "[declaration name: " << decl.token << "]";
    std::cerr << std::endl;
}

void Visitor::visit(FuncDecl &decl) {
    std::cerr << "visiting function declaration" << std::endl;
    if (decl.get_data_type() == int_type) std::cerr << "[return: int]";
    std::cerr << "[function name: " << decl.token << "]";
    std::cerr << "[parameters: ]";
    std::cerr << std::endl;
}

