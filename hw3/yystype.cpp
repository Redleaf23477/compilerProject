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

TranslationUnit::~TranslationUnit() {
    for (auto x : decl_func_list) delete x;
}

Declaration::~Declaration() {
    delete type;
}

FuncDecl::~FuncDecl() {
    for (auto x : parameter_list) delete x;
}

FuncDefn::~FuncDefn() {
    for (auto x : func_body) delete x;
}

ExpressionStatement::~ExpressionStatement() {
    delete expr;
}

UnaryExpression::~UnaryExpression() {
    delete expr;
}

CallExpression::~CallExpression() {
    for (auto x : argument_list) delete x;
}

Identifier::Identifier(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}

// Vistor (AST)

void Visitor::visit(Node &node) {
    AST << indent() << "<Node>" << std::endl;
}

void Visitor::visit(TranslationUnit &unit) {
    AST << indent() << "<Translation Unit>" << std::endl;

    inc_indent();
    for (auto x : unit.decl_func_list) {
        x->accept(*this);
    }
    dec_indent();
}

void Visitor::visit(Declaration &decl) {
    AST << indent() << "<Declaration>";
    if (decl.get_data_type() == int_type) AST << "[type: int]";
    AST << "[declaration name: " << decl.token << "]";
    AST << std::endl;
}

void Visitor::visit(FuncDecl &decl) {
    AST << indent() << "<Function Declaration>";
    if (decl.get_data_type() == int_type) AST << "[return: int]";
    AST << "[function name: " << decl.token << "]";
    AST << "[parameters: ]";
    AST << std::endl;
}

void Visitor::visit(FuncDefn &defn) {
    AST << indent() << "<Function Definition>";
    if (defn.get_data_type() == int_type) AST << "[return: int]";
    AST << "[function name: " << defn.token << "]";
    AST << "[parameters: ]";
    AST << std::endl;

    inc_indent(), scope.enter();
    for (auto c : defn.func_body) c->accept(*this);
    dec_indent(), scope.leave();
}

void Visitor::visit(Statement &stmt) {
    AST << indent() << "<Statement>" << std::endl;
}

void Visitor::visit(ExpressionStatement &expr_stmt) {
    AST << indent() << "<Expression Statement>" << std::endl;

    inc_indent();
    expr_stmt.expr->accept(*this);
    dec_indent();
}

void Visitor::visit(Expression &expr) {
    AST << indent() << "<Expression>" << std::endl;
}

void Visitor::visit(UnaryExpression &expr) {
    AST << indent() << "<Unary Expression>";
    AST << "[op = " << expr.op << "]";
    AST << std::endl;

    inc_indent();
    expr.expr->accept(*this);
    dec_indent();
}

void Visitor::visit(CallExpression &expr) {
    AST << indent() << "<Call Expression>" << std::endl;

    inc_indent();
    expr.expr->accept(*this);
    for (auto a : expr.argument_list) a->accept(*this);
    dec_indent();
}

void Visitor::visit(Identifier &id) {
    AST << indent() << "<Identifier>";
    AST << "[identifier = " << id.token << "]";
    AST << std::endl;
}
