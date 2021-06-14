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

std::string get_op_name(Operator op) {
    switch (op) {
    case op_call: return "function call";
    case op_add: return "add";
    case op_sub: return "sub";
    case op_mul: return "mul";
    case op_div: return "div";
    }
    std::cerr << "unrecognized op code: " << static_cast<int>(op) << std::endl;
    assert (false && "invalid operator");
    return "unreachable";
}

void Visitor::save_regs_on_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " saves registers >>>" << std::endl;
    ASM << "  addi sp, sp, " << -8 * (int)regs.size() << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  sd " << reg << ", " << 8*i << "(sp)" << std::endl;
    }
    ASM << "  // <<<" << std::endl;
}

void Visitor::restore_regs_from_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " restores registers >>>" << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  ld " << reg << ", " << 8*i << "(sp)" << std::endl;
    }
    ASM << "  addi sp, sp, " << 8 * (int)regs.size() << std::endl;
    ASM << "  // <<<" << std::endl;
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

BinaryExpression::~BinaryExpression() {
    delete lhs;
    delete rhs;
}

CallExpression::~CallExpression() {
    for (auto x : argument_list) delete x;
}

Identifier::Identifier(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}

Literal::Literal(char *str) {
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

std::vector<std::string> callee_preserved_registers {
    "sp", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11" };

void Visitor::visit(FuncDefn &defn) {
    AST << indent() << "<Function Definition>";
    if (defn.get_data_type() == int_type) AST << "[return: int]";
    AST << "[function name: " << defn.token << "]";
    AST << "[parameters: ]";
    AST << std::endl;

    // function header in asm
    ASM << ".global " << defn.token << std::endl;
    ASM << defn.token << ":" << std::endl;

    // store callee preserved registers
    save_regs_on_stack("callee", callee_preserved_registers);
    // set new frame
    ASM << "  addi s0, sp, 104" << std::endl;

    inc_indent();
    scope.enter();
    for (auto c : defn.func_body) c->accept(*this);
    dec_indent();
    scope.leave();

    // restore callee preserved registers
    restore_regs_from_stack("callee", callee_preserved_registers);

    // return
    ASM << "  jalr x0, 0(ra)" << std::endl;
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
    AST << "[op = " << get_op_name(expr.op) << "]";
    AST << std::endl;

    inc_indent();
    expr.expr->accept(*this);
    dec_indent();
}

void Visitor::visit(BinaryExpression &expr) {
    AST << indent() << "<Binary Expression>";
    AST << "[op = " << get_op_name(expr.op) << "]";
    AST << std::endl;

    inc_indent();
    expr.lhs->accept(*this);
    expr.rhs->accept(*this);
    dec_indent();
}

std::vector<std::string> arg_regs { "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };
std::vector<std::string> caller_preserved_registers { 
    "ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };

void Visitor::visit(CallExpression &expr) {
    AST << indent() << "<Call Expression>" << std::endl;

    inc_indent();
    for (size_t i = 0; i < expr.argument_list.size(); i++) {
        auto &arg = expr.argument_list[i];
        arg->set_save_to_reg(arg_regs[i]);
        arg->accept(*this);
    }
    expr.expr->accept(*this);

    // store caller preserved registers
    save_regs_on_stack("caller", caller_preserved_registers);

    ASM << "  jal ra, " << expr.expr->token << std::endl;

    // restore caller preserved registers
    restore_regs_from_stack("caller", caller_preserved_registers);

    dec_indent();
}

void Visitor::visit(Identifier &id) {
    AST << indent() << "<Identifier>";
    AST << "[identifier = " << id.token << "]";
    AST << std::endl;
}

void Visitor::visit(Literal &lit) {
    AST << indent() << "<Literal>";
    AST << "[value = " << lit.token << "]";
    AST << std::endl;

    if (lit.dest.is_reg()) {
        ASM << "  li " << lit.dest.reg_name << ", " << lit.token << std::endl;
    } else {
        assert(false && "function unsupported yet");
    }
}

