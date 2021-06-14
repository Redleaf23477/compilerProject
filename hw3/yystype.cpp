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
    case op_lt: return "less than";
    case op_eq: return "equal to";
    case op_assign: return "assign";
    case op_addr: return "address of";
    case op_deref: return "de-reference";
    case op_neg: return "arithmetic negate";
    case op_subscript: return "subscript";
    }
    std::cerr << "unrecognized op code: " << static_cast<int>(op) << std::endl;
    assert (false && "invalid operator");
    return "unreachable";
}

std::string get_type_name(DataType type) {
    switch (type) {
    case T_INT: return "int";
    case T_PTR: return "int*";
    }
    std::cerr << "unrecognized data type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid data type");
    return "unreachable";
}

std::string get_value_type_name(ValueType type) {
    switch (type) {
    case lvalue: return "lvalue";
    case rvalue: return "rvalue";
    case no_value: return "no_value";
    }
    std::cerr << "unrecognized value type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid value type");
    return "unreachable";
}

void Visitor::save_regs_on_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " saves registers >>>" << std::endl;
    ASM << "  addi sp, sp, " << -WORD_SIZE * (int)regs.size() << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  sd " << reg << ", " << WORD_SIZE * i << "(sp)" << std::endl;
    }
    ASM << "  // <<<" << std::endl;
}

void Visitor::restore_regs_from_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " restores registers >>>" << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  ld " << reg << ", " << WORD_SIZE * i << "(sp)" << std::endl;
    }
    ASM << "  addi sp, sp, " << WORD_SIZE * (int)regs.size() << std::endl;
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
    if (initializer) delete initializer;
}

FuncDecl::~FuncDecl() {
    for (auto x : parameter_list) delete x;
}

FuncDefn::~FuncDefn() {
    delete func_body;
}

CompoundStatement::~CompoundStatement() {
    for (auto x : stmt_decl_list) delete x;
}

IfStatement::~IfStatement() {
    delete cond;
    delete if_body;
    if (else_body) delete else_body;
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

ArraySubscriptExpression::~ArraySubscriptExpression() {
    delete subscript;
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
    AST << "[type: " << get_type_name(decl.get_data_type()) << "]";
    AST << "[declaration name: " << decl.token << "]";
    AST << std::endl;
}

void Visitor::visit(FuncDecl &decl) {
    AST << indent() << "<Function Declaration>";
    AST << "[return: " << get_type_name(decl.get_data_type()) << "]";
    AST << "[function name: " << decl.token << "]";
    AST << "[parameters: ]";
    AST << std::endl;
}

std::vector<std::string> callee_preserved_registers {
    "sp", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11" };

void Visitor::visit(FuncDefn &defn) {
    AST << indent() << "<Function Definition>";
    AST << "[return: " << get_type_name(defn.get_data_type()) << "]";
    AST << "[function name: " << defn.token << "]";
    AST << "[parameters: ]";
    AST << std::endl;

    // function header in asm
    ASM << ".global " << defn.token << std::endl;
    ASM << defn.token << ":" << std::endl;

    // store callee preserved registers
    save_regs_on_stack("callee", callee_preserved_registers);
    symbol_table.push_stack(callee_preserved_registers.size());
    // set new frame
    ASM << "  addi s0, sp, 104" << std::endl;


    inc_indent();
    defn.func_body->accept(*this);
    dec_indent();

    // restore callee preserved registers
    ASM << "  // release local variables" << std::endl;
    ASM << "  addi sp, sp, " << WORD_SIZE * (symbol_table.frame_cnt - callee_preserved_registers.size()) << std::endl;
    restore_regs_from_stack("callee", callee_preserved_registers);
    symbol_table.pop_stack(callee_preserved_registers.size());

    // return
    ASM << "  jalr x0, 0(ra)" << std::endl;
}

void Visitor::visit(ScalarDecl &decl) {
    // push into symbol table
    symbol_table.push(decl.token, scope.get_scope(), 1, M_LOCAL, decl.get_data_type());
    Symbol *sym = symbol_table.lookup(decl.token);
    assert(sym != nullptr && "symbol not found");

    AST << indent() << "<Scalar Declaration>";
    AST << "[variable name = " << decl.token << "]";
    AST << "[type = " << get_type_name(decl.get_data_type()) << "]";
    AST << "{scope = " << sym->scope << "}";
    AST << "{offset = " << sym->offset << "}";
    AST << std::endl;

    // codegen: simply grow the stack
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // initializer
    inc_indent();
    if (decl.initializer != nullptr) {
        decl.initializer->set_save_to_mem(sym->offset);
        decl.initializer->set_gen_rvalue();
        decl.initializer->accept(*this); 
    }
    dec_indent();
}

void Visitor::visit(ArrayDecl &decl) {
    // push into symbol table
    symbol_table.push(decl.token, scope.get_scope(), decl.array_size, M_LOCAL, decl.get_data_type());
    Symbol *sym = symbol_table.lookup(decl.token);
    assert(sym != nullptr && "symbol not found");

    AST << indent() << "<Array Declaration>";
    AST << "[variable name = " << decl.token << "]";
    AST << "[element type = " << get_type_name(decl.get_data_type()) << "]";
    AST << "[array size = " << decl.array_size << "]";
    AST << "{scope = " << sym->scope << "}";
    AST << "{offset = " << sym->offset << "}";
    AST << std::endl;

    // codegen: simply grow the stack
    ASM << "  addi sp, sp, " << -decl.array_size * WORD_SIZE << std::endl;

    // note: there is no array initializer in the test cases, forget about them
}

void Visitor::visit(Statement &stmt) {
    AST << indent() << "<Statement>" << std::endl;
}

void Visitor::visit(CompoundStatement &stmt) {
    AST << indent() << "<Compund Statement>" << std::endl;

    // nothing to codegen :)

    inc_indent();
    scope.enter();
    for (auto c : stmt.stmt_decl_list) c->accept(*this);
    dec_indent();
    scope.leave();
}

void Visitor::visit(IfStatement &if_stmt) {
    AST << indent() << "<If Statement>" << std::endl;

    ASM << "  // IfStatement >>>" << std::endl;

    new_if_label_set();

    // allocate temp for cond
    int cond_offset = symbol_table.push_stack(1) * WORD_SIZE;
    if_stmt.cond->set_save_to_mem(cond_offset);
    if_stmt.cond->set_gen_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // fall-through implementation

    // traverse child (cond)
    inc_indent();
    if_stmt.cond->accept(*this);

    ASM << "  ld t0, " << cond_offset << "(fp)" << std::endl; // t0 = cond
    ASM << "  beqz t0, " << label_else() << std::endl;  // !cond then goto else

    // traverse child (if_body)
    if_stmt.if_body->accept(*this);
    if (if_stmt.else_body) ASM << "  j " << label_end_if() << std::endl;

    ASM << label_else() << ":" << std::endl;

    // traverse child (else_body)
    if (if_stmt.else_body) if_stmt.else_body->accept(*this);

    ASM << label_end_if() << ":" << std::endl;

    // traverse end
    dec_indent();

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< IfStatement" << std::endl;
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
    AST << "[value type = " << get_value_type_name(expr.dest.value_type) << "]";
    AST << std::endl;

    ASM << "  // UnaryExpression >>>" << std::endl;

    // allocate temp
    int tmp_offset = symbol_table.push_stack(1) * WORD_SIZE;
    expr.expr->set_save_to_mem(tmp_offset);
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // set value type
    if (expr.op == op_addr) {
        expr.expr->set_gen_lvalue();
    } else {
        expr.expr->set_gen_rvalue();
    }

    // traverse child
    inc_indent();
    expr.expr->accept(*this);
    dec_indent();

    // codegen
    // load result into register
    ASM << "  ld t0, " << tmp_offset << "(fp)" << std::endl;

    // operator codegen
    switch (expr.op) {
    case op_addr: break;  // t0 is already address of the variable
    case op_deref: break;  // t0 is now the address of the variable that is pointed to
    case op_neg: ASM << "  sub t0, x0, t0" << std::endl; break;
    default: 
        std::cerr << "unsupported operator " << get_op_name(expr.op) << std::endl; 
        assert(false && "unsupported unary operator codegen");
    }

    // Note that op_addr, op_neg must return rvalue
    // Cast op_deref if required
    if (expr.op == op_deref && expr.dest.is_rvalue()) {
        ASM << "  ld t0, 0(t0)" << std::endl;
    }

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <<< UnaryExpression" << std::endl;
}

void Visitor::visit(BinaryExpression &expr) {
    AST << indent() << "<Binary Expression>";
    AST << "[op = " << get_op_name(expr.op) << "]";
    AST << "[value type = " << get_value_type_name(expr.dest.value_type) << "]";
    AST << std::endl;

    ASM << "  // Binary Expression >>>" << std::endl;

    // allocate temp
    int lhs_offset = symbol_table.push_stack(1) * WORD_SIZE;
    int rhs_offset = symbol_table.push_stack(1) * WORD_SIZE; 
    expr.lhs->set_save_to_mem(lhs_offset);
    expr.rhs->set_save_to_mem(rhs_offset);
    ASM << "  addi sp, sp, " << -2*WORD_SIZE << std::endl;

    if (expr.op == op_assign) {
        expr.lhs->set_gen_lvalue();
        expr.rhs->set_gen_rvalue();

        // traverse to child
        inc_indent();
        expr.lhs->accept(*this);
        expr.rhs->accept(*this);
        dec_indent();

        // load lhs, rhs result into registers
        ASM << "  ld t0, " << lhs_offset << "(fp)" << std::endl;
        ASM << "  ld t1, " << rhs_offset << "(fp)" << std::endl;

        // operator codegen
        ASM << "  sd t1, 0(t0)" << std::endl;

        // cast to rvalue if needed
        if (expr.dest.is_rvalue()) ASM << "  addi t0, t1, 0" << std::endl;
    } else {  // arithmetic
        expr.lhs->set_gen_rvalue();
        expr.rhs->set_gen_rvalue();

        // traverse to child
        inc_indent();
        expr.lhs->accept(*this);
        expr.rhs->accept(*this);
        dec_indent();

        // load lhs, rhs result into registers
        ASM << "  ld t0, " << lhs_offset << "(fp)" << std::endl;
        ASM << "  ld t1, " << rhs_offset << "(fp)" << std::endl;

        // operator codegen
        switch (expr.op) {
        case op_add: ASM << "  add t0, t0, t1" << std::endl; break;
        case op_sub: ASM << "  sub t0, t0, t1" << std::endl; break;
        case op_mul: ASM << "  mul t0, t0, t1" << std::endl; break;
        case op_div: ASM << "  div t0, t0, t1" << std::endl; break;
        case op_lt: ASM << "  slt t0, t0, t1" << std::endl; break;
        case op_eq: 
            ASM << "  sub t0, t0, t1" << std::endl;
            ASM << "  seqz t0, t0" << std::endl;
            break;
        default: 
            std::cerr << "unsupported operator " << get_op_name(expr.op) << std::endl; 
            assert(false && "unsupported binary operator codegen");
        }

        // Note: arithmetic operator should always be rvalue
    }

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // release temp
    symbol_table.pop_stack(2);
    ASM << "  addi sp, sp, " << 2*WORD_SIZE << std::endl;

    ASM << "  // <<< BinaryExpression" << std::endl;
}

std::vector<std::string> arg_regs { "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };
std::vector<std::string> caller_preserved_registers { 
    "ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };

void Visitor::visit(CallExpression &expr) {
    AST << indent() << "<Call Expression>" << std::endl;

    ASM << "  // CallExpression >>>" << std::endl;

    inc_indent();
    for (size_t i = 0; i < expr.argument_list.size(); i++) {
        auto &arg = expr.argument_list[i];
        arg->set_save_to_reg(arg_regs[i]); 
        arg->set_gen_rvalue(); 
        arg->accept(*this);
    }
    expr.expr->accept(*this);

    // store caller preserved registers
    save_regs_on_stack("caller", caller_preserved_registers);

    ASM << "  jal ra, " << expr.expr->token << std::endl;

    // restore caller preserved registers
    restore_regs_from_stack("caller", caller_preserved_registers);

    ASM << "  // <<< CallExpression" << std::endl;

    dec_indent();
}

void Visitor::visit(ArraySubscriptExpression &expr) {
    AST << indent() << "<Array Subscript Expression>" << std::endl;

    ASM << "  // ArraySubscriptExpression >>>" << std::endl;

    // allocate temp
    int expr_offset = symbol_table.push_stack(1) * WORD_SIZE;
    int subscript_offset = symbol_table.push_stack(1) * WORD_SIZE; 
    expr.expr->set_save_to_mem(expr_offset);
    expr.subscript->set_save_to_mem(subscript_offset);
    ASM << "  addi sp, sp, " << -2*WORD_SIZE << std::endl;

    expr.expr->set_gen_lvalue();
    expr.subscript->set_gen_rvalue();

    // traverse to child
    inc_indent();
    expr.expr->accept(*this);
    expr.subscript->accept(*this);
    dec_indent();

    // load into register
    ASM << "  ld t0, " << expr_offset << "(fp)" << std::endl;
    ASM << "  ld t1, " << subscript_offset << "(fp)" << std::endl;

    // operator codegen
    ASM << "  slli t1, t1, " << LG_WORD_SIZE << std::endl;
    ASM << "  add t0, t0, t1" << std::endl;

    // cast to rvalue if needed
    if (expr.dest.is_rvalue()) ASM << "  ld t0, 0(t0)" << std::endl;

    // return value
    if (expr.dest.is_reg()) {
        ASM << "  addi " << expr.dest.reg_name << ", t0, 0" << std::endl;
    } else if (expr.dest.is_mem()) {
        ASM << "  sd t0, " << expr.dest.mem_offset << "(fp)" << std::endl;
    }

    // release temp
    symbol_table.pop_stack(2);
    ASM << "  addi sp, sp, " << 2*WORD_SIZE << std::endl;

    ASM << "  // <<< ArraySubscriptExpression" << std::endl;
}

void Visitor::visit(Identifier &id) {
    AST << indent() << "<Identifier>";
    AST << "[identifier = " << id.token << "]";
    AST << std::endl;

    Symbol *sym = symbol_table.lookup(id.token);
    if (sym == nullptr) return;  // probably it is id of function

    int offset = sym->offset;

    ASM << "  // Identifier >>>" << std::endl;

    // lvalue or rvalue
    if (id.dest.is_lvalue()) {
        ASM << "  addi t0, fp, 0" << std::endl;
        ASM << "  addi t0, t0, " << offset << std::endl;
    } else if (id.dest.is_rvalue()) {
        ASM << "  ld t0, " << offset << "(fp)" << std::endl;
    }

    // register or mem
    if (id.dest.is_reg()) {
        ASM << "  addi " << id.dest.reg_name << ", t0, 0" << std::endl;
    } else if (id.dest.is_mem()) {
        ASM << "  sd t0, " << id.dest.mem_offset << "(fp)" << std::endl;
    }

    ASM << "  // <<< Identifier" << std::endl;
}

void Visitor::visit(Literal &lit) {
    AST << indent() << "<Literal>";
    AST << "[value = " << lit.token << "]";
    AST << std::endl;

    ASM << "  // Literal >>>" << std::endl;

    // always rvalue
    if (lit.dest.is_reg()) {
        ASM << "  li " << lit.dest.reg_name << ", " << lit.token << std::endl;
    } else if (lit.dest.is_mem()) {
        ASM << "  li t0, " << lit.token << std::endl;
        ASM << "  sd t0, " << lit.dest.mem_offset << "(fp)" << std::endl;
    }

    ASM << "  // <<< Literal" << std::endl;
}

