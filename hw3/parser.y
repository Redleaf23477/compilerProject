%{

#include <cstdio>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include <initializer_list>

#ifdef DEV
#define info(x) do { std::cerr << "[P:INFO] " << x << std::endl; } while (0)
#else
#define info(x) do {} while (0)
#endif

extern "C" {
    int yyerror(std::string s);
    int yyparse();
    int yylex();
}

// yylval Type Definition
#include "yystype.h"
#include "y.tab.h"

// usefull helper functions

Visitor visitor;

//////////////////////////////////////////////////
// AST Builder
//////////////////////////////////////////////////


using NodePtr = Node*;

void set(NodePtr &_dest, Tag t, std::initializer_list<NodePtr> child_list) {
    Node* &dest = _dest;
    dest = new Node;
    dest->tag = t;
    size_t i = 0;
    for (auto c : child_list) {
        dest->child[i++] = c;
    }
}

void set(NodePtr &dest, Tag t, NodePtr _1) { set(dest, t, { _1 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2) { set(dest, t, { _1, _2 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3) { set(dest, t, { _1, _2, _3 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4) { set(dest, t, { _1, _2, _3, _4 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5) { set(dest, t, { _1, _2, _3, _4, _5 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6) { set(dest, t, { _1, _2, _3, _4, _5, _6 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8, _9 } ); }
void set(NodePtr &dest, Tag t, NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9, NodePtr _10, NodePtr _11) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11 } ); }

void cleanup(std::initializer_list<NodePtr> child_list) {
    for (auto c : child_list) delete c;
}

void cleanup(NodePtr _1) { cleanup({ _1 } ); }
void cleanup(NodePtr _1, NodePtr _2) { cleanup({ _1, _2 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3) { cleanup({ _1, _2, _3 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4) { cleanup({ _1, _2, _3, _4 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5) { cleanup({ _1, _2, _3, _4, _5 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6) { cleanup({ _1, _2, _3, _4, _5, _6 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7) { cleanup({ _1, _2, _3, _4, _5, _6, _7 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8, _9 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9, NodePtr _10, NodePtr _11) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11 } ); }

void set_hint(NodePtr &dest, Tag hint) { dest->hint = hint; }
void set_hint(NodePtr &dest, NodePtr &child) { dest->hint = child->hint; }

inline void BEG(Tag t) { std::cout << "<" << tag2str(t) << ">"; }
inline void END(Tag t) { std::cout << "</" << tag2str(t) << ">"; }

//////////////////////////////////////////////////
// TODO
//////////////////////////////////////////////////

void print_and_bye(YYSTYPE);
void print_and_bye(Node*);

void codegen(Declaration*);

%}

/**********************************
 *
 * YYSTYPE Definition
 *
 **********************************/

%union {
    Node* node;
    Type* type;
    TranslationUnit* translation_unit;
    Declaration* decl;
    FuncDefn* func_defn;
    Statement* stmt;
    ExpressionStatement* expr_stmt;
    Expression* expr;
    NodeList<Expression*>* expr_list;
    NodeList<Node*>* node_list;
}


/**********************************
 *
 * Token Definitions
 *
 **********************************/

%token<node> IDENTIFIER
%token<node> LITERAL

// operators
%token<node> INC_OP DEC_OP LEQ_OP GEQ_OP EQ_OP NEQ_OP RSHIFT_OP LSHIFT_OP LAND_OP LOR_OP

// binary operator precedence
%right '='
%left LOR_OP
%left LAND_OP
%left '|'
%left '^'
%left '&'
%left EQ_OP NEQ_OP
%left '>' GEQ_OP '<' LEQ_OP
%left LSHIFT_OP RSHIFT_OP
%left '+' '-'
%left '*' '/' '%'

// type qualifiter
%token<node> CONST

// type specifier
%token<node> INT CHAR FLOAT DOUBLE VOID SIGNED UNSIGNED LONG SHORT

// more keywords
%token<node> IF ELSE SWITCH CASE DEFAULT WHILE DO FOR RETURN BREAK CONTINUE

// terminals represented by char
%type<node> '!'
%type<node> '%'
%type<node> '&'
%type<node> '('
%type<node> ')'
%type<node> '*'
%type<node> '+'
%type<node> ','
%type<node> '-'
%type<node> '/'
%type<node> ':'
%type<node> ';'
%type<node> '<'
%type<node> '='
%type<node> '>'
%type<node> '['
%type<node> ']'
%type<node> '^'
%type<node> '{'
%type<node> '|'
%type<node> '}'
%type<node> '~'

// non-terminals

%type<node> codegen
%type<translation_unit> translation_unit
%type<decl> external_declaration
%type<func_defn> function_definition
%type<stmt> statement
%type<expr_stmt> expression_statement
%type<node> selection_statement
%type<stmt> if_statement
%type<node> switch_statement
%type<node> switch_clause_list
%type<node> switch_clause
%type<node> statement_list
%type<stmt> iteration_statement
%type<node> while_statement
%type<stmt> do_while_statement
%type<node> for_statement
%type<node> emptiable_expression
%type<node> jump_statement
%type<node> break_statement
%type<node> continue_statement
%type<node> return_statement
%type<stmt> compound_statement
%type<node_list> statement_declaration_list
%type<expr> primary_expression
%type<expr> suffix_expression
%type<node> multidim_arr_list
%type<expr_list> argument_expression_list
%type<expr> prefix_expression
// %type<node> unary_operator
%type<node> type_name
%type<node> specifier_qualifier_list
%type<expr> multiplicative_expression
%type<expr> additive_expression
%type<expr> shift_expression
%type<expr> relational_expression
%type<expr> equality_expression
%type<expr> bitwise_and_expression
%type<expr> bitwise_xor_expression
%type<expr> bitwise_or_expression
%type<expr> logical_and_expression
%type<expr> logical_or_expression
%type<expr> assignment_expression
%type<expr> expression
%type<decl> declaration
%type<type> declaration_specifiers
%type<node> type_specifier
%type<node> type_qualifier
%type<decl> init_declarator_list
%type<decl> init_declarator
%type<decl> declarator
%type<decl> direct_declarator
%type<node> parameter_list
%type<node> parameter_declaration
%type<type> pointer
%type<expr> initializer
%type<node> initializer_list

%start codegen

%%

// entry point for parsing

codegen
    : translation_unit { codegen($1); cleanup($1); $$ = NULL; }

translation_unit
    : external_declaration                      { $$ = new TranslationUnit(); $$->add_extern_decl($1); }
    | translation_unit external_declaration     { $$ = $1; $$->add_extern_decl($2); }
    ;

external_declaration
      /* e.g. global variables, functions */
    : declaration
      /* e.g. int main() { ... } */
    | function_definition
    ;

/**********************************
 *
 * Function definition
 *
 **********************************/

function_definition
    : declaration_specifiers declarator compound_statement { $$ = new FuncDefn($1, $2->token, $3); }
    ;

/**********************************
 *
 * Statement
 *
 **********************************/

statement
    : expression_statement 
    | selection_statement
    | iteration_statement
    | jump_statement
    | compound_statement
    ;

expression_statement
    : expression ';' { $$ = new ExpressionStatement($1); cleanup($2); }
    ;

selection_statement
    : if_statement
    | switch_statement
    ;

if_statement
    : IF '(' expression ')' compound_statement { $$ = new IfStatement($3, $5); cleanup($1, $2, $4); }
    | IF '(' expression ')' compound_statement ELSE compound_statement {
        $$ = new IfStatement($3, $5, $7); cleanup($1, $2, $4, $6); }
    ;

switch_statement
    : SWITCH '(' expression ')' '{' '}' {
        set($$, STMT, $1, $2, $3, $4, $5, $6); }
    | SWITCH '(' expression ')' '{' switch_clause_list '}' {
        set($$, STMT, $1, $2, $3, $4, $5, $6, $7); }
    ;

switch_clause_list
    : switch_clause
    | switch_clause switch_clause_list      { set($$, NOTAG, $1, $2); }
    ;

switch_clause
    : CASE expression ':'                   { set($$, NOTAG, $1, $2, $3); }
    | CASE expression ':' statement_list    { set($$, NOTAG, $1, $2, $3, $4); }
    | DEFAULT ':'                           { set($$, NOTAG, $1, $2); }
    | DEFAULT ':' statement_list            { set($$, NOTAG, $1, $2, $3); }
    ;

statement_list
    : statement
    | statement statement_list  { set($$, NOTAG, $1, $2); }
    ;

iteration_statement
    : while_statement
    | do_while_statement
    | for_statement
    ;

while_statement
    : WHILE '(' expression ')' statement    { set($$, STMT, $1, $2, $3, $4, $5); }
    ;

do_while_statement
    : DO statement WHILE '(' expression ')' ';' { $$ = new DoStatement($5, $2); cleanup($1, $3, $4, $6, $7); }
    ;

for_statement
    : FOR '(' emptiable_expression ';' emptiable_expression ';' emptiable_expression ')' statement {
            set($$, STMT, $1, $2, $3, $4, $5, $6, $7, $8, $9); }
    ;

emptiable_expression
    : /* empty */   { $$ = new Node; }
    | expression
    ;

jump_statement
    : break_statement
    | continue_statement
    | return_statement
    ;

break_statement
    : BREAK ';'     { set($$, STMT, $1, $2); }
    ;

continue_statement
    : CONTINUE ';'  { set($$, STMT, $1, $2); }
    ;

return_statement
    : RETURN ';'                { set($$, STMT, $1, $2); }
    | RETURN expression ';'     { set($$, STMT, $1, $2, $3); }
    ;

compound_statement
    : '{' '}'                               { $$ = new CompoundStatement(new NodeList<Node*>); cleanup($1, $2); }
    | '{' statement_declaration_list '}'    { $$ = new CompoundStatement($2); cleanup($1, $2, $3); }
    ;

statement_declaration_list
    : statement                                 { $$ = new NodeList<Node*>; $$->push($1); }
    | statement_declaration_list statement      { $$ = $1; $$->push($2); }
    | declaration                               { $$ = new NodeList<Node*>; $$->push($1); }
    | statement_declaration_list declaration    { $$ = $1; $$->push($2); }
    ;

/**********************************
 *
 * Expression (Listed from high to low precedence)
 *
 **********************************/

// highest precedence, should not be separated
primary_expression
    : IDENTIFIER            { $$ = new Identifier($1->token); cleanup($1); }
    | LITERAL               { $$ = new Literal($1->token); cleanup($1); }
    | '(' expression ')'    { $$ = $2; cleanup($1, $3); }  // unhealthy though, should be Parentheses Expression
    ;

    /* Right precedence (Right to Left) */

suffix_expression
    : primary_expression
    | suffix_expression INC_OP                                  //{ set($$, EXPR, $1, $2); }
    | suffix_expression DEC_OP                                  //{ set($$, EXPR, $1, $2); }
    | suffix_expression '(' ')'                                 { $$ = new CallExpression($1); cleanup($2, $3); }
    | suffix_expression '(' argument_expression_list ')'        { $$ = new CallExpression($1, $3); cleanup($2, $3, $4); }
      /* array: hw spec differs from c / c++ spec */
    | IDENTIFIER '[' expression ']'                             { $$ = new ArraySubscriptExpression(new Identifier($1->token), $3); cleanup($1, $2, $4); }
    | IDENTIFIER '[' expression ']' multidim_arr_list           //{ set($$, EXPR, $1, $2, $3, $4, $5); }
    ;

multidim_arr_list
    : '[' expression ']'                        { set($$, NOTAG, $1, $2, $3); }
    | '[' expression ']' multidim_arr_list      { set($$, NOTAG, $1, $2, $3, $4); }
    ;

argument_expression_list
    : assignment_expression                                 { $$ = new NodeList<Expression*>; $$->push($1); }
    | argument_expression_list ',' assignment_expression    { $$ = $1; $$->push($3); cleanup($2); }
    ;

    /*
unary_operation_expression
    : suffix_expression
    | unary_operator unary_operation_expression { set($$, EXPR, $1, $2); }
    ;
*/

prefix_expression
    : suffix_expression 
    | '&' prefix_expression                 { $$ = new UnaryExpression(op_addr, $2); cleanup($1); }
    | '*' prefix_expression                 { $$ = new UnaryExpression(op_deref, $2); cleanup($1); } 
    | '-' prefix_expression                 { $$ = new UnaryExpression(op_neg, $2); cleanup($1); } 
    | INC_OP prefix_expression              //{ set($$, EXPR, $1, $2); }
    | DEC_OP prefix_expression              //{ set($$, EXPR, $1, $2); }
    | '(' type_name ')' prefix_expression   //{ set($$, EXPR, $1, $2, $3, $4); }
    ;

    /*
unary_operator
    : '&'
    | '*'
    | '!'
    | '~'
    | '+'
    | '-'
    ;
    */

type_name
    : specifier_qualifier_list
    | specifier_qualifier_list pointer      { set($$, NOTAG, $1, $2); }
    ;

    // TODO: wait until declaration
specifier_qualifier_list
    : type_qualifier
    | type_qualifier specifier_qualifier_list   { set($$, NOTAG, $1, $2); }
    | type_specifier
    | type_specifier specifier_qualifier_list   { set($$, NOTAG, $1, $2); }
    ;

    /* Left precedence (Left to Right) */

multiplicative_expression
    : prefix_expression
    | multiplicative_expression '*' prefix_expression       { $$ = new BinaryExpression(op_mul, $1, $3); cleanup($2); }
    | multiplicative_expression '/' prefix_expression       { $$ = new BinaryExpression(op_div, $1, $3); cleanup($2); }
    | multiplicative_expression '%' prefix_expression       { $$ = new BinaryExpression(op_mod, $1, $3); cleanup($2); }
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression     { $$ = new BinaryExpression(op_add, $1, $3); cleanup($2); }
    | additive_expression '-' multiplicative_expression     { $$ = new BinaryExpression(op_sub, $1, $3); cleanup($2); }
    ;

shift_expression
    : additive_expression
    | shift_expression LSHIFT_OP additive_expression //{ set($$, EXPR, $1, $2, $3); }
    | shift_expression RSHIFT_OP additive_expression //{ set($$, EXPR, $1, $2, $3); }
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression        { $$ = new BinaryExpression(op_lt, $1, $3); cleanup($2); }
    | relational_expression LEQ_OP shift_expression     //{ set($$, EXPR, $1, $2, $3); }
    | relational_expression '>' shift_expression        //{ set($$, EXPR, $1, $2, $3); }
    | relational_expression GEQ_OP shift_expression     //{ set($$, EXPR, $1, $2, $3); }
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression   { $$ = new BinaryExpression(op_eq, $1, $3); cleanup($2); }
    | equality_expression NEQ_OP relational_expression  //{ set($$, EXPR, $1, $2, $3); }
    ;

bitwise_and_expression
    : equality_expression
    | bitwise_and_expression '&' equality_expression //{ set($$, EXPR, $1, $2, $3); }
    ;

bitwise_xor_expression
    : bitwise_and_expression
    | bitwise_xor_expression '^' bitwise_and_expression //{ set($$, EXPR, $1, $2, $3); }
    ;

bitwise_or_expression
    : bitwise_xor_expression
    | bitwise_or_expression '|' bitwise_xor_expression //{ set($$, EXPR, $1, $2, $3); }
    ;

logical_and_expression
    : bitwise_or_expression
    | logical_and_expression LAND_OP bitwise_or_expression //{ set($$, EXPR, $1, $2, $3); }
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression LOR_OP logical_and_expression //{ set($$, EXPR, $1, $2, $3); }
    ;


    /* Right precedence (Right to Left) */

assignment_expression
    : logical_or_expression
    | logical_or_expression '=' assignment_expression   { $$ = new BinaryExpression(op_assign, $1, $3); cleanup($2); }
    ;

// lowest precedence, includes everything
// comma not supported
expression
    : assignment_expression
    ;

/**********************************
 *
 * Declarations: scalar, array, function
 *
 **********************************/

declaration
      /* (Type) (id/id=...) ; */
    : declaration_specifiers init_declarator_list ';' { $$ = $2; $$->set_type($1); cleanup($3); }
    ;

// grammar describing a type
// forget about type and const, supporting 64-bit int only
declaration_specifiers
      /* e.g. int */
    : type_specifier                        { $$ = new Type(T_INT); cleanup($1); }
      /* e.g. signed int */
    | type_specifier declaration_specifiers { $$ = new Type(T_INT); cleanup($1, $2); }
      /* i.e. const */
    | type_qualifier                        { $$ = new Type(T_INT); cleanup($1); }
      /* e.g. const int */
    | type_qualifier declaration_specifiers { $$ = new Type(T_INT); cleanup($1, $2); }
    ;

// terminals: fundamental types
type_specifier
    : INT 
    | CHAR
    | FLOAT
    | DOUBLE
    | VOID
    | SIGNED
    | UNSIGNED
    | LONG
    | SHORT
    ;

// terminals: type qualifiers
type_qualifier
    : CONST
    ;

// grammar describing a set of declaraed name (and possibly initialization)
init_declarator_list
      /* single declared instance */
    : init_declarator
      /* multiple declared instance */
    | init_declarator ',' init_declarator_list  // { set($$, NOTAG, $1, $2, $3), set_hint($$, $1); }
    ;

// declare without/with copy initialization
init_declarator
    : declarator
    | declarator '=' initializer   { $$ = $1; $$->set_initializer($3); cleanup($2); }
    ;

// declare without/with pointer
declarator
      /* non-pointer */
    : direct_declarator
      /* single level pointer */
    | pointer direct_declarator { $$ = $2; $$->set_type($1); }
    ;

// declare the name of variable
direct_declarator
    : IDENTIFIER                            { $$ = new ScalarDecl($1->token); cleanup($1); }
    | IDENTIFIER '(' ')'                    { $$ = new FuncDecl($1->token); cleanup($1, $2, $3); }
    | IDENTIFIER '(' parameter_list ')'     //{ set($$, NOTAG, $1, $2, $3, $4), set_hint($$, FDEC); }
      /* supports simple 1D array declation only, e.g. arr[3] */
    | direct_declarator '[' expression ']'  { $$ = new ArrayDecl($1->token, atoi($3->token)); cleanup($1, $2, $3, $4); }
    ;

// grammar of parameter list
parameter_list
    : parameter_declaration
    | parameter_declaration ',' parameter_list  { set($$, NOTAG, $1, $2, $3); }
    ;

parameter_declaration
      /* TYPE id */
    : declaration_specifiers declarator { set($$, NOTAG, $1, $2); }
    ;

// terminal: pointer (only supports single level pointer)
pointer
    : '*' { $$ = new Type(T_PTR); cleanup($1); }
    ;

initializer
    : expression 
    | '{' initializer_list '}'  // { set($$, NOTAG, $1, $2, $3); }
    ;

initializer_list
    : initializer
    | initializer ',' initializer_list  { set($$, NOTAG, $1, $2, $3); }
    ;

%%

int main(void) {
    // parse & codegen
    yyparse();
    return 0;
}

int yyerror(std::string s) {
    std::cerr << "[yyerror] " << s << std::endl;
    return 0;
}

void print_and_bye(YYSTYPE rt) { print_and_bye(rt.node); }

void print_and_bye(Node *rt) {
    if (rt == NULL) return;
    size_t n = sizeof(rt->child)/sizeof(Node*);

    if (rt->tag == NOTAG) {
        if (rt->token != NULL) {
            std::cout << rt->token;
        }
        for (size_t i = 0; i < n; i++) {
            if (rt->child[i] == NULL) break;
            print_and_bye(rt->child[i]);
            rt->child[i] = NULL;
        }
    } else {
        BEG(rt->tag);
        for (size_t i = 0; i < n; i++) {
            if (rt->child[i] == NULL) break;
            print_and_bye(rt->child[i]);
            rt->child[i] = NULL;
        }
        END(rt->tag);
    }
    // kill rt
    delete rt;
}

void codegen(TranslationUnit *unit) {
    unit->accept(visitor);
}
