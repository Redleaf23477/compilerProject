%{

#include <cstdio>
#include <cassert>
#include <iostream>
#include <string>
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

// usefull helper functions

void set(YYSTYPE &dest, Tag t, std::initializer_list<YYSTYPE> child_list) {
    dest = new Node;
    dest->tag = t;
    size_t i = 0;
    for (auto c : child_list) {
        dest->child[i++] = c;
    }
#ifdef DEV
    for (size_t j = 0; j < i; j++) {
        if (dest->child[j] == NULL) continue;
        info("c[" << j << "]=(" << (dest->child[j]->token? dest->child[j]->token : "X") << "," << tag2str(dest->child[j]->tag) << ")");
    }
#endif
}

void set(YYSTYPE &dest, Tag t, YYSTYPE _1) { set(dest, t, { _1 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2) { set(dest, t, { _1, _2 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3) { set(dest, t, { _1, _2, _3 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4) { set(dest, t, { _1, _2, _3, _4 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5) { set(dest, t, { _1, _2, _3, _4, _5 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5, YYSTYPE _6) { set(dest, t, { _1, _2, _3, _4, _5, _6 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5, YYSTYPE _6, YYSTYPE _7) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5, YYSTYPE _6, YYSTYPE _7, YYSTYPE _8) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5, YYSTYPE _6, YYSTYPE _7, YYSTYPE _8, YYSTYPE _9) { set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8, _9 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, 
            YYSTYPE _5, YYSTYPE _6, YYSTYPE _7, YYSTYPE _8, YYSTYPE _9, YYSTYPE _10, YYSTYPE _11) { 
            set(dest, t, { _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11 } ); }

void set_hint(YYSTYPE &dest, Tag hint) { dest->hint = hint; }
void set_hint(YYSTYPE &dest, YYSTYPE &child) { dest->hint = child->hint; }

inline void BEG(Tag t) { std::cout << "<" << tag2str(t) << ">"; }
inline void END(Tag t) { std::cout << "</" << tag2str(t) << ">"; }

void print_and_bye(YYSTYPE);

%}

/**********************************
 *
 * Token Definitions
 *
 **********************************/

%token IDENTIFIER
%token LITERAL

// operators
%token INC_OP DEC_OP LEQ_OP GEQ_OP EQ_OP NEQ_OP RSHIFT_OP LSHIFT_OP LAND_OP LOR_OP

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
%token CONST

// type specifier
%token INT CHAR FLOAT DOUBLE VOID SIGNED UNSIGNED LONG SHORT

// more keywords
%token IF ELSE SWITCH CASE DEFAULT WHILE DO FOR RETURN BREAK CONTINUE

%start translation_unit

%%

// entry point for parsing
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
      /* e.g. global variables, functions */
    : declaration           { print_and_bye($1); $$ = NULL; }
      /* e.g. int main() { ... } */
    | function_definition   { print_and_bye($1); $$ = NULL; } 
    ;

/**********************************
 *
 * Function definition
 *
 **********************************/

function_definition
    : declaration_specifiers declarator '{' '}'                             { set($$, FDEF, $1, $2, $3, $4); }
    | declaration_specifiers declarator '{' statement_declaration_list '}'  { set($$, FDEF, $1, $2, $3, $4, $5); }
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
    : expression ';' { set($$, STMT, $1, $2); }
    ;

selection_statement
    : if_statement
    | switch_statement
    ;

if_statement
    : IF '(' expression ')' '{' emptiable_statement_declaration_list '}' { 
        set($$, STMT, $1, $2, $3, $4, $5, $6, $7); }
    | IF '(' expression ')' '{' emptiable_statement_declaration_list '}' ELSE '{' emptiable_statement_declaration_list '}' {
        set($$, STMT, $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11); }
    ;

emptiable_statement_declaration_list
    : /* empty */                   { $$ = new Node; }
    | statement_declaration_list
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
    : DO statement WHILE '(' expression ')' ';' { 
            set($$, STMT, $1, $2, $3, $4, $5, $6, $7); }
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
    : '{' '}'                               { set($$, STMT, $1, $2); }
    | '{' statement_declaration_list '}'    { set($$, STMT, $1, $2, $3); }
    ;

statement_declaration_list
    : statement
    | statement statement_declaration_list      { set($$, NOTAG, $1, $2); }
    | declaration
    | declaration statement_declaration_list    { set($$, NOTAG, $1, $2); }
    ;

/**********************************
 *
 * Expression (Listed from high to low precedence)
 *
 **********************************/

// highest precedence, should not be separated
primary_expression
    : IDENTIFIER            { set($$, EXPR, $1); }
    | LITERAL               { set($$, EXPR, $1); }
    | '(' expression ')'    { set($$, EXPR, $1, $2, $3); }
    ;

    /* Right precedence (Right to Left) */

suffix_expression
    : primary_expression
    | suffix_expression INC_OP                                  { set($$, EXPR, $1, $2); }
    | suffix_expression DEC_OP                                  { set($$, EXPR, $1, $2); }
    | suffix_expression '(' ')'                                 { set($$, EXPR, $1, $2, $3); }
    | suffix_expression '(' argument_expression_list ')'        { set($$, EXPR, $1, $2, $3, $4); }
      /* array: hw spec differs from c / c++ spec */
    | IDENTIFIER '[' expression ']'                             { set($$, EXPR, $1, $2, $3, $4); }
    | IDENTIFIER '[' expression ']' multidim_arr_list           { set($$, EXPR, $1, $2, $3, $4, $5); }
    ;

multidim_arr_list
    : '[' expression ']'                        { set($$, NOTAG, $1, $2, $3); }
    | '[' expression ']' multidim_arr_list      { set($$, NOTAG, $1, $2, $3, $4); }
    ;

argument_expression_list
    : assignment_expression 
    | assignment_expression ',' argument_expression_list { set($$, NOTAG, $1, $2, $3); }
    ;

    /*
unary_operation_expression
    : suffix_expression
    | unary_operator unary_operation_expression { set($$, EXPR, $1, $2); }
    ;
*/

prefix_expression
    : suffix_expression 
    | unary_operator prefix_expression      { set($$, EXPR, $1, $2); }
    | INC_OP prefix_expression              { set($$, EXPR, $1, $2); }
    | DEC_OP prefix_expression              { set($$, EXPR, $1, $2); }
    | '(' type_name ')' prefix_expression   { set($$, EXPR, $1, $2, $3, $4); }
    ;

unary_operator
    : '&'
    | '*'
    | '!'
    | '~'
    | '+'
    | '-'
    ;

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
    | multiplicative_expression '*' prefix_expression { set($$, EXPR, $1, $2, $3); }
    | multiplicative_expression '/' prefix_expression { set($$, EXPR, $1, $2, $3); }
    | multiplicative_expression '%' prefix_expression { set($$, EXPR, $1, $2, $3); }
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression { set($$, EXPR, $1, $2, $3); }
    | additive_expression '-' multiplicative_expression { set($$, EXPR, $1, $2, $3); }
    ;

shift_expression
    : additive_expression
    | shift_expression LSHIFT_OP additive_expression { set($$, EXPR, $1, $2, $3); }
    | shift_expression RSHIFT_OP additive_expression { set($$, EXPR, $1, $2, $3); }
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression        { set($$, EXPR, $1, $2, $3); }
    | relational_expression LEQ_OP shift_expression     { set($$, EXPR, $1, $2, $3); }
    | relational_expression '>' shift_expression        { set($$, EXPR, $1, $2, $3); }
    | relational_expression GEQ_OP shift_expression     { set($$, EXPR, $1, $2, $3); }
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression   { set($$, EXPR, $1, $2, $3); }
    | equality_expression NEQ_OP relational_expression  { set($$, EXPR, $1, $2, $3); }
    ;

bitwise_and_expression
    : equality_expression
    | bitwise_and_expression '&' equality_expression { set($$, EXPR, $1, $2, $3); }
    ;

bitwise_xor_expression
    : bitwise_and_expression
    | bitwise_xor_expression '^' bitwise_and_expression { set($$, EXPR, $1, $2, $3); }
    ;

bitwise_or_expression
    : bitwise_xor_expression
    | bitwise_or_expression '|' bitwise_xor_expression { set($$, EXPR, $1, $2, $3); }
    ;

logical_and_expression
    : bitwise_or_expression
    | logical_and_expression LAND_OP bitwise_or_expression { set($$, EXPR, $1, $2, $3); }
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression LOR_OP logical_and_expression { set($$, EXPR, $1, $2, $3); }
    ;


    /* Right precedence (Right to Left) */

assignment_expression
    : logical_or_expression
    | logical_or_expression '=' assignment_expression { set($$, EXPR, $1, $2, $3); }
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
    : declaration_specifiers init_declarator_list ';' { set($$, $2->hint, $1, $2, $3); }
    ;

// grammar describing a type
declaration_specifiers
      /* e.g. int */
    : type_specifier
      /* e.g. signed int */
    | type_specifier declaration_specifiers     { set($$, NOTAG, $1, $2); }
      /* i.e. const */
    | type_qualifier
      /* e.g. const int */
    | type_qualifier declaration_specifiers     { set($$, NOTAG, $1, $2); }
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
    : init_declarator                           { $$ = $1, set_hint($$, $1); }
      /* multiple declared instance */
    | init_declarator ',' init_declarator_list  { set($$, NOTAG, $1, $2, $3), set_hint($$, $1); }
    ;

// declare without/with copy initialization
init_declarator
    : declarator                    { $$ = $1, set_hint($$, $1); }
    | declarator '=' initializer    { set($$, NOTAG, $1, $2, $3), set_hint($$, $1); }
    ;

// declare without/with pointer
declarator
      /* non-pointer */
    : direct_declarator         { $$ = $1, set_hint($$, $1); }
      /* single level pointer */
    | pointer direct_declarator { set($$, NOTAG, $1, $2), set_hint($$, $2); }
    ;

// declare the name of variable
direct_declarator
    : IDENTIFIER                            { $$ = $1, set_hint($$, SDEC); }
    | IDENTIFIER '(' ')'                    { set($$, NOTAG, $1, $2, $3), set_hint($$, FDEC); }
    | IDENTIFIER '(' parameter_list ')'     { set($$, NOTAG, $1, $2, $3, $4), set_hint($$, FDEC); }
    | direct_declarator '[' expression ']'  { set($$, NOTAG, $1, $2, $3, $4), set_hint($$, ADEC); }
    ;

// grammar of parameter list
parameter_list
    : parameter_declaration
    | parameter_declaration ',' parameter_list { set($$, NOTAG, $1, $2, $3); }
    ;

parameter_declaration
      /* TYPE id */
    : declaration_specifiers declarator { set($$, NOTAG, $1, $2); }
    ;

// terminal: pointer (only supports single level pointer)
pointer
    : '*'
    ;

initializer
    : expression 
    | '{' initializer_list '}'  { set($$, NOTAG, $1, $2, $3); }
    ;

initializer_list
    : initializer
    | initializer ',' initializer_list  { set($$, NOTAG, $1, $2, $3); }
    ;

%%

int main(void) {
    yyparse();
    return 0;
}

int yyerror(std::string s) {
    std::cerr << "[yyerror] " << s << std::endl;
    return 0;
}

void print_and_bye(YYSTYPE rt) {
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

