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
        info("c[" << j << "]=(" << (dest->child[j]->token? dest->child[j]->token : "X") << "," << tag2str(dest->child[j]->tag) << ")");
    }
#endif
}

void set(YYSTYPE &dest, Tag t, YYSTYPE _1) { set(dest, t, { _1 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2) { set(dest, t, { _1, _2 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3) { set(dest, t, { _1, _2, _3 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4) { set(dest, t, { _1, _2, _3, _4 } ); }
void set(YYSTYPE &dest, Tag t, YYSTYPE _1, YYSTYPE _2, YYSTYPE _3, YYSTYPE _4, YYSTYPE _5) { set(dest, t, { _1, _2, _3, _4, _5 } ); }

inline void BEG(Tag t) { std::cout << "<" << tag2str(t) << ">"; }
inline void END(Tag t) { std::cout << "</" << tag2str(t) << ">"; }

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

%token NUL

%start translation_unit

%%

// entry point for parsing
translation_unit
    : external_declaration
    | translation_unit external_declaration
    ;

external_declaration
    : declaration               // e.g. global variables
    | statement
    | function_definition       // e.g. int main() { ... }
    ;

/**********************************
 *
 * Function definition
 *
 **********************************/

function_definition
    : declaration_specifiers declarator compound_statement
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
    : expression ';'
    ;

selection_statement
    : if_statement
    | switch_statement
    ;

if_statement
    : IF '(' expression ')' compound_statement
    | IF '(' expression ')' compound_statement ELSE compound_statement
    ;

switch_statement
    : SWITCH '(' expression ')' '{' '}'                     // no switch clause
    | SWITCH '(' expression ')' '{' switch_clause_list '}'  // 1 or more switch clause
    ;

switch_clause_list
    : switch_clause
    | switch_clause switch_clause_list 
    ;

switch_clause
    : CASE expression ':' 
    | CASE expression ':' statement_list
    | DEFAULT ':'
    | DEFAULT ':' statement_list
    ;

statement_list
    : statement
    | statement statement_list
    ;

iteration_statement
    : while_statement
    | do_while_statement
    | for_statement
    ;

while_statement
    : WHILE '(' expression ')' statement
    ;

do_while_statement
    : DO statement WHILE '(' expression ')' ';'
    ;

for_statement
    : FOR '(' emptiable_expression ';' emptiable_expression ';' emptiable_expression ')' statement
    ;

emptiable_expression
    : /* empty */
    | expression
    ;

jump_statement
    : break_statement
    | continue_statement
    | return_statement
    ;

break_statement
    : BREAK ';'
    ;

continue_statement
    : CONTINUE ';'
    ;

return_statement
    : RETURN ';'
    | RETURN expression ';'
    ;

compound_statement
    : '{' '}'
    | '{' statement_declaration_list '}'
    ;

statement_declaration_list
    : statement
    | statement statement_declaration_list
    | declaration
    | declaration statement_declaration_list
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
    | suffix_expression INC_OP { set($$, EXPR, $1, $2); }
    | suffix_expression DEC_OP { set($$, EXPR, $1, $2); }
    ;

unary_operation_expression
    : suffix_expression
    | unary_operator suffix_expression { set($$, EXPR, $1, $2); }
    ;

unary_operator
    : '&'
    | '*'
    | '!'
    | '~'
    | '+'
    | '-'
    ;

prefix_expression
    : unary_operation_expression
    | INC_OP unary_operation_expression             { set($$, EXPR, $1, $2); }
    | DEC_OP unary_operation_expression             { set($$, EXPR, $1, $2); }
    | '(' type_name ')' unary_operation_expression  { set($$, EXPR, $1, $2, $3, $4); }
    ;

type_name
    : specifier_qualifier_list
    | specifier_qualifier_list pointer
    ;

specifier_qualifier_list
    : type_qualifier
    | type_qualifier specifier_qualifier_list
    | type_specifier
    | type_specifier specifier_qualifier_list
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
    | relational_expression '<' shift_expression { set($$, EXPR, $1, $2, $3); }
    | relational_expression LEQ_OP shift_expression { set($$, EXPR, $1, $2, $3); }
    | relational_expression '>' shift_expression { set($$, EXPR, $1, $2, $3); }
    | relational_expression GEQ_OP shift_expression { set($$, EXPR, $1, $2, $3); }
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression { set($$, EXPR, $1, $2, $3); }
    | equality_expression NEQ_OP relational_expression { set($$, EXPR, $1, $2, $3); }
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
    : declaration_specifiers init_declarator_list ';'   // (Type) (id/id=...) ;
    ;

// grammar describing a type
declaration_specifiers
    : type_specifier                            // e.g. int
    | type_specifier declaration_specifiers     // e.g. signed int
    | type_qualifier                            // const
    | type_qualifier declaration_specifiers     // e.g. const int
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
    : init_declarator                               // single declared instance
    | init_declarator ',' init_declarator_list      // multiple declared instance
    ;

// declare without/with copy initialization
init_declarator
    : declarator
    | declarator '=' initializer
    ;

// declare without/with pointer
declarator
    : direct_declarator             // non-pointer
    | pointer direct_declarator     // single level pointer
    ;

// declare the name of variable
direct_declarator
    : IDENTIFIER 
    | IDENTIFIER '(' ')'                    // function with no parameters
    | IDENTIFIER '(' parameter_list ')'
    | direct_declarator '[' expression ']' 
    ;

// grammar of parameter list
parameter_list
    : parameter_declaration
    | parameter_declaration ',' parameter_list
    ;

parameter_declaration
    : declaration_specifiers declarator     // TYPE id
    ;

// terminal: pointer (only supports single level pointer)
pointer
    : '*'                           // e.g. int *ptr
    ;

initializer
    : expression 
    | '{' initializer_list '}'
    ;

initializer_list
    : initializer
    | initializer ',' initializer_list
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
