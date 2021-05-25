%{

#include <cstdio>
#include <cassert>
#include <iostream>
#include <string>

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
#define YYSTYPE std::string

// usefull helper functions

enum Tag {
    SDEC,  // scalar declaration
    ADEC,  // array declaration
    FDEC,  // function declaration
    FDEF,  // function definition
    EXPR,  // expr
    STMT   // stmt
};

inline std::string tag2str(Tag t) {
    switch (t) {
    case SDEC: return "scalar_decl";
    case ADEC: return "array_decl";
    case FDEC: return "func_decl";
    case FDEF: return "func_def";
    case EXPR: return "expr";
    case STMT: return "stmt";
    default: info("unknown tag " << t); assert(false && "unreachable"); return "unknown";
    }
}

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
    | expression ';' // TODO: delete me, just for testing
//    | function_definition       // e.g. int main() { ... }
    ;

/**********************************
 *
 * Expression (Listed from high to low precedence)
 *
 **********************************/

// highest precedence, should not be separated
primary_expression
    : IDENTIFIER
    | LITERAL
    | '(' expression ')'
    ;

    /* Right precedence (Right to Left) */

suffix_expression
    : primary_expression
    | suffix_expression INC_OP
    | suffix_expression DEC_OP
    ;

unary_operation_expression
    : suffix_expression
    | unary_operator suffix_expression
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
    | INC_OP unary_operation_expression
    | DEC_OP unary_operation_expression
    | '(' declaration_specifiers ')' unary_operation_expression     // TODO: rewrite type case
    ;

    /* Left precedence (Left to Right) */

multiplicative_expression
    : prefix_expression
    | multiplicative_expression '*' prefix_expression
    | multiplicative_expression '/' prefix_expression
    | multiplicative_expression '%' prefix_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;

shift_expression
    : additive_expression
    | shift_expression LSHIFT_OP additive_expression
    | shift_expression RSHIFT_OP additive_expression
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression LEQ_OP shift_expression
    | relational_expression '>' shift_expression
    | relational_expression GEQ_OP shift_expression
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
    | equality_expression NEQ_OP relational_expression
    ;

bitwise_and_expression
    : equality_expression
    | bitwise_and_expression '&' equality_expression
    ;

bitwise_xor_expression
    : bitwise_and_expression
    | bitwise_xor_expression '^' bitwise_and_expression
    ;

bitwise_or_expression
    : bitwise_xor_expression
    | bitwise_or_expression '|' bitwise_xor_expression
    ;

logical_and_expression
    : bitwise_or_expression
    | logical_and_expression LAND_OP bitwise_or_expression
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression LOR_OP logical_and_expression
    ;


    /* Right precedence (Right to Left) */

assignment_expression
    : logical_or_expression
    | logical_or_expression '=' assignment_expression
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
    | direct_declarator '[' LITERAL ']'  // TODO: wait until expr impl.
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

// TODO: wait until expr impl.
initializer
    : LITERAL
    | '{' initializer_list '}'
    ;

initializer_list
    : initializer
    | initializer ',' initializer_list
    ;

/**********************************
 *
 * Function Definition
 *
 **********************************/

// TODO: wait until stmt impl.

%%

int main(void) {
    yyparse();
    return 0;
}

int yyerror(std::string s) {
    std::cerr << "[yyerror] " << s << std::endl;
    return 0;
}
