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

// type qualifiter
%token CONST

// type specifier
%token INT CHAR FLOAT DOUBLE VOID SIGNED UNSIGNED LONG SHORT

// more keywords
%token IF ELSE SWITCH CASE DEFAULT WHILE DO FOR RETURN BREAK CONTINUE

%token NUL

%%

/**********************************
 *
 * Scalar Declaration
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
    ;

// const pointer to type will not happen
pointer
    : '*'                           // e.g. int *ptr

// TODO: wait until expr impl.
initializer
    : LITERAL
%%

int main(void) {
    yyparse();
    return 0;
}

int yyerror(std::string s) {
    std::cerr << "[yyerror] " << s << std::endl;
    return 0;
}
