%{

#include <cstdio>
#include <iostream>
#include <string>

extern "C" {
    int yyerror(std::string s);
    int yyparse();
    int yylex();
}

#define YYSTYPE std::string

%}

/**********************************
 *
 * yylval Type Definition
 *
 **********************************/

/*
%union {
    int intval;
}
*/

/**********************************
 *
 * Token Definitions
 *
 **********************************/

%token IDENTIFIER

// type qualifiter
%token CONST

// type specifier
%token INT, CHAR, FLOAT, DOUBLE, VOID
%token SIGNED, UNSIGNED
%token LONG, SHORT

%%

declaration
    : declaration_specifiers init_declarator_list ';'
    ;

declaration_specifiers
    : type_specifier
    ;

type_specifier
    : INT { std::cout << "type = " << $1 << std::endl; $$ = $1; }
    | CHAR
    | FLOAT
    | DOUBLE
    | VOID
    | SIGNED
    | UNSIGNED
    | LONG
    | SHORT
    ;

init_declarator_list
    : init_declarator
    ;

init_declarator
    : declarator
    ;

declarator
    : direct_declarator
    ;

direct_declarator
    : IDENTIFIER { std::cout << "id = " << $1 << std::endl; $$ = $1; }
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
