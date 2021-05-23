%{
    #include <iostream>
    #include <string>

    #ifdef DEV
    #define info(x) do { std::cerr << "[P:INFO]" << x << std::endl; } while (0)
    #else
    #define info(x) do {} while (0)
    #endif

    #include "y.tab.h"

    extern "C" int yylex();

    extern std::string yylval;

%}

sigma               [\x20-\x7e]
special_char        [\\'"]
    /*escape_seq          \\[\\'"0tn]*/
escape_seq          \\.
c_char              ([\x20-\x7e]{-}[\\'"])|(\\.)|(\\[0-7]{1,3})|(\\x[a-fA-F0-9]+)|(\\u[a-fA-F0-9]{4})|(\\U[a-fA-F0-9]{8})
operators           \+|-|\*|\/|%|\+\+|--|<|<=|>|>=|==|!=|=|&&|\|\||!|&|\|
punctuations        :|;|,|\.|\[|\]|\(|\)|\{|\}
keywords            for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
macros              NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
identifiers         [a-zA-Z_][a-zA-Z0-9_]*

%x LINE_COMMENT
%x BLOCK_COMMENT

%%

    /**********************************
     *
     * LINE COMMENT related rules 
     *
     **********************************/

<LINE_COMMENT>\n {
    info("exit line comment mode");
    BEGIN 0;
}

<LINE_COMMENT>. {
}

    /**********************************
     *
     * BLOCK COMMENT related rules 
     *
     **********************************/

<BLOCK_COMMENT>\n {
}

<BLOCK_COMMENT>"*/" {
    info("exit line comment mode");
    BEGIN 0;
}

<BLOCK_COMMENT>. {
}

    /**********************************
     *
     * Normal mode related rules
     *
     **********************************/

\n  { 
    info("consume new line");
}

"//" /* begin line comment */ {
    info("enter line comment mode");
    BEGIN LINE_COMMENT;
}

"/*" /* begin block comment */ {
    info("enter block comment mode");
    BEGIN BLOCK_COMMENT;
}

\"({c_char}|\')*\" /* string literals */ {
    info("consume string literal");
//  printToken(Token::STR, yytext);
}

\'({c_char}|\")\' /* char literals */ {
    info("consume char literal");
//  printToken(Token::CHAR, yytext);
}

^[ \t]*#pragma[ \t]+source[ \t]+on$ /* #pragma source on */ {
    info("set pragma source on");
}

^[ \t]*#pragma[ \t]+source[ \t]+off$ /* #pragma source off */ {
    info("set pragma source off");
}

^[ \t]*#pragma[ \t]+token[ \t]+on$ /* #pragma token on */ {
    info("set pragma token on");
}

^[ \t]*#pragma[ \t]+token[ \t]+off$ /* #pragma token off */ {
    info("set pragma token off");
}

[\+-]?[0-9]+\.[0-9]* /* float point literal case1: (sign)x. */ {
    info("consume float point literal");
//  printToken(Token::FP, yytext);
}

[\+-]?[0-9]*\.[0-9]+ /* float point literal case2: (sign).x */ {
    info("consume float point literal");
//  printToken(Token::FP, yytext);
}

[\+-]?[0-9]+ /* int literal */ {
    info("consume int literal");
//  printToken(Token::INT, yytext);
}

{operators} {
    info("consume operator");
//  printToken(Token::OP, yytext);
}

{punctuations} {
    info("consume puncuation");
//  printToken(Token::PUNC, yytext);
    yylval = yytext;
    return ';';
}

{keywords} {
    info("consume keywords");
//  printToken(Token::KEY, yytext);
    // Dirty quick fix
    yylval = yytext;
    return INT;
}

{macros} {
    info("consume macro");
//  printToken(Token::MACRO, yytext);
}

{identifiers} {
    info("consume id");
//  printToken(Token::ID, yytext);
    yylval = yytext;
    return IDENTIFIER;
}

. /* not matching any rule */ {
    info("consume an unmatched char");
}

%% 

