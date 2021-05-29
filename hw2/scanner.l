%{
    #include <iostream>
    #include <iomanip>

    #ifdef DEV
    #define report(TYPE) info("consume " << #TYPE << " " << yylval->token); return TYPE
    #define info(x) do { std::cerr << "[S:INFO] " << x << std::endl; } while (0)
    #else
    #define report(TYPE) return TYPE
    #define info(x) do {} while (0)
    #endif

    #include "y.tab.h"
    #include "yystype.h"

    extern "C" int yylex();

    // extern std::string yylval;
    extern YYSTYPE yylval;

%}

sigma               [\x20-\x7e]
special_char        [\\'"]
    /*escape_seq          \\[\\'"0tn]*/
escape_seq          \\.
c_char              ([\x20-\x7e]{-}[\\'"])|(\\.)|(\\[0-7]{1,3})|(\\x[a-fA-F0-9]+)|(\\u[a-fA-F0-9]{4})|(\\U[a-fA-F0-9]{8})
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
    // return nothong
}

<LINE_COMMENT>. {
    // return nothong
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
    // return nothong
}

<BLOCK_COMMENT>. {
    // return nothong
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

    /**********************************
     *
     * Pragma
     *
     **********************************/

^[ \t]*#pragma[ \t]+source[ \t]+on$ /* #pragma source on */ {
    // do nothing
}

^[ \t]*#pragma[ \t]+source[ \t]+off$ /* #pragma source off */ {
    // do nothing
}

^[ \t]*#pragma[ \t]+token[ \t]+on$ /* #pragma token on */ {
    // do nothing
}

^[ \t]*#pragma[ \t]+token[ \t]+off$ /* #pragma token off */ {
    // do nothing
}

    /**********************************
     *
     * Literals
     *
     **********************************/

\"({c_char}|\')*\" /* string literals */ {
    yylval = new Node(yytext);
    report(LITERAL);
}

\'({c_char}|\")\' /* char literals */ {
    yylval = new Node(yytext);
    report(LITERAL);
}

    /* TO TEST */
([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*) /* signless loat point literal,  case2: .x , case1: x.*/ {
    double n = atof(yytext);
    char buff[1000]; sprintf(buff, "%f", n);
    yylval = new Node(buff);
    report(LITERAL);
}

[0-9]+ /* signless int literal */ {
    int n = atoi(yytext);
    char buff[1000]; sprintf(buff, "%d", n);
    yylval = new Node(buff);
    report(LITERAL);
}

    /**********************************
     *
     * Operators
     *
     **********************************/

"++"    { yylval = new Node(yytext); report(INC_OP); }
"--"    { yylval = new Node(yytext); report(DEC_OP); }
"<="    { yylval = new Node(yytext); report(LEQ_OP); }
">="    { yylval = new Node(yytext); report(GEQ_OP); }
"=="    { yylval = new Node(yytext); report(EQ_OP); }
"!="    { yylval = new Node(yytext); report(NEQ_OP); }
">>"    { yylval = new Node(yytext); report(RSHIFT_OP); }
"<<"    { yylval = new Node(yytext); report(LSHIFT_OP); }
"&&"    { yylval = new Node(yytext); report(LAND_OP); }
"||"    { yylval = new Node(yytext); report(LOR_OP); }
"+"     { yylval = new Node(yytext); report('+'); }
"-"     { yylval = new Node(yytext); report('-'); }
"*"     { yylval = new Node(yytext); report('*'); }
"/"     { yylval = new Node(yytext); report('/'); }
"%"     { yylval = new Node(yytext); report('%'); }
"<"     { yylval = new Node(yytext); report('<'); }
">"     { yylval = new Node(yytext); report('>'); }
"="     { yylval = new Node(yytext); report('='); }
"!"     { yylval = new Node(yytext); report('!'); }
"~"     { yylval = new Node(yytext); report('~'); }
"^"     { yylval = new Node(yytext); report('^'); }
"&"     { yylval = new Node(yytext); report('&'); }
"|"     { yylval = new Node(yytext); report('|'); }
"["     { yylval = new Node(yytext); report('['); }
"]"     { yylval = new Node(yytext); report(']'); }
"("     { yylval = new Node(yytext); report('('); }
")"     { yylval = new Node(yytext); report(')'); }

    /**********************************
     *
     * Punctuations
     *
     **********************************/

";"     { yylval = new Node(yytext); report(';'); }
":"     { yylval = new Node(yytext); report(':'); }
","     { yylval = new Node(yytext); report(','); }
"{"     { yylval = new Node(yytext); report('{'); }
"}"     { yylval = new Node(yytext); report('}'); }

    /**********************************
     *
     * Key words
     *
     **********************************/

"const"     { yylval = new Node(yytext); report(CONST); }

"int"       { yylval = new Node(yytext); report(INT); }
"char"      { yylval = new Node(yytext); report(CHAR); }
"float"     { yylval = new Node(yytext); report(FLOAT); }
"double"    { yylval = new Node(yytext); report(DOUBLE); }
"void"      { yylval = new Node(yytext); report(VOID); }
"signed"    { yylval = new Node(yytext); report(SIGNED); }
"unsigned"  { yylval = new Node(yytext); report(UNSIGNED); }
"long"      { yylval = new Node(yytext); report(LONG); }
"short"     { yylval = new Node(yytext); report(SHORT); }

"if"        { yylval = new Node(yytext); report(IF); }
"else"      { yylval = new Node(yytext); report(ELSE); }
"switch"    { yylval = new Node(yytext); report(SWITCH); }
"case"      { yylval = new Node(yytext); report(CASE); }
"default"   { yylval = new Node(yytext); report(DEFAULT); }
"while"     { yylval = new Node(yytext); report(WHILE); }
"do"        { yylval = new Node(yytext); report(DO); }
"for"       { yylval = new Node(yytext); report(FOR); }
"return"    { yylval = new Node(yytext); report(RETURN); }
"break"     { yylval = new Node(yytext); report(BREAK); }
"continue"  { yylval = new Node(yytext); report(CONTINUE); }

"NULL"      { 
    char buff[2] = "0";
    yylval = new Node(buff); 
    report(NUL); 
}

    /**********************************
     *
     * Identifier
     *
     **********************************/

{identifiers}   { yylval = new Node(yytext); report(IDENTIFIER); }

    /**********************************
     *
     * Defaults
     *
     **********************************/

. /* not matching any rule */ {
    info("consume an unmatched char '" << yytext << "'");
    // return nothong
}

%% 

