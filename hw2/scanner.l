%{
    #include <iostream>
    #include <iomanip>

    #ifdef DEV
    #define report(TYPE) info("consume " << #TYPE << " " << yylval); return TYPE
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

    /*
<BLOCK_COMMENT>\n {
}
    */

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

    /*
\n  { 
    info("consume new line");
}
    */

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
    yylval = yytext;
    report(LITERAL);
}

\'({c_char}|\")\' /* char literals */ {
    yylval = yytext;
    report(LITERAL);
}

    /* TO TEST */
([0-9]*\.[0-9]+)|([0-9]+\.[0-9]*) /* signless loat point literal,  case2: .x , case1: x.*/ {
    double n = atof(yytext);
    char buff[1000]; sprintf(buff, "%f", n);
    yylval = buff;
    report(LITERAL);
}

[0-9]+ /* signless int literal */ {
    int n = atoi(yytext);
    char buff[1000]; sprintf(buff, "%d", n);
    yylval = buff;
    report(LITERAL);
}

    /**********************************
     *
     * Operators
     *
     **********************************/

"++"    { yylval = yytext; report(INC_OP); }
"--"    { yylval = yytext; report(DEC_OP); }
"<="    { yylval = yytext; report(LEQ_OP); }
">="    { yylval = yytext; report(GEQ_OP); }
"=="    { yylval = yytext; report(EQ_OP); }
"!="    { yylval = yytext; report(NEQ_OP); }
">>"    { yylval = yytext; report(RSHIFT_OP); }
"<<"    { yylval = yytext; report(LSHIFT_OP); }
"&&"    { yylval = yytext; report(LAND_OP); }
"||"    { yylval = yytext; report(LOR_OP); }
"+"     { yylval = yytext; report('+'); }
"-"     { yylval = yytext; report('-'); }
"*"     { yylval = yytext; report('*'); }
"/"     { yylval = yytext; report('/'); }
"%"     { yylval = yytext; report('%'); }
"<"     { yylval = yytext; report('<'); }
">"     { yylval = yytext; report('>'); }
"="     { yylval = yytext; report('='); }
"!"     { yylval = yytext; report('!'); }
"~"     { yylval = yytext; report('~'); }
"^"     { yylval = yytext; report('^'); }
"&"     { yylval = yytext; report('&'); }
"|"     { yylval = yytext; report('|'); }
"["     { yylval = yytext; report('['); }
"]"     { yylval = yytext; report(']'); }
"("     { yylval = yytext; report('('); }
")"     { yylval = yytext; report(')'); }

    /**********************************
     *
     * Punctuations
     *
     **********************************/

";"     { yylval = yytext; report(';'); }
":"     { yylval = yytext; report(':'); }
","     { yylval = yytext; report(','); }
"{"     { yylval = yytext; report('{'); }
"}"     { yylval = yytext; report('}'); }

    /**********************************
     *
     * Key words
     *
     **********************************/

"const"     { yylval = yytext; report(CONST); }

"int"       { yylval = yytext; report(INT); }
"char"      { yylval = yytext; report(CHAR); }
"float"     { yylval = yytext; report(FLOAT); }
"double"    { yylval = yytext; report(DOUBLE); }
"void"      { yylval = yytext; report(VOID); }
"signed"    { yylval = yytext; report(SIGNED); }
"unsigned"  { yylval = yytext; report(UNSIGNED); }
"long"      { yylval = yytext; report(LONG); }
"short"     { yylval = yytext; report(SHORT); }

"if"        { yylval = yytext; report(IF); }
"else"      { yylval = yytext; report(ELSE); }
"switch"    { yylval = yytext; report(SWITCH); }
"case"      { yylval = yytext; report(CASE); }
"default"   { yylval = yytext; report(DEFAULT); }
"while"     { yylval = yytext; report(WHILE); }
"do"        { yylval = yytext; report(DO); }
"for"       { yylval = yytext; report(FOR); }
"return"    { yylval = yytext; report(RETURN); }
"break"     { yylval = yytext; report(BREAK); }
"continue"  { yylval = yytext; report(CONTINUE); }

"NULL"      { yylval = "0"; report(NUL); }

    /**********************************
     *
     * Identifier
     *
     **********************************/

{identifiers}   { yylval = yytext; report(IDENTIFIER); }

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

