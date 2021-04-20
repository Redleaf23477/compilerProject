%{
    #include <iostream>
    #include <string>

    #ifdef DEV
    #define info(x) do { std::cerr << "[INFO]" << x << std::endl; } while (0)
    #else
    #define info(x) do {} while (0)
    #endif

    extern "C" int yylex();
    
    std::string line;
    bool token_on = true;
    bool source_on = true;
    
    void printLine() {
        static int lineNo = 0;
        // note that '\n' is included in `line` as well
        lineNo++;
        if (source_on) {
            std::cout << lineNo << ":" << line;
        }
    }
    void clearLine() {
        line.clear();
    }
    void pushToLine(char *matched) {
        line.append(matched);
    }

    namespace Token {
        constexpr char STR[] {"string"};
        constexpr char CHAR[] {"char"};
        constexpr char FP[] {"float"};
        constexpr char INT[] {"integer"};
        constexpr char OP[] {"op"};
        constexpr char PUNC[] {"punc"};
        constexpr char KEY[] {"key"};
        constexpr char MACRO[] {"macro"};
        constexpr char ID[] {"id"};
    }
    
    void printToken(std::string_view tokType, std::string_view tok) {
        if (token_on) {
            std::cout << "#" << tokType << ":" << tok << std::endl;
        }
    }
%}

sigma               [\x20-\x7e]
special_char        [\\'"]
escape_seq          \\[\\'"0tn]
c_char              ([\x20-\x7e]{-}[\\'"])|(\\[\\'"0tn])
operators           \+|-|\*|\/|%|\+\+|--|<|<=|>|>=|==|!=|=|&&|\|\||!|&|\|
punctuations        :|;|,|\.|\[|\]|\(|\)|\{|\}
keywords            for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
macros              NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
identifiers         [a-zA-z_][a-zA-Z0-9_]*

%x LINE_COMMENT
%x BLOCK_COMMENT

%%

    /**********************************
     *
     * LINE COMMENT related rules 
     *
     **********************************/

<LINE_COMMENT>\n {
    pushToLine(yytext);
    info("exit line comment mode");
    BEGIN 0;
    printLine();
    clearLine();
}

<LINE_COMMENT>. {
    pushToLine(yytext);
}

    /**********************************
     *
     * BLOCK COMMENT related rules 
     *
     **********************************/

<BLOCK_COMMENT>\n {
    pushToLine(yytext);
    printLine();
    clearLine();
}

<BLOCK_COMMENT>"*/" {
    pushToLine(yytext);
    info("exit line comment mode");
    BEGIN 0;
}

<BLOCK_COMMENT>. {
    pushToLine(yytext);
}

    /**********************************
     *
     * Normal mode related rules
     *
     **********************************/

\n  { 
    pushToLine(yytext);
    printLine();
    clearLine();
}

"//" /* begin line comment */ {
    pushToLine(yytext);
    info("enter line comment mode");
    BEGIN LINE_COMMENT;
}

"/*" /* begin block comment */ {
    pushToLine(yytext);
    info("enter block comment mode");
    BEGIN BLOCK_COMMENT;
}

\"{c_char}*\" /* string literals */ {
    pushToLine(yytext);
    printToken(Token::STR, yytext);
}

\'{c_char}\' /* char literals */ {
    pushToLine(yytext);
    printToken(Token::CHAR, yytext);
}

^[ \t]*#pragma[ \t]+source[ \t]+on$ /* #pragma source on */ {
    pushToLine(yytext);
    info("set pragma source on");
    source_on = true;
}

^#[ \t]*pragma[ \t]+source[ \t]+off$ /* #pragma source off */ {
    pushToLine(yytext);
    info("set pragma source off");
    source_on = false;
}

^#[ \t]*pragma[ \t]+token[ \t]+on$ /* #pragma token on */ {
    pushToLine(yytext);
    info("set pragma token on");
    token_on = true;
}

^#[ \t]*pragma[ \t]+token[ \t]+off$ /* #pragma token off */ {
    pushToLine(yytext);
    info("set pragma token off");
    token_on = false;
}

[\+-]?[0-9]+\.[0-9]* /* float point literal */ {
    pushToLine(yytext);
    printToken(Token::FP, yytext);
}

[\+-]?[0-9]+ /* int literal */ {
    pushToLine(yytext);
    printToken(Token::INT, yytext);
}

{operators} {
    pushToLine(yytext);
    printToken(Token::OP, yytext);
}

{punctuations} {
    pushToLine(yytext);
    printToken(Token::PUNC, yytext);
}

{keywords} {
    pushToLine(yytext);
    printToken(Token::KEY, yytext);
}

{macros} {
    pushToLine(yytext);
    printToken(Token::MACRO, yytext);
}

{identifiers} {
    pushToLine(yytext);
    printToken(Token::ID, yytext);
}

. /* not matching any rule */ {
    pushToLine(yytext);
}

%% 

int main(int argc, char *argv[]) {
    yylex();
    return 0;
}

