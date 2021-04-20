%{
    #include <iostream>
    #include <string>
    #define info std::cerr << "[INFO]"

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
        const char *str {"string"};
        const char *chr {"char"};
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

%x LINE_COMMENT
%x BLOCK_COMMENT

%%

    /* LINE COMMENT related rules */

<LINE_COMMENT>\n {
    pushToLine(yytext);
    info << "exit line comment mode" << std::endl;
    BEGIN 0;
    printLine();
    clearLine();
}

<LINE_COMMENT>{sigma} {
    pushToLine(yytext);
}

    /* BLOCK COMMENT related rules */

<BLOCK_COMMENT>\n {
    pushToLine(yytext);
    printLine();
    clearLine();
}

<BLOCK_COMMENT>"*/" {
    pushToLine(yytext);
    info << "exit line comment mode" << std::endl;
    BEGIN 0;
}

<BLOCK_COMMENT>{sigma} {
    pushToLine(yytext);
}

    /* normal mode related rules */

\n  { 
    pushToLine(yytext);
    printLine();
    clearLine();
}

"//" {
    pushToLine(yytext);
    info << "enter line comment mode" << std::endl;
    BEGIN LINE_COMMENT;
}

"/*" {
    pushToLine(yytext);
    info << "enter block comment mode" << std::endl;
    BEGIN BLOCK_COMMENT;
}

\"{c_char}*\" {
    pushToLine(yytext);
    printToken(Token::str, yytext);
}

\'{c_char}\' {
    pushToLine(yytext);
    printToken(Token::chr, yytext);
}

{c_char} {
    pushToLine(yytext);
}

%% 

int main(int argc, char *argv[]) {
    yylex();
    return 0;
}

