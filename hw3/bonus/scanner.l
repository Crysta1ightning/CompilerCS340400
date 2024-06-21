%{
    #include <iostream>
    // #include <iomanip>

    #include "yystype.h"
    #include "y.tab.h"

    extern "C" int yylex();
%}

identifier  [a-zA-Z_][a-zA-Z0-9_]*
integer     [0-9]+
double      [0-9]+[.][0-9]*|[.][0-9]+
char        \'(.|\\a|\\b|\\e|\\f|\\n|\\r|\\t|\\v|\\\\|\\\'|\\\"|\\\?|\\[0-7]{1,3}|\\x[0-9a-fA-F]+|\\u[0-9a-fA-F]{4,4}|\\U[0-9a-fA-F]{8,8})\'
string      \"((\\a|\\b|\\e|\\f|\\n|\\r|\\t|\\v|\\\\|\\\'|\\\"|\\\?|\\[0-7]{1,3}|\\x[0-9a-fA-F]+|\\u[0-9a-fA-F]{4,4}|\\U[0-9a-fA-F]{8,8})|[^\\\"])*\"

%x          ONELINECOMMENT
%x          MULTILINECOMMENT

%%
"//"    {
            BEGIN(ONELINECOMMENT);
        }

<ONELINECOMMENT>\n {
            BEGIN(INITIAL);
        }

<ONELINECOMMENT>. {}

"/*"    {
            BEGIN(MULTILINECOMMENT);
        }

<MULTILINECOMMENT>\n {}

<MULTILINECOMMENT>"*/" {
            BEGIN(INITIAL);
        }

<MULTILINECOMMENT>. {}

"const"              { yylval.node = new Node(yytext); return CONST; }
"int"                { yylval.node = new Node(yytext); return INT; }
"uint32_t"           { yylval.node = new Node(yytext); return INT; }
"void"               { yylval.node = new Node(yytext); return VOID; }
"for"                { yylval.node = new Node(yytext); return FOR; }
"if"                 { yylval.node = new Node(yytext); return IF; }
"else"               { yylval.node = new Node(yytext); return ELSE; }
"while"              { yylval.node = new Node(yytext); return WHILE; }
"do"                 { yylval.node = new Node(yytext); return DO; }
"return"             { yylval.node = new Node(yytext); return RETURN; }
"break"              { yylval.node = new Node(yytext); return BREAK; }
"__rv__ukadd8"       { yylval.node = new Node(yytext); return EXT_ADD; }
"__rv__uksub8"       { yylval.node = new Node(yytext); return EXT_SUB; }
"__rv__cmpeq8"       { yylval.node = new Node(yytext); return EXT_EQ; }
"__rv__ucmplt8"      { yylval.node = new Node(yytext); return EXT_LT; }

"+"                  { yylval.node = new Node(yytext); return PLUS; }
"-"                  { yylval.node = new Node(yytext); return MINUS; }
"*"                  { yylval.node = new Node(yytext); return STAR; }
"/"                  { yylval.node = new Node(yytext); return DIVIDE; }
"=="                 { yylval.node = new Node(yytext); return EQUAL; }
"!="                 { yylval.node = new Node(yytext); return NOT_EQUAL; }
"<"                  { yylval.node = new Node(yytext); return LESS_THAN; }
"="                  { yylval.node = new Node(yytext); return ASSIGNMENT; }
"&"                  { yylval.node = new Node(yytext); return ADDR; }

"("                  { yylval.node = new Node(yytext); return LEFT_PAREN; }
")"                  { yylval.node = new Node(yytext); return RIGHT_PAREN; }
"["                  { yylval.node = new Node(yytext); return LEFT_BRACKET; }
"]"                  { yylval.node = new Node(yytext); return RIGHT_BRACKET; }
"{"                  { yylval.node = new Node(yytext); return LEFT_BRACE; }
"}"                  { yylval.node = new Node(yytext); return RIGHT_BRACE; }
":"                  { yylval.node = new Node(yytext); return COLON; }
";"                  { yylval.node = new Node(yytext); return SEMICOLON; }
","                  { yylval.node = new Node(yytext); return COMMA; }

"NULL"      {     // treat NULL exactly the same as reading `0`
    char buff[2] = "0";
    yylval.node = new Node(buff); 
    return LITERAL;
}

"HIGH"      {     // treat HIGH exactly the same as reading `1`
    char buff[2] = "1";
    yylval.node = new Node(buff); 
    return LITERAL;
}

"LOW"      {    // treat LOW exactly the same as reading `0`
    char buff[2] = "0";
    yylval.node = new Node(buff); 
    return LITERAL;
}

{identifier} { yylval.node = new Node(yytext); return IDENTIFIER; }
{integer}    { yylval.node = new Node(yytext); return LITERAL; }
{double}     { yylval.node = new Node(yytext); return LITERAL; }
{char}       { yylval.node = new Node(yytext); return LITERAL; }
{string}     { yylval.node = new Node(yytext); return LITERAL; }


[ \t\n]+             { /* ignore whitespace */ }

%% 

