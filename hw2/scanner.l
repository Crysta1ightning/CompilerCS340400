%{
#include <stdio.h>
#include "y.tab.h"
%}

%%

"const"              { return CONST; }
"signed"             { return SIGNED; }
"unsigned"           { return UNSIGNED; }
"float"              { return FLOAT; }
"double"             { return DOUBLE; }
"void"               { return VOID; }
"long"               { return LONG; }
"short"              { return SHORT; }
"char"               { return CHAR; }
"int"                { return INT; }
"for"                { return FOR; }
"if"                 { return IF; }
"else"               { return ELSE; }
"switch"             { return SWITCH; }
"case"               { return CASE; }
"default"            { return DEFAULT; }
"while"              { return WHILE; }
"do"                 { return DO; }
"return"             { return RETURN; }
"break"              { return BREAK; }
"continue"           { return CONTINUE; }

"+"                  { return PLUS; }
"-"                  { return MINUS; }
"*"                  { return TIMES_POINTER; }
"/"                  { return DIVIDE; }
"%"                  { return MODULO; }
"++"                 { return INCREMENT; }
"--"                 { return DECREMENT; }
"=="                 { return EQUAL; }
"!="                 { return NOT_EQUAL; }
"<="                 { return LESS_EQUAL; }
">="                 { return GREATER_EQUAL; }
"<"                  { return LESS_THAN; }
">"                  { return GREATER_THAN; }
"="                  { return ASSIGNMENT; }
"&&"                 { return LOGICAL_AND; }
"||"                 { return LOGICAL_OR; }
"!"                  { return LOGICAL_NOT; }
"&"                  { return BITWISE_AND; }
"|"                  { return BITWISE_OR; }
"^"                  { return BITWISE_XOR; }
"~"                  { return BITWISE_NOT; }
"<<"                 { return LEFT_SHIFT; }
">>"                 { return RIGHT_SHIFT; }

"("                  { return LEFT_PAREN; }
")"                  { return RIGHT_PAREN; }
"["                  { return LEFT_BRACKET; }
"]"                  { return RIGHT_BRACKET; }
"{"                  { return LEFT_BRACE; }
"}"                  { return RIGHT_BRACE; }
";"                  { return SEMICOLON; }
","                  { return COMMA; }

[0-9]+               { yylval.intval = atoi(yytext); return CONSTANT; }
\"([^\\\n]|\\.)*\"    { yylval.strval = strdup(yytext); return STRING_LITERAL; }
[a-zA-Z_][a-zA-Z0-9_]* { yylval.id = strdup(yytext); return IDENTIFIER; }

[ \t\n]+             { /* ignore whitespace */ }

%%

int yywrap() {
    return 1;
}

// void yyerror(const char *s) {
//     fprintf(stderr, "Error: %s\n", s);
// }
