%{
#include <stdio.h>
#include <stdbool.h>
int lineCount = 0;
char thisLine[600];
bool source = true;
bool token = true;

void printKey(const char *key, const char *word) {
    if (token) {
        printf("#%s:%s\n", key, word);
    }
} 
void addToLine(const char *word) {
    if (source) {
        int len = strlen(thisLine);
        if (len + strlen(word) < sizeof(thisLine)) {
            strcat(thisLine, word);
        }
        else {
            fprintf(stderr, "Line buffer full, skipping remainder of line.\n");
        }
    }
  
}
void printLine() {
    ++lineCount;
    if (source) {
        printf("%d:%s\n", lineCount,  thisLine);
    }
    thisLine[0] = '\0'; // Reset thisLine to an empty string
}
%}

%option noyywrap

key         for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
macro       NULL|__COUNTER__|__LINE__|INT_MAX|INT_MIN|CHAR_MAX|CHAR_MIN|MAX|MIN
operator    (\+\+|\-\-|==|!=|<=|>=|\&\&|\|\||[+\-*\/%<>=!&|])
identifier  [a-zA-Z_][a-zA-Z0-9_]*
punctuation [:;,\.\[\]\(\)\{\}]
integer     [+-]?[0-9]+
float       [+-]?([0-9]+[.][0-9]*|[.][0-9]+)
char        \'(.|\\a|\\b|\\e|\\f|\\n|\\r|\\t|\\v|\\\\|\\\'|\\\"|\\\?|\\[0-7]{1,3}|\\x[0-9a-fA-F]+|\\u[0-9a-fA-F]{4,4}|\\U[0-9a-fA-F]{8,8})\'
string      \"((\\a|\\b|\\e|\\f|\\n|\\r|\\t|\\v|\\\\|\\\'|\\\"|\\\?|\\[0-7]{1,3}|\\x[0-9a-fA-F]+|\\u[0-9a-fA-F]{4,4}|\\U[0-9a-fA-F]{8,8})|[^\\\"])*\"
pragmason   #pragma[ ]*source[ ]*on
pragmasoff  #pragma[ ]*source[ ]*off
pragmaton   #pragma[ ]*token[ ]*on
pragmatoff  #pragma[ ]*token[ ]*off

%x          ONELINECOMMENT
%x          MULTILINECOMMENT
%%

"//"    {
            BEGIN(ONELINECOMMENT);
            addToLine(yytext);
        }

<ONELINECOMMENT>\n {
            printLine();
            BEGIN(INITIAL);
        }

<ONELINECOMMENT>. {
            addToLine(yytext);
        }

"/*"    {
            BEGIN(MULTILINECOMMENT);
            addToLine(yytext);
        }

<MULTILINECOMMENT>\n {
            printLine();
        }

<MULTILINECOMMENT>"*/" {
            addToLine(yytext);
            BEGIN(INITIAL);
        }

<MULTILINECOMMENT>. {
            addToLine(yytext);
        }

{pragmason} {
            source = true;
            addToLine(yytext);
        }

{pragmasoff} {
            source = false;
            addToLine(yytext);
        }

{pragmaton} {
            token = true;
            addToLine(yytext);
        }

{pragmatoff} {
            token = false;
            addToLine(yytext);
        }

{key}   {
            printKey("key", yytext);
            addToLine(yytext);
        }

{macro}   {
            printKey("macro", yytext);
            addToLine(yytext);
        }

{operator}    {
            printKey("op", yytext);
            addToLine(yytext);
        }

{identifier} {
            printKey("id", yytext);
            addToLine(yytext);
        }

{punctuation}    {
            printKey("punc", yytext);
            addToLine(yytext);
        }

{integer}    {
            printKey("integer", yytext);
            addToLine(yytext);
        }

{float}    {
            printKey("float", yytext);
            addToLine(yytext);
        }

{char}    {
            printKey("char", yytext);
            addToLine(yytext);
        }

{string}    {
            printKey("string", yytext);
            addToLine(yytext);
        }

\n      {
            printLine();
        }

.       {
            addToLine(yytext);
        }

%%

int main(void)
{
    yylex();
    return 0;
}
