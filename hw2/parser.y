%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void yyerror(const char *s);
int yylex(void);

void threeParamConcat(char *dest, char *s1, char *s2, char *s3) {
    strcat(dest, s1); 
    strcat(dest, s2); 
    strcat(dest, s3); 
    free(s1); 
    free(s2); 
    free(s3);
}

void twoParamConcat(char *dest, char *s1, char *s2) {
    strcat(dest, s1); 
    strcat(dest, s2); 
    free(s1); 
    free(s2); 
}

%}

%union {
    int intval;
    char *strval;
    char *id;
}

%token CONST SIGNED UNSIGNED FLOAT DOUBLE VOID LONG SHORT CHAR INT
%token FOR IF ELSE SWITCH CASE DEFAULT WHILE DO RETURN BREAK CONTINUE
%token PLUS MINUS TIMES_POINTER DIVIDE MODULO INCREMENT DECREMENT
%token EQUAL NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS_THAN GREATER_THAN
%token ASSIGNMENT LOGICAL_AND LOGICAL_OR LOGICAL_NOT
%token BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_NOT
%token LEFT_SHIFT RIGHT_SHIFT
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE
%token SEMICOLON COMMA
%token CONSTANT STRING_LITERAL IDENTIFIER
%token TYPE_CAST

%left PLUS MINUS
%left TIMES_POINTER DIVIDE MODULO
%right UNARY_PLUS UNARY_MINUS
%left POST_INCREMENT POST_DECREMENT
%right PRE_INCREMENT PRE_DECREMENT

%type <intval> CONSTANT
%type <strval> STRING_LITERAL 
%type <id> IDENTIFIER

%token <strval> CONST SIGNED UNSIGNED FLOAT DOUBLE VOID LONG SHORT CHAR INT
%token <strval> FOR IF ELSE SWITCH CASE DEFAULT WHILE DO RETURN BREAK CONTINUE
%token <strval> PLUS MINUS TIMES DIVIDE MODULO INCREMENT DECREMENT
%token <strval> EQUAL NOT_EQUAL LESS_EQUAL GREATER_EQUAL LESS_THAN GREATER_THAN
%token <strval> ASSIGNMENT LOGICAL_AND LOGICAL_OR LOGICAL_NOT
%token <strval> BITWISE_AND BITWISE_OR BITWISE_XOR BITWISE_NOT
%token <strval> LEFT_SHIFT RIGHT_SHIFT
%token <strval> LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE
%token <strval> SEMICOLON COMMA
%token <intval> CONSTANT
%token <strval> TYPE_CAST


%type <strval> program 
%type <strval> stmt scalarDeclaration arrayDeclaration functionDeclaration functionDefinition
%type <strval> type idents ident arrays parameters param 
%type <strval> compoundStmt stmts stmt expr argument_list
%type <strval> empty_const empty_signs basic_types basic_types2 basic_types3
%start program

%%

program :  /* empty */ {  }
      //     | program stmt
          | program scalarDeclaration 
          {
            printf("%s", $2);
          }
      //     | program arrayDeclaration
      //     | program functionDeclaration
      //     | program functionDefinition
          ;

// stmt : expr SEMICOLON
//      | ifElseStmt
//      | switchStmt
//      | whileStmt
//      | forStmt
//      | returnStmt
//      | BREAK SEMICOLON
//      | CONTINUE SEMICOLON
//      ;

scalarDeclaration : type idents SEMICOLON
                  { 
										char *s = strdup(";");
                    $$ = malloc(13 + strlen($1) + strlen($2) + strlen(s) + 14 + 1);
                    strcpy($$, "<scalar_decl>");
                    threeParamConcat($$, $1, $2, s);
                    strcat($$, "</scalar_decl>");
                  }
                  ;

type : CONST { $$ = strdup("const"); }
      | empty_const basic_types
      { $$ = malloc(strlen($1) + strlen($2) + 1);
        strcpy($$, "");
        twoParamConcat($$, $1, $2);
      }
      | empty_const empty_signs basic_types2
      { $$ = malloc(strlen($1) + strlen($2) + strlen($3) + 1);
        strcpy($$, "");
        threeParamConcat($$, $1, $2, $3);
      }
      | empty_const empty_signs basic_types3
      { $$ = malloc(strlen($1) + strlen($2) + strlen($3) + 1);
        strcpy($$, "");
        threeParamConcat($$, $1, $2, $3);
      }
      ;

empty_const : /* empty */  { $$ = strdup(""); }
      | CONST { $$ = strdup("const"); }

empty_signs: /* empty */ { $$ = strdup(""); }
      | SIGNED { $$ = strdup("signed"); }
      | UNSIGNED { $$ = strdup("unsigned"); }

basic_types: SIGNED { $$ = strdup("signed"); }
      | UNSIGNED { $$ = strdup("unsigned"); }
      | FLOAT { $$ = strdup("float"); }
      | DOUBLE { $$ = strdup("double"); }
      | VOID { $$ = strdup("void"); }

basic_types2: LONG LONG { $$ = strdup("longlong"); }
      | LONG { $$ = strdup("long"); }
      | SHORT { $$ = strdup("short"); }
      | CHAR { $$ = strdup("char"); }

basic_types3: LONG LONG INT { $$ = strdup("longlongint"); } 
      | LONG INT { $$ = strdup("longint"); }
      | SHORT INT { $$ = strdup("shortint"); }
      | INT { $$ = strdup("int"); }

idents : ident { $$ = strdup($1); }
      | ident COMMA idents
      {
				char* s = strdup(",");
				$$ = malloc(strlen($1) + strlen(s) + strlen($3) + 1);
				strcpy($$, "");
				threeParamConcat($$, $1, s, $3);
      }

ident : TIMES_POINTER IDENTIFIER ASSIGNMENT expr { // pointer here
				char* s = strdup("*");
				$$ = malloc(strlen(s) + strlen($2) + 1);
				strcpy($$, "");
				twoParamConcat($$, s, $2);
			}
			| IDENTIFIER ASSIGNMENT expr {
				
			}
			| TIMES_POINTER IDENTIFIER { // pointer here
				char* s = strdup("*");
				$$ = malloc(strlen(s) + strlen($2) + 1);
				strcpy($$, "");
				twoParamConcat($$, s, $2);
			}
      | IDENTIFIER {
				strcpy($$, $1);
			}
      ;

// arrayDeclaration : type arrays SEMICOLON
//                  {printf("<array_decl>%s %s;</array_decl>", $1, $2); free($1); free($2); }
//                  ;

// arrays : array
//        | array COMMA arrays
//        ;

// array : IDENTIFIER LEFT_BRACKET expr RIGHT_BRACKET
//       | IDENTIFIER LEFT_BRACKET expr RIGHT_BRACKET ASSIGNMENT arrContent
//       ;

// arrContent : LEFT_BRACE exprList RIGHT_BRACE
//            | LEFT_BRACE arrContentList RIGHT_BRACE
//            ;

// exprList : expr
//          | expr COMMA exprList
//          ;

// arrContentList : arrContent
//                | arrContent COMMA arrContentList
//                ;

// functionDeclaration : type IDENTIFIER LEFT_PAREN parameters RIGHT_PAREN
//                     { printf("<func_decl>%s %s(%s);</func_decl>", $1, $2, $3); free($1); free($2); free($3); }
//                     | type '*' IDENTIFIER LEFT_PAREN parameters RIGHT_PAREN
//                     { printf("<func_decl>%s *%s(%s);</func_decl>", $1, $3, $4); free($1); free($3); free($4); }
//                     ;

// parameters : /* empty */ { $$ = strdup(""); }
//            | param
//            | param COMMA parameters
//            ;

// param : type IDENTIFIER
//       {
//         asprintf(&$$, "%s %s", $1, $2);
//         free($1);
//         free($2);
//       }
//       ;

// functionDefinition : functionDeclaration compoundStmt
//                    { printf("<func_def>%s %s</func_def>", $1, $2); free($1); free($2); }
//                    ;

// compoundStmt : LEFT_BRACE stmts RIGHT_BRACE
//              { asprintf(&$$, "{%s}", $2); free($2); }
//              ;

// stmts : /* empty */ { $$ = strdup(""); }
//       | stmts stmt { asprintf(&$$, "%s %s", $1, $2); free($1); free($2); }
//       ;

// expr : expr PLUS expr        { asprintf(&$$, "<expr>%s+%s</expr>", $1, $3); free($1); free($3); }
//      | expr MINUS expr       { asprintf(&$$, "<expr>%s-%s</expr>", $1, $3); free($1); free($3); }
//      | expr TIMES expr       { asprintf(&$$, "<expr>%s*%s</expr>", $1, $3); free($1); free($3); }
//      | expr DIVIDE expr      { asprintf(&$$, "<expr>%s/%s</expr>", $1, $3); free($1); free($3); }
//      | expr MODULO expr      { asprintf(&$$, "<expr>%s%%%s</expr>", $1, $3); free($1); free($3); }
//      | expr INCREMENT        { asprintf(&$$, "<expr>%s++</expr>", $1); free($1); }
//      | expr DECREMENT        { asprintf(&$$, "<expr>%s--</expr>", $1); free($1); }
//      | PLUS expr %prec UNARY_PLUS
//                           { asprintf(&$$, "<expr>+%s</expr>", $2); free($2); }
//      | MINUS expr %prec UNARY_MINUS
//                           { asprintf(&$$, "<expr>-%s</expr>", $2); free($2); }
//      | LOGICAL_NOT expr
//                           { asprintf(&$$, "<expr>!%s</expr>", $2); free($2); }
//      | BITWISE_NOT expr
//                           { asprintf(&$$, "<expr>~%s</expr>", $2); free($2); }
//      | expr BITWISE_XOR expr
//                           { asprintf(&$$, "<expr>%s^%s</expr>", $1, $3); free($1); free($3); }
//      | expr BITWISE_AND expr
//                           { asprintf(&$$, "<expr>%s&%s</expr>", $1, $3); free($1); free($3); }
//      | expr BITWISE_OR expr
//                           { asprintf(&$$, "<expr>%s|%s</expr>", $1, $3); free($1); free($3); }
//      | expr LEFT_SHIFT expr
//                           { asprintf(&$$, "<expr>%s<<%s</expr>", $1, $3); free($1); free($3); }
//      | expr RIGHT_SHIFT expr
//                           { asprintf(&$$, "<expr>%s>>%s</expr>", $1, $3); free($1); free($3); }
//      | expr EQUAL expr
//                           { asprintf(&$$, "<expr>%s==%s</expr>", $1, $3); free($1); free($3); }
//      | expr NOT_EQUAL expr
//                           { asprintf(&$$, "<expr>%s!=%s</expr>", $1, $3); free($1); free($3); }
//      | expr LESS_THAN expr
//                           { asprintf(&$$, "<expr>%s<%s</expr>", $1, $3); free($1); free($3); }
//      | expr LESS_EQUAL expr
//                           { asprintf(&$$, "<expr>%s<=%s</expr>", $1, $3); free($1); free($3); }
//      | expr GREATER_THAN expr
//                           { asprintf(&$$, "<expr>%s>%s</expr>", $1, $3); free($1); free($3); }
//      | expr GREATER_EQUAL expr
//                           { asprintf(&$$, "<expr>%s>=%s</expr>", $1, $3); free($1); free($3); }
//      | expr ASSIGNMENT expr
//                           { asprintf(&$$, "<expr>%s=%s</expr>", $1, $3); free($1); free($3); }
//      | expr LOGICAL_AND expr
//                           { asprintf(&$$, "<expr>%s&&%s</expr>", $1, $3); free($1); free($3); }
//      | expr LOGICAL_OR expr
//                           { asprintf(&$$, "<expr>%s||%s</expr>", $1, $3); free($1); free($3); }
//      | LEFT_PAREN expr RIGHT_PAREN
//                           { asprintf(&$$, "<expr>(%s)</expr>", $2); free($2); }
//      | IDENTIFIER LEFT_PAREN argument_list RIGHT_PAREN
//                           { asprintf(&$$, "<expr>%s(%s)</expr>", $1, $3); free($1); free($3); }
//      | IDENTIFIER LEFT_BRACKET expr RIGHT_BRACKET
//                           { asprintf(&$$, "<expr>%s[%s]</expr>", $1, $3); free($1); free($3); }
//      | expr LEFT_BRACKET expr RIGHT_BRACKET
//                           { asprintf(&$$, "<expr>%s[%s]</expr>", $1, $3); free($1); free($3); }
//      | BITWISE_AND expr %prec ADDRESS_OF
//                           { asprintf(&$$, "<expr>&%s</expr>", $2); free($2); }
//      | TIMES expr %prec DEREFERENCE
//                           { asprintf(&$$, "<expr>*%s</expr>", $2); free($2); }
//      | LEFT_PAREN TYPE_CAST RIGHT_PAREN expr %prec TYPE_CAST
//                           { asprintf(&$$, "<expr>(%s)%s</expr>", $2, $4); free($2); free($4); }
//      | CONSTANT
//                           { asprintf(&$$, "<expr>%d</expr>", $1); }
//      | STRING_LITERAL
//                           { asprintf(&$$, "<expr>%s</expr>", $1); free($1); }
//      | IDENTIFIER
//                           { asprintf(&$$, "<expr>%s</expr>", $1); free($1); }
//      ;


// ifElseStmt : IF LEFT_PAREN expr RIGHT_PAREN compoundStmt
//            | IF LEFT_PAREN expr RIGHT_PAREN compoundStmt ELSE compoundStmt
//            ;

// switchStmt : SWITCH LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE switchClauses RIGHT_BRACE
//            ;

// switchClauses : switchClause
//               | switchClauses switchClause
//               ;

// switchClause : CASE expr COLON stmts
//              | DEFAULT COLON stmts
//              ;

// whileStmt : WHILE LEFT_PAREN expr RIGHT_PAREN stmt
//           | DO stmt WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON 
//           ;

// forStmt : FOR LEFT_PAREN expr SEMICOLON expr SEMICOLON expr RIGHT_PAREN stmt
//         ;

// returnStmt : RETURN expr SEMICOLON
//            | RETURN SEMICOLON
//            ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

int main(void) {
    return yyparse();
}
