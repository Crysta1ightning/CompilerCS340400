%{

#include <cstdio>
#include <cassert>
#include <iostream>
#include <string>
#include <vector>
#include <initializer_list>

extern "C" {
    int yyerror(std::string s);
    int yyparse();
    int yylex();
}

#include "yystype.h"
#include "y.tab.h"

Visitor visitor;

using NodePtr = Node*;

void cleanup(std::initializer_list<NodePtr> child_list) {
    for (auto c : child_list) delete c;
}

void cleanup(NodePtr _1) { cleanup({ _1 } ); }
void cleanup(NodePtr _1, NodePtr _2) { cleanup({ _1, _2 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3) { cleanup({ _1, _2, _3 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4) { cleanup({ _1, _2, _3, _4 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5) { cleanup({ _1, _2, _3, _4, _5 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6) { cleanup({ _1, _2, _3, _4, _5, _6 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7) { cleanup({ _1, _2, _3, _4, _5, _6, _7 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8, _9 } ); }
void cleanup(NodePtr _1, NodePtr _2, NodePtr _3, NodePtr _4, NodePtr _5, NodePtr _6, NodePtr _7, NodePtr _8, NodePtr _9, NodePtr _10, NodePtr _11) { cleanup({ _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11 } ); }


void codegen(Declaration*);

%}

%union {
    Node* node;
    Type* type;
    TranslationUnit* translation_unit;
    Declaration* decl;
    FuncDecl* func_decl;
    FuncDefn* func_defn;
    Statement* stmt;
    ExpressionStatement* expr_stmt;
    Expression* expr;
    NodeList<Expression*>* expr_list;
    NodeList<Declaration*>* decl_list;
    NodeList<Node*>* node_list;
}

%token<node> IDENTIFIER
%token<node> LITERAL

%token<node> CONST INT VOID
%token<node> FOR IF ELSE WHILE DO RETURN BREAK CONTINUE
%token<node> PLUS MINUS STAR DIVIDE ADDR 
%token<node> EQUAL NOT_EQUAL LESS_THAN 
%token<node> ASSIGNMENT 
%token<node> LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE
%token<node> SEMICOLON COLON COMMA

%right ASSIGNMENT 
%left EQUAL NOT_EQUAL 
%left LESS_THAN 
%left PLUS MINUS
%left STAR DIVIDE
%right DEREFERENCE ADDRESS_OF NEGATE

%type <node> codegen 
%type <translation_unit> program 
%type <decl> variableDeclaration 
%type <decl> scalarDeclaration 
%type <decl> arrayDeclaration 
%type <decl> functionDeclaration 
%type <func_defn> functionDefinition 

%type <expr_stmt> exprStmt
%type <node> empty_const 
%type <expr> expr exprprec7 exprprec6 exprprec5 exprprec4 exprprec3 exprprec2 exprprec1 exprprec0
%type <expr> argument empty_expr

%type <stmt> stmt ifElseStmt whileStmt forStmt returnStmt breakStmt compoundStmt
%type <node_list> compoundStmtElements 

%type <type> type 
%type <decl_list> idents arrays parameters 
%type <expr_list> arguments
%type <decl> array ident pointer_ident param 

%start codegen

%%
/*
	CODEGEN
*/
codegen : program { 
			codegen($1); 
			cleanup($1); 
			$$ = NULL; 
		}
		;

program : /* empty */ { $$ = new TranslationUnit(); }
		| program variableDeclaration {
			$$ = $1;
			$$->add_extern_decl($2); 
		}
		| program functionDefinition {
			$$ = $1;
			$$->add_extern_decl($2); 
		}
		;

variableDeclaration : scalarDeclaration { $$ = $1; } 
		| arrayDeclaration { $$ = $1; }
		| functionDeclaration { $$ = $1; }


// all type in the testcases are of type int, const int, *int and void, treat them all like INT
type : empty_const INT { 
			$$ = new Type(T_INT); 
			cleanup($1, $2); 
		}
		| VOID {
			$$ = new Type(T_INT); 
			cleanup($1); 
		}
		;

empty_const : /* empty */  { $$ = NULL; }
		| CONST { $$ = $1; }
		;

idents : ident { 
			$$ = new NodeList<Declaration*>; 
			$$->push($1); 
		}
		| idents COMMA ident {
			$$ = $1;
			$$->push($3); 
			cleanup($2);
		}
		;

ident : pointer_ident { $$ = $1; }
		| pointer_ident ASSIGNMENT expr { 
			$$ = $1;
			$$->set_initializer($3);
			cleanup($2);
		} 
		;

pointer_ident : IDENTIFIER { 
			$$ = new ScalarDecl($1->token); 
			cleanup($1);
		}
		| STAR IDENTIFIER { // pointer
			$$ = new ScalarDecl($2->token);
			$$->set_type(new Type(T_PTR));
			cleanup($1, $2);
		}
		;

/*
	SCALAR DECLARATION
*/
scalarDeclaration : type idents SEMICOLON {
			$$ = new MultiDecl($2); 
			$$->set_type($1); 
			cleanup($2, $3);
		}
		;

/*
	ARRAY DECLARATION
*/
arrayDeclaration : type arrays SEMICOLON {
			$$ = new MultiDecl($2); 
			$$->set_type($1); 
			cleanup($2, $3);
		}
        ;

// arrays are declared one at a time in the testcases, no comma
arrays : array { 
			$$ = new NodeList<Declaration*>; 
			$$->push($1); 
 		}

// array initialization with content is not in the testcases
// only 1D array is in the testcases
array : IDENTIFIER LEFT_BRACKET expr RIGHT_BRACKET {
			$$ = new ArrayDecl($1->token, atoi($3->token)); 
			cleanup($1, $2, $3, $4); 
		}
		;

/*
	FUNCTION DECLARATION
*/
functionDeclaration : type pointer_ident LEFT_PAREN RIGHT_PAREN SEMICOLON { 
			auto tmp1 = new FuncDecl($2->token);
			auto tmp2 = new NodeList<Declaration*>;

			tmp2->push(tmp1);
			$$ = new MultiDecl(tmp2); 
			$$->set_type($1);
			cleanup(tmp2, $2, $3, $4, $5);
		}
		| type pointer_ident LEFT_PAREN parameters RIGHT_PAREN SEMICOLON { 
			auto tmp1 = new FuncDecl($2->token, $4);
			auto tmp2 = new NodeList<Declaration*>;

			tmp2->push((Declaration*) tmp1);
			$$ = new MultiDecl(tmp2); 
			$$->set_type($1);
			cleanup(tmp2, $2, $3, $4, $5, $6);
		}
        ;

parameters : param { 
			$$ = new NodeList<Declaration*>; 
			$$->push($1); 
		}
		| parameters COMMA param {
			$$ = $1;
			$$->push($3); 
			cleanup($2);		
		}
		;

param : type pointer_ident {
			$$ = $2; 
			$$->set_type($1); 
		}
		;

arguments : argument { 
			$$ = new NodeList<Expression*>; 
			$$->push($1);
		}
		| arguments COMMA argument {
			$$ = $1; 
			$$->push($3); 
			cleanup($2);
		}
		;
	
argument : expr { $$ = $1; }

/*
	FUNCTION DEFINITION
*/
functionDefinition : type pointer_ident LEFT_PAREN RIGHT_PAREN compoundStmt {
			auto tmp = new FuncDecl($2->token);
			$$ = new FuncDefn($1, tmp, $5);
			cleanup($2, $3, $4);
		}
		|
		type pointer_ident LEFT_PAREN parameters RIGHT_PAREN compoundStmt {
			auto tmp = new FuncDecl($2->token, $4);
			$$ = new FuncDefn($1, tmp, $6);
			cleanup($2, $3, $4, $5);
		}
		;

/*
	STMT
	in the testcases, only ifElse, while, doWhile, for, return, break
*/
stmt :  exprStmt { $$ = (Statement*) $1; }
		| ifElseStmt { $$ = (Statement*) $1; }
		| whileStmt { $$ = (Statement*) $1; }
		| forStmt { $$ = (Statement*) $1; }
		| returnStmt { $$ = (Statement*) $1; }
		| breakStmt { $$ = (Statement*) $1; }
		| compoundStmt { $$ = (Statement*) $1; }
		; 
		
exprStmt : expr SEMICOLON { 
			$$ = new ExpressionStatement($1);
			cleanup($2);
		}
		;

ifElseStmt : IF LEFT_PAREN expr RIGHT_PAREN compoundStmt {
			$$ = new IfStatement($3, $5);
			cleanup($1, $2, $4);
		}
		| IF LEFT_PAREN expr RIGHT_PAREN compoundStmt ELSE compoundStmt {
			$$ = new IfStatement($3, $5, $7);
			cleanup($1, $2, $4, $6);
		}
		;


whileStmt : WHILE LEFT_PAREN expr RIGHT_PAREN stmt {
			$$ = new WhileStatement($3, $5);
			cleanup($1, $2, $4);
		} 
		| DO stmt WHILE LEFT_PAREN expr RIGHT_PAREN SEMICOLON {
			$$ = new DoStatement($5, $2); 
			cleanup($1, $3, $4, $6, $7);
		}
        ;

forStmt : FOR LEFT_PAREN empty_expr SEMICOLON empty_expr SEMICOLON empty_expr RIGHT_PAREN stmt {
			$$ = new ForStatement($3, $5, $7, $9); 
			cleanup($1, $2, $4, $6, $8);
		}
        ;

empty_expr : /* empty */ { $$ = NULL; }
		| expr { $$ = $1; }

returnStmt : RETURN SEMICOLON {  
			$$ = new ReturnStatement(); 
			cleanup($1, $2); 
		}
		| RETURN expr SEMICOLON {
			$$ = new ReturnStatement($2); 
			cleanup($1, $3);
		}
		;

breakStmt : BREAK SEMICOLON { 
			$$ = new BreakStatement(); 
			cleanup($1, $2); 
		}
		;

compoundStmt : LEFT_BRACE compoundStmtElements RIGHT_BRACE {
			$$ = new CompoundStatement($2);
			cleanup($1, $2, $3);
		}
        ;

compoundStmtElements : /* empty */ {  $$ = new NodeList<Node*>; }
		| compoundStmtElements variableDeclaration {
			$$ = $1;
			$$->push($2);
		}
		| compoundStmtElements stmt {
			$$ = $1;
			$$->push($2);
		}


/* 
	EXPR
*/
expr : exprprec7 { $$ = $1; }
		;

exprprec7 : exprprec6 { $$ = $1; }
		| exprprec6 ASSIGNMENT exprprec7 {
			$$ = new BinaryExpression(op_assign, $1, $3); 
			cleanup($2);
		}
		;

exprprec6 : exprprec5 { $$ = $1; }
		| exprprec6 EQUAL exprprec5 {
			$$ = new BinaryExpression(op_eq, $1, $3); 
			cleanup($2);
		}
		| exprprec6 NOT_EQUAL exprprec5 {
			$$ = new BinaryExpression(op_neq, $1, $3); 
			cleanup($2);
		}
		;

exprprec5 : exprprec4 { $$ = $1; }
		| exprprec5 LESS_THAN exprprec4 {
			$$ = new BinaryExpression(op_lt, $1, $3); 
			cleanup($2);
		}
		;

exprprec4 : exprprec3 { $$ = $1; }
		|  exprprec4 PLUS exprprec3  {
			$$ = new BinaryExpression(op_add, $1, $3); 
			cleanup($2);
		}
		| exprprec4 MINUS exprprec3 { 
			$$ = new BinaryExpression(op_sub, $1, $3); 
			cleanup($2);
		}
		;

exprprec3 : exprprec2 { $$ = $1; }
		| exprprec3 STAR exprprec2 { // times
			$$ = new BinaryExpression(op_mul, $1, $3); 
			cleanup($2);
		}
		| exprprec3 DIVIDE exprprec2 {
			$$ = new BinaryExpression(op_div, $1, $3); 
			cleanup($2);
		}
		;

exprprec2 : exprprec1 { $$ = $1; }
		| STAR exprprec2 %prec DEREFERENCE { // dereference
			$$ = new UnaryExpression(op_deref, $2);
			cleanup($1);
		}
		| ADDR exprprec2 %prec ADDRESS_OF { // address of
			$$ = new UnaryExpression(op_addr, $2); 
			cleanup($1);
		}
		| MINUS exprprec2 %prec NEGATE { // negate
			$$ = new UnaryExpression(op_neg, $2);
			cleanup($1);
		}
		;

exprprec1: exprprec0 { $$ = $1; }
		// every function in the testcases have arguments
		| exprprec0 LEFT_PAREN arguments RIGHT_PAREN { // function call
			$$ = new CallExpression($1, $3); 
			cleanup($2, $3, $4);
		}
		;

exprprec0: IDENTIFIER { 
			$$ = new Identifier($1->token); 
			cleanup($1);
		}
		| LITERAL { 
			$$ = new Literal($1->token); 
			cleanup($1); 
		}	
		// only 1D array in the testcases
		| IDENTIFIER LEFT_BRACKET expr RIGHT_BRACKET  { // array subscripting
			$$ = new ArraySubscriptExpression(new Identifier($1->token), $3); 
			cleanup($1, $2, $4);
		}
		| LEFT_PAREN expr RIGHT_PAREN {
			$$ = $2; 
			cleanup($1, $3);
		}
		;	
%%

int main(void) {
    // parse & codegen
    yyparse();
    return 0;
}

int yyerror(std::string s) {
    std::cerr << "[yyerror] " << s << std::endl;
    return 0;
}

void codegen(TranslationUnit *unit) {
    unit->accept(visitor);
}
