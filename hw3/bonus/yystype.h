#ifndef YYSTYPE_H_
#define YYSTYPE_H_

#include <cassert>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <tuple>
#include <algorithm>

// #define YYSTYPE Node*

constexpr int WORD_SIZE = 4;
constexpr int LG_WORD_SIZE = 2;

enum Tag {
    NOTAG,  // should not be assigned tag
    SDEC,   // scalar declaration
    ADEC,   // array declaration
    FDEC,   // function declaration
    FDEF,   // function definition
    EXPR,   // expr
    STMT    // stmt
};

std::string tag2str(Tag t);

// Operators

enum Operator {
    // Function Call
    op_call,
    // Arithmetic
    op_add,
    op_sub,
    op_mul,
    op_div,
    op_neg,
    // Relational
    op_lt,
    op_eq,
    op_neq,
    // Assignment
    op_assign,
    // Pointer & Address
    op_addr,
    op_deref,
    // member access
    op_subscript,
    // ext
    op_extadd,
    op_extsub,
    op_exteq,
    op_extlt
};

std::string get_op_name(Operator op);

// AST Nodes Declaration

struct Node;
struct Type;
struct Declaration;
struct FuncDecl;
struct FuncDefn;
struct MultiDecl;   // int a, b, c;
struct ScalarDecl;
struct ArrayDecl;
struct TranslationUnit;
struct Statement;
struct CompoundStatement;
struct ExpressionStatement;
struct IfStatement;
struct DoStatement;  // do-while
struct WhileStatement; 
struct ForStatement;
struct BreakStatement;
struct ReturnStatement;
struct Expression;
struct UnaryExpression;
struct BinaryExpression;
struct CallExpression;
struct ArraySubscriptExpression;
struct ExtExpression;
struct Identifier;
struct Literal;

template<typename T = Node*> struct NodeList;

// Symbol Table (No type support yet)

enum VarMode {
    M_LOCAL,
    // M_GLOBAL,
    // M_PARAMETER
};

enum DataType {
    T_INT, // int
    T_PTR, // int*
    T_ARR  // int[]
};

std::string get_type_name(DataType type);

struct Symbol {
    std::string name;
    int scope;
    int size;       // number of element (e.g. 1 for scalar, array size for array)
    int offset;     // w.r.t. frame base pointer
    VarMode mode;   // local var or parameters
    DataType type;
};

struct SymbolTable {

    std::vector<Symbol> table;
    int frame_cnt;  // number of stuffs in current stack frame

    int push_stack(int size, int scope) { 
        push("", scope, size, M_LOCAL, T_INT);
        return frame_cnt; 
    }
    void pop_stack(int size) { 
        // size should be set to the size of the last in stack
        // for example 1 to an int, and 5 to a[5]
        if (table.back().size != size) {
            // stack content incorrect
            Symbol* nd = NULL;
            nd->scope = 7122;
        }
        frame_cnt -= size; 
        table.pop_back();
    }
    void push(std::string name, int scope, int size, VarMode mode, DataType type) {
        frame_cnt += size;
        table.emplace_back((Symbol){name, scope, size, frame_cnt * WORD_SIZE, mode, type});
    }
    Symbol* lookup(std::string name) {
        std::vector<Symbol>::reverse_iterator it;
        // search from tail to head
        for (it = table.rbegin(); it != table.rend(); it++) {
            if (it->name == name) break;
        }
        if (it == table.rend()) return nullptr;
        else return &(*it);
    }
    int clear_to_scope(int scope, bool fake_delete = false) {
        int remove_cnt = 0;
        std::vector<Symbol> tmp;
        while (table.size() && table.back().scope > scope) {
            frame_cnt -= table.back().size; 
            remove_cnt += table.back().size;
            tmp.emplace_back(table.back());
            table.pop_back();
        }
        std::reverse(tmp.begin(), tmp.end());
        if (fake_delete) {
            table.insert(table.end(), tmp.begin(), tmp.end());
            frame_cnt += remove_cnt;
        }
        return remove_cnt;
    }
};

struct Scope {
    int scope;

    Scope():scope(0) {}

    int enter() { return ++scope; }
    int leave() { return --scope; }
    int get_scope() { return scope; }
};

// Visitor Declaration

struct Visitor {
    // output file discriptor
    std::ofstream ASM;

    // codegen related member
    SymbolTable symbol_table;
    Scope scope;
    void save_regs_on_stack(std::string whom, std::vector<std::string> &regs);
    void restore_regs_from_stack(std::string whom, std::vector<std::string> &regs);

    // codegen label maintainer
    std::string gen_label(std::string str, int idx) { char buff[30]; sprintf(buff, "_%d", idx); return str+buff; }

    int if_cnt;
    int new_if_label_set() { return ++if_cnt; }
    std::string label_else(int idx) { return gen_label("else", idx); }
    std::string label_end_if(int idx) { return gen_label("endif", idx); }

    int loop_cnt;
    int new_loop_label_set() { return ++loop_cnt; }
    /* loop_start: label where loop starts */
    std::string label_loop_start(int idx) { return gen_label("lstart", idx); }
    /* loop_continue: label where `continue` goes to */
    std::string label_loop_continue(int idx) { return gen_label("lcont", idx); }
    /* loop_cond: label where condition starts*/
    std::string label_loop_cond(int idx) { return gen_label("lcond", idx); }
    /* loop_end: label where `break` goes to */
    std::string label_loop_end(int idx) { return gen_label("lend", idx); }

    // maintain which label should `break` `continue` jump to
    // Note: will break when function is allowed to return in the middle of func body
    // (loop idx, loop scope)
    std::vector<std::tuple<int,int>> loop_stack;
    void enter_loop(int idx, int scope) { loop_stack.emplace_back(idx, scope); }
    void leave_loop() { loop_stack.pop_back(); }
    std::tuple<int,int> get_current_loop() { return loop_stack.empty()? std::make_tuple(-1, -1) : loop_stack.back(); }

    // currently in which function, used for return stmt
    FuncDefn *current_function;
    std::string label_function_end();  

    // constructor & visitor pattern
    Visitor(): ASM("codegen.S"), if_cnt(0), loop_cnt(0), current_function(nullptr) {}

    void visit(Node &);
    void visit(TranslationUnit &);
    void visit(Declaration &);
    void visit(FuncDecl &);
    void visit(FuncDefn &);
    void visit(MultiDecl &);
    void visit(ScalarDecl &);
    void visit(ArrayDecl &);
    void visit(Statement &);
    void visit(CompoundStatement &);
    void visit(ExpressionStatement &);
    void visit(IfStatement &);
    void visit(DoStatement &);
    void visit(WhileStatement &);
    void visit(ForStatement &);
    void visit(BreakStatement &);
    void visit(ReturnStatement &);
    void visit(Expression &);
    void visit(UnaryExpression &);
    void visit(BinaryExpression &);
    void visit(CallExpression &);
    void visit(ArraySubscriptExpression &);
    void visit(Identifier &);
    void visit(Literal &);
};

// AST Nodes Definition
    
enum ValueType { lvalue, rvalue, no_value };

std::string get_value_type_name(ValueType type);

struct CodegenDest {
    enum Dest { reg, mem, no_gen };
    Dest dest;
    ValueType value_type;
    std::string reg_name;
    int mem_offset;  // relative to stack pointer

    bool is_mem() { return dest == mem; }
    void set_mem(int _offset) { dest = mem, mem_offset = _offset; }

    bool is_lvalue() { return value_type == lvalue; }
    bool is_rvalue() { return value_type == rvalue; }
    bool set_lvalue() { return value_type = lvalue; }
    bool set_rvalue() { return value_type = rvalue; }

    CodegenDest():dest(no_gen), value_type(no_value) {}
};

// The very base class

struct Node {
    char *token;    // NULL if non-terminal, assigned in lex
    Tag tag;        // NULL if terminal, assigned in yacc
    Tag hint;       // NULL if non-declaration
    Node* child[15];

    virtual void accept(Visitor &visitor) { visitor.visit(*this); }

    Node();
    Node(char*);    // construct from yytext
    virtual ~Node();
};

std::ostream& operator << (std::ostream&, Node&);

// Recursive list to array

template<typename T>
struct NodeList : public Node {
    std::vector<T> arr;

    void push(T elem) { arr.emplace_back(elem); }
};

// Type class

struct Type : public Node {
    DataType type;

    Type(DataType _type):type(_type) {}
    void add(Type *_type) {
        if (_type->type == T_PTR) type = T_PTR;
        if (_type->type == T_ARR) type = T_ARR;
    }
};

// Translation Unit Class

struct TranslationUnit : public Node {
    std::vector<Node*> decl_func_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    void add_extern_decl(Node *node) { decl_func_list.emplace_back(node); }

    ~TranslationUnit();
};

// Declaration base class

struct Declaration : public Node {
    Type *type; // scalar, atomic element of array, return type of function
    Expression *initializer;

    Declaration():type(nullptr), initializer(nullptr) {}

    Declaration(char *txt):Node(txt), type(nullptr), initializer(nullptr) {}
    virtual ~Declaration();

    void accept(Visitor &visitor) { visitor.visit(*this); }

    virtual void set_type(Type* _type) { 
        if (type == nullptr) type = _type;
        else { type->add(_type); delete _type; }
    }
    DataType get_data_type() { return type->type; }

    void set_initializer(Expression *init) { initializer = init; }
};

// Function Declaration

struct FuncDecl : public Declaration {
    std::vector<Declaration*> parameter_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDecl(char* str, NodeList<Declaration*> *_list = nullptr):Declaration(str) {
        if (_list) std::swap(_list->arr, parameter_list);
    }
    virtual ~FuncDecl();
};

struct FuncDefn : public Declaration {
    FuncDecl *func_decl;
    Statement *func_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    FuncDefn(Type* _type, FuncDecl *decl, Statement *body);
    ~FuncDefn();
};

// variable declaration

struct MultiDecl: public Declaration {
    std::vector<Declaration*> decl_list;

    void set_type(Type *_type) {
        for (auto decl : decl_list) {
            Type *t = new Type(*_type);
            decl->Declaration::set_type(t);
        }
        delete _type;
    }

    void accept(Visitor &visitor) { visitor.visit(*this); }
    MultiDecl(NodeList<Declaration*> *_list) {
        std::swap(_list->arr, decl_list);
    }
    ~MultiDecl();
};

struct ScalarDecl : public Declaration {

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ScalarDecl(char* str):Declaration(str) {}
};


struct ArrayDecl : public Declaration {
    int array_size;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ArrayDecl(char* str, int _size):Declaration(str), array_size(_size) {}
};

// Statement Base Class

struct Statement : public Node {
    bool is_func_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    Statement():is_func_body(false){}
    virtual ~Statement() {}
};

struct CompoundStatement : public Statement {
    std::vector<Node*> stmt_decl_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    CompoundStatement(NodeList<Node*> *_list) {
        if (_list) std::swap(_list->arr, stmt_decl_list);
    }
    ~CompoundStatement();
};

// If Statement (including if-else)
// Note: there is no else-if in spec, forget about it

struct IfStatement : public Statement {
    Expression *cond;
    Statement *if_body, *else_body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    IfStatement(Expression *_cond, Statement *_if, Statement *_else = nullptr) : cond(_cond), if_body(_if), else_body(_else) {}
    ~IfStatement();
};

// Do While Statement

struct DoStatement : public Statement {
    Expression *cond;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    DoStatement(Expression *_cond, Statement *_body) : cond(_cond), body(_body) {}
    ~DoStatement();
};

// While Statement

struct WhileStatement : public Statement {
    Expression *cond;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    WhileStatement(Expression *_cond, Statement *_body) : cond(_cond), body(_body) {}
    ~WhileStatement();
};

// For Statement

struct ForStatement : public Statement {
    Expression *initialize, *condition, *increment;
    Statement *body;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    ForStatement(Expression *init, Expression *cond , Expression *inc, Statement *_body)
        : initialize(init), condition(cond), increment(inc), body(_body) {}
    ~ForStatement();
};

// Expression Statement

struct ExpressionStatement : public Statement {
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ExpressionStatement(Expression *_expr):expr(_expr) {}
    ~ExpressionStatement();
};

// Break Statement

struct BreakStatement : public Statement {
    void accept(Visitor &visitor) { visitor.visit(*this); }
};

struct ReturnStatement : public Statement {
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ReturnStatement(Expression *_expr = nullptr):expr(_expr) {}
    ~ReturnStatement();
};

// Expression

struct Expression : public Node {
    CodegenDest dest;
    DataType return_type;

    Expression():return_type(T_INT) {}

    void accept(Visitor &visitor) { visitor.visit(*this); }
    virtual ~Expression() {}

    void set_save_to_mem(int offset) { dest.set_mem(offset); }
    // lvalue is something you can assign to, i.e. an address
    void set_to_lvalue() { dest.set_lvalue(); }
    // rvalue will be an actual number or string
    void set_to_rvalue() { dest.set_rvalue(); }
};

struct UnaryExpression : public Expression {
    Operator op;
    Expression *expr;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    UnaryExpression(Operator _op, Expression *_expr):op(_op), expr(_expr) {}
    virtual ~UnaryExpression();
};

struct BinaryExpression : public Expression { 
    Operator op;
    Expression *lhs, *rhs;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    BinaryExpression(Operator _op, Expression *_lhs, Expression *_rhs):op(_op), lhs(_lhs), rhs(_rhs) {}
    virtual ~BinaryExpression(); 
};

struct CallExpression : public UnaryExpression {
    std::vector<Expression*> argument_list;

    void accept(Visitor &visitor) { visitor.visit(*this); }
    void set_argument_list(NodeList<Expression*> *nl) { std::swap(nl->arr, argument_list); }

    CallExpression(Expression *_expr, NodeList<Expression*> *nl = nullptr):UnaryExpression(op_call, _expr) {
        if (nl != nullptr) set_argument_list(nl); 
    }
    ~CallExpression();
};

struct ArraySubscriptExpression : public UnaryExpression {
    Expression *subscript;

    void accept(Visitor &visitor) { visitor.visit(*this); }

    ArraySubscriptExpression(Expression *arr, Expression *sub):UnaryExpression(op_subscript, arr), subscript(sub) {}
    ~ArraySubscriptExpression();
};

struct Identifier : public Expression {
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Identifier(char *str);
};

struct Literal : public Expression { 
    void accept(Visitor &visitor) { visitor.visit(*this); }
    Literal(char *str);
};


#endif // YYSTYPE_H_