#include <string>
#include <iostream>
#include <cstring>
#include <cassert>
#include "yystype.h"

// riscv registers
std::vector<std::string> arg_regs { "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };
std::vector<std::string> caller_preserved_registers { 
    "ra", "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7" };
std::vector<std::string> callee_preserved_registers {
    "sp", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11" };

// utils
std::string get_type_name(DataType type) {
    switch (type) {
    case T_INT: return "int";
    case T_PTR: return "int*";
    case T_ARR: return "int[]";
    }
    std::cerr << "unrecognized data type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid data type");
    return "unreachable";
}

std::string get_value_type_name(ValueType type) {
    switch (type) {
    case lvalue: return "lvalue";
    case rvalue: return "rvalue";
    case no_value: return "no_value";
    }
    std::cerr << "unrecognized value type: " << static_cast<int>(type) << std::endl;
    assert (false && "invalid value type");
    return "unreachable";
}

void Visitor::save_regs_on_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " <saves registers>" << std::endl;
    ASM << "  addi sp, sp, " << -WORD_SIZE * (int)regs.size() << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  sw " << reg << ", " << WORD_SIZE * i << "(sp)" << std::endl;
    }
    ASM << "  // <saves registers/>" << std::endl;
}

void Visitor::restore_regs_from_stack(std::string whom, std::vector<std::string> &regs) {
    ASM << "  // " << whom << " <restores registers>" << std::endl;
    for (size_t i = 0; i < regs.size(); i++) {
        auto &reg = regs[i];
        ASM << "  lw " << reg << ", " << WORD_SIZE * i << "(sp)" << std::endl;
    }
    ASM << "  addi sp, sp, " << WORD_SIZE * (int)regs.size() << std::endl;
    ASM << "  // <restores registers>" << std::endl;
}

std::string Visitor::label_function_end() {
    std::string label = current_function->func_decl->token;
    label += "_end";
    return label;
}

// Node Class

Node::Node():token(NULL), tag(NOTAG), hint(NOTAG) {
    memset(child, 0, sizeof(child));
}

Node::Node(char *yytext):token(NULL), tag(NOTAG), hint(NOTAG) {
    int buffsz = strlen(yytext) + 1; // note the '\0'
    token = new char[buffsz];
    memcpy(token, yytext, buffsz);
    memset(child, 0, sizeof(child));
}

Node::~Node() {
    if (token != NULL) delete[] token;
}

std::ostream &operator << (std::ostream &out, Node &nd) {
    if (nd.token != NULL) {
        out << nd.token;
    }
    return out;
}

TranslationUnit::~TranslationUnit() {
    for (auto x : decl_func_list) delete x;
}

Declaration::~Declaration() {
    delete type;
    if (initializer) delete initializer;
}

FuncDecl::~FuncDecl() {
    for (auto x : parameter_list) delete x;
}

MultiDecl::~MultiDecl() {
    for (auto x : decl_list) delete x;
}

FuncDefn::FuncDefn(Type* _type, FuncDecl *decl, Statement *body):func_decl(decl), func_body(body) { 
    func_decl->set_type(_type); 
    func_body->is_func_body = true;
}

FuncDefn::~FuncDefn() {
    delete func_decl;
    delete func_body;
}

CompoundStatement::~CompoundStatement() {
    for (auto x : stmt_decl_list) delete x;
}

IfStatement::~IfStatement() {
    delete cond;
    delete if_body;
    if (else_body) delete else_body;
}

DoStatement::~DoStatement() {
    delete cond;
    delete body;
}

WhileStatement::~WhileStatement() {
    delete cond;
    delete body;
}

ForStatement::~ForStatement() {
    delete initialize;
    delete condition;
    delete increment;
    delete body;
}

ExpressionStatement::~ExpressionStatement() {
    delete expr;
}

ReturnStatement::~ReturnStatement() {
    if (expr) delete expr;
}

UnaryExpression::~UnaryExpression() {
    delete expr;
}

BinaryExpression::~BinaryExpression() {
    delete lhs;
    delete rhs;
}

CallExpression::~CallExpression() {
    for (auto x : argument_list) delete x;
}

ArraySubscriptExpression::~ArraySubscriptExpression() {
    delete subscript;
}

Identifier::Identifier(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}

Literal::Literal(char *str) {
    token = new char[strlen(str)+1];
    memcpy(token, str, strlen(str)+1);
}


void Visitor::visit(Node &node) {
}

void Visitor::visit(TranslationUnit &unit) {
    for (auto x : unit.decl_func_list) {
        x->accept(*this);
    }
}

void Visitor::visit(Declaration &decl) {
    // will not be called
}

void Visitor::visit(FuncDecl &decl) {
    // do nothing
}

void Visitor::visit(FuncDefn &defn) {
    current_function = &defn;

    // 1. create header
    ASM << ".global " << defn.func_decl->token << std::endl;
    ASM << defn.func_decl->token << ":" << std::endl;

    // 2. store callee preserved registers
    save_regs_on_stack("callee", callee_preserved_registers);
    symbol_table.push_stack(callee_preserved_registers.size(), scope.get_scope());
    // 3. set new frame
    ASM << "  addi s0, sp, " << callee_preserved_registers.size()*WORD_SIZE << " // save preserved reg" << std::endl;

    // 4. save parameters as local variables
    int arg_n = defn.func_decl->parameter_list.size();
    if (arg_n > 0) {
        ASM << "  addi sp, sp, " << -arg_n*WORD_SIZE << " // save parameters for: " << defn.func_decl->token << std::endl;
        for (int i = 0; i < arg_n; i++) {
            Declaration *decl = defn.func_decl->parameter_list[i];
            symbol_table.push(decl->token, scope.get_scope(), 1, M_LOCAL, decl->get_data_type());
            Symbol *sym = symbol_table.lookup(decl->token);
            ASM << "  sw " << arg_regs[i] << ", " << -sym->offset << "(fp)" << std::endl;
        }
    }

    // 5. deal with the body
    defn.func_body->accept(*this);

    // 6. release parameters 
    if (arg_n > 0) {
        ASM << "  addi sp, sp, " << arg_n*WORD_SIZE << " // release parameters" << std::endl;
        for (int i = 0; i < arg_n; i++) symbol_table.pop_stack(1);
    }

    // 7. release local variables
    int removed_local_cnt = symbol_table.clear_to_scope(scope.get_scope());
    if (removed_local_cnt > 0) {
        ASM << "  addi sp, sp, " << removed_local_cnt * WORD_SIZE << " // release local variable" << std::endl;
    }

    // 8. restore callee preserved registers
    ASM << "  addi sp, sp, " << WORD_SIZE * (symbol_table.frame_cnt - callee_preserved_registers.size()) << " // release preserved reg" << std::endl;

    restore_regs_from_stack("callee", callee_preserved_registers);
    symbol_table.pop_stack(callee_preserved_registers.size());

    current_function = nullptr;

    // 9. return
    ASM << "  jalr x0, 0(ra)" << " // return" <<std::endl;
}

void Visitor::visit(MultiDecl &decl) {
    // go through every declaration (scalarDeclarations or arrayDeclarations or funcDeclarations)
    for (auto x : decl.decl_list) x->accept(*this);
}

void Visitor::visit(ScalarDecl &decl) {
    // 1. push into symbol table
    symbol_table.push(decl.token, scope.get_scope(), 1, M_LOCAL, decl.get_data_type());
    Symbol *sym = symbol_table.lookup(decl.token);
    if (sym == nullptr) {
        std::cerr << "symbol not found: " << decl.token << std::endl; 
    }

    // 2. make space for new IDENTIFIER
    ASM << "  addi sp, sp, " << -WORD_SIZE << " // Scalar declaration of: " << decl.token << std::endl;

    // 3. initialize it
    if (decl.initializer != nullptr) {
        decl.initializer->set_save_to_mem(sym->offset);
        decl.initializer->set_to_rvalue();
        decl.initializer->accept(*this); 
    }
}

void Visitor::visit(ArrayDecl &decl) {
    // 1. push into symbol table
    symbol_table.push(decl.token, scope.get_scope(), decl.array_size, M_LOCAL, T_ARR); 
    Symbol *sym = symbol_table.lookup(decl.token);
    if (sym == nullptr) {
        std::cerr << "symbol not found: " << decl.token << std::endl; 
    }

    // 2. make space for new IDENTIFIER[]
    ASM << "  addi sp, sp, " << -decl.array_size * WORD_SIZE << " // Array declaration of: " <<  decl.token << "[" << decl.array_size << "]" << std::endl;

    // no need to handle initialize
}

void Visitor::visit(Statement &stmt) {
    // will not be called
}

void Visitor::visit(CompoundStatement &stmt) {
    int backup = scope.get_scope();

    // 1. change the scope
    scope.enter();

    // 2. deal with the body
    for (auto c : stmt.stmt_decl_list) c->accept(*this);

    // 3. change the scope back
    int recovered_scope = scope.leave();

    // 4. release variables introduced in the scope
    int removed_local_cnt = symbol_table.clear_to_scope(recovered_scope);
    ASM << "  addi sp, sp, " << removed_local_cnt * WORD_SIZE << " // release local variable" << std::endl;

    // 5. if this compound statment is in func declaration, add a label for end so return can jump to
    if (stmt.is_func_body) {
        ASM << label_function_end() << ":" << std::endl;
    }
}

void Visitor::visit(IfStatement &if_stmt) {
    ASM << "  // <IfStatement>" << std::endl;

    int if_idx = new_if_label_set();

    // 1. allocate temp for the result of condition
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    if_stmt.cond->set_save_to_mem(cond_offset);
    if_stmt.cond->set_to_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << "// for result of condition of IfStatement" << std::endl;

    // 2. deal with condition
    if_stmt.cond->accept(*this);

    // 3. branch if condition
    ASM << "  lw t0, " << -cond_offset << "(fp)" << std::endl; // t0 = cond
    ASM << "  beqz t0, " << label_else(if_idx) << std::endl;  // !cond then goto else

    // 4. deal with if body
    if_stmt.if_body->accept(*this);

    // 5. if there is else, don't run the code in else, jump over
    ASM << "  j " << label_end_if(if_idx) << std::endl;

    // 6. deal with else body
    ASM << label_else(if_idx) << ":" << std::endl;
    if (if_stmt.else_body) if_stmt.else_body->accept(*this);
    
    // 7. create a label so after if is finished, jump over else
    ASM << label_end_if(if_idx) << ":" << std::endl;

    // 8. release condition
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << "// release result of condition of IfStatement" << std::endl;

    ASM << "  // <IfStatement/>" << std::endl;
}

void Visitor::visit(DoStatement &do_stmt) {
    ASM << "  // <DoStatement>" << std::endl;

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // 1. allocate temp for the result of condition
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    do_stmt.cond->set_save_to_mem(cond_offset);
    do_stmt.cond->set_to_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << "// for result of condition of DoStatement" << std::endl;

    // 2. deal with body
    ASM << label_loop_start(loop_idx) << ":" << std::endl;
    do_stmt.body->accept(*this);

    // 3. deal with condition
    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    do_stmt.cond->accept(*this);
    
    // 4. branch to the start of the loop if condition passed
    ASM << "  lw t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  bnez t0, " << label_loop_start(loop_idx) << std::endl;

    leave_loop();
    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // 5. release result of condition
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << "// release result of condition of DoStatement" << std::endl;

    ASM << "  // <DoStatement/>" << std::endl;
}

void Visitor::visit(WhileStatement &while_stmt) {
    ASM << "  // <WhileStatement>" << std::endl;

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // 1. allocate temp for the result of condition
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    while_stmt.cond->set_save_to_mem(cond_offset);
    while_stmt.cond->set_to_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // 2. deal with condition
    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    while_stmt.cond->accept(*this);
    ASM << "  lw t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  beqz t0, " << label_loop_end(loop_idx) << std::endl;

    // 3. deal with body
    while_stmt.body->accept(*this);
    ASM << "  j " << label_loop_continue(loop_idx) << std::endl;

    leave_loop();
    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // 4. release result of condition
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <WhileStatement/>" << std::endl;
}

void Visitor::visit(ForStatement &for_stmt) {
    ASM << "  // <ForStatement>" << std::endl;
    // for(a; b; c) {}

    int loop_idx = new_loop_label_set();
    enter_loop(loop_idx, scope.get_scope());

    // 1. allocate temp for the result of condition
    int cond_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    for_stmt.condition->set_save_to_mem(cond_offset);
    for_stmt.condition->set_to_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << " // for the condition of ForStatement" << std::endl;

    // 2. deal with initialize: a
    for_stmt.initialize->accept(*this);

    // 3. deal with condition: b
    ASM << label_loop_cond(loop_idx) << ":" << std::endl;
    for_stmt.condition->accept(*this);
    ASM << "  lw t0, " << -cond_offset << "(fp)" << std::endl;
    ASM << "  beqz t0, " << label_loop_end(loop_idx) << std::endl;

    // 4. deal with body
    for_stmt.body->accept(*this);

    // 5. deal with increment: c
    ASM << label_loop_continue(loop_idx) << ":" << std::endl;
    for_stmt.increment->accept(*this);
    ASM << "  j " << label_loop_cond(loop_idx) << std::endl;

    leave_loop();

    ASM << label_loop_end(loop_idx) << ":" << std::endl;

    // 6. release result of condition
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <ForStatement/>" << std::endl;
}

void Visitor::visit(ExpressionStatement &expr_stmt) {
    expr_stmt.expr->accept(*this);
}

void Visitor::visit(BreakStatement &break_stmt) {
    ASM << "  // <BreakStatement>" << std::endl;

    // 1. break from last entered loop
    auto [loop_idx, loop_scope] = loop_stack.back();

    // 2. release all variables declared in the scope
    int released_cnt = symbol_table.clear_to_scope(loop_scope, true);  
    if (released_cnt > 0) {
            ASM << "  addi sp, sp, " << released_cnt * WORD_SIZE << std::endl;
    }

    // 3. jump to the end of the loop
    ASM << "  j " << label_loop_end(loop_idx) << std::endl;

    ASM << "  // <BreakStatement/>" << std::endl;
}

void Visitor::visit(ReturnStatement &return_stmt) {
    ASM << "  // <ReturnStatement>" << std::endl;

    // 1. allocate temp to store return result
    int tmp_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    return_stmt.expr->set_save_to_mem(tmp_offset);
    return_stmt.expr->set_to_rvalue();
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;
    
    // 2. deal with expression
    if (return_stmt.expr) return_stmt.expr->accept(*this);

    // 3. store return result
    ASM << "  lw a0, " << -tmp_offset << "(fp)" << std::endl;

    // 4. release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    // 5. release all the variables declared in the whole function
    int released_cnt = symbol_table.clear_to_scope(0, true);  
    if (released_cnt > 0) {
        ASM << "  addi sp, sp, " << released_cnt * WORD_SIZE << std::endl;
    }
   
    // 6. jump to the end of the function
    ASM << "  j " << label_function_end() << std::endl;
    ASM << "  // <ReturnStatement/>" << std::endl;
}

void Visitor::visit(Expression &expr) {
    // will not be called
}

void Visitor::visit(UnaryExpression &expr) {
    ASM << "  // <UnaryExpression>" << std::endl;

    // 1. allocate temp for the result
    int tmp_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    expr.expr->set_save_to_mem(tmp_offset);
    ASM << "  addi sp, sp, " << -WORD_SIZE << std::endl;

    // 2. set value type
    if (expr.op == op_addr) {
        // for &, we return the address, so it is a lvalue
        expr.expr->set_to_lvalue();
    } else {
        // for - and *, we return a value, so it is a rvalue
        expr.expr->set_to_rvalue();
    }

    // 3. deal with expression
    expr.expr->accept(*this);

    // 4. get address of result of expression
    ASM << "  lw t0, " << -tmp_offset << "(fp)" << " // get address " << std::endl;

    // 5. deal with different
    switch (expr.op) {
        case op_addr: 
            break;  // t0 is already address of the variable
        case op_deref: 
            if (expr.dest.is_rvalue()) { // if it is rvalue, we cannot simply return the address, so dereference it
                ASM << "  lw t0, 0(t0)" << " // dereference it" << std::endl;
            }
            break;  // t0 is now the address of the variable that is pointed to
        case op_neg: 
            ASM << "  sub t0, x0, t0" << std::endl; 
            break;
        default: 
            std::cerr << "unsupported operator" << std::endl; 
    }


    // 6. store result
    ASM << "  sw t0, " << -expr.dest.mem_offset << "(fp)" << " // store: value of UnaryExpression " << std::endl;
    

    // 7. release temp
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << WORD_SIZE << std::endl;

    ASM << "  // <UnaryExpression/>" << std::endl;
}

void Visitor::visit(BinaryExpression &expr) {
    ASM << "  // <Binary Expression>" << std::endl;

    // 1. allocate temp for the left and right, the result is stored in left
    int lhs_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    int rhs_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE; 
    expr.lhs->set_save_to_mem(lhs_offset);
    expr.rhs->set_save_to_mem(rhs_offset);
    ASM << "  addi sp, sp, " << -2*WORD_SIZE << " // for left and right Expression " <<std::endl;

    // 2. if assign, set lr else rr
    if (expr.op == op_assign) {
        expr.lhs->set_to_lvalue();
        expr.rhs->set_to_rvalue();
    } else {
        expr.lhs->set_to_rvalue();
        expr.rhs->set_to_rvalue();
    }

    // 3. deal with left and right expressions
    expr.lhs->accept(*this);
    expr.rhs->accept(*this);
    
    // 4. set return type
    switch (expr.op) {
        case op_add: case op_sub: // might use x + i to represent x[i]
            expr.return_type = (expr.lhs->return_type == T_PTR || expr.rhs->return_type == T_PTR? T_PTR : T_INT);
            break;
        case op_mul: case op_div: case op_lt: case op_eq: case op_neq: case op_extadd: case op_extsub: case op_exteq: case op_extlt:
            expr.return_type = T_INT;
            break;
        case op_assign:
            expr.return_type = expr.lhs->return_type;
            break;
        default:
            std::cerr << "unsupported operator " << std::endl; 
    }

    // 5. load lhs, rhs result into registers
    ASM << "  lw t0, " << -lhs_offset << "(fp)" << std::endl;
    ASM << "  lw t1, " << -rhs_offset << "(fp)" << std::endl;

    // 6. deal with different operator
    if (expr.op == op_assign) {
        ASM << "  sw t1, 0(t0)" << " // =" << std::endl;

        // cast to rvalue if needed
        if (expr.dest.is_rvalue()) ASM << "  addi t0, t1, 0" << std::endl;
    } else if (expr.return_type == T_INT) {  // arithmetic
        switch (expr.op) {
        case op_add: ASM << "  add t0, t0, t1" << std::endl; break;
        case op_sub: ASM << "  sub t0, t0, t1" << std::endl; break;
        case op_mul: ASM << "  mul t0, t0, t1" << std::endl; break;
        case op_div: ASM << "  div t0, t0, t1" << std::endl; break;
        case op_lt: ASM << "  slt t0, t0, t1" << " // <" <<std::endl; break;
        case op_eq: 
            ASM << "  sub t0, t0, t1" << " // ==" << std::endl;
            ASM << "  seqz t0, t0" << " // ==" << std::endl;
            break;
        case op_neq:
            ASM << "  sub t0, t0, t1" << " // !=" << std::endl;
            ASM << "  snez t0, t0" << " // !=" <<std::endl;
            break;
        case op_extadd:
            ASM << "  ukadd8 t0, t0, t1" << std::endl;
            break;
        case op_extsub:
            ASM << "  uksub8 t0, t0, t1" << std::endl;
            break;
        case op_exteq:
            ASM << "  cmpeq8 t0, t0, t1" << std::endl;
            break;
        case op_extlt:
            ASM << "  ucmplt8 t0, t0, t1" << std::endl;
            break;
        default: 
            std::cerr << "unsupported operator" << std::endl; 
        }
    } else {  // pointer int + -
        std::string ptr_reg = (expr.lhs->return_type == T_PTR? "t0" : "t1");
        std::string int_reg = (expr.lhs->return_type == T_INT? "t0" : "t1");

        ASM << "  slli " << int_reg << ", " << int_reg << ", " << LG_WORD_SIZE << " // " << int_reg << " * (2^" << LG_WORD_SIZE << ")" << std::endl;
        
        // We store arr[5] from sp-20 to sp-0, so to access arr[0], it is sp-20
        switch (expr.op) {
        case op_add: ASM << "  add t0, " << ptr_reg << ", " << int_reg << std::endl; break;
        case op_sub: ASM << "  sub t0, " << ptr_reg << ", " << int_reg << std::endl; break;
        default: 
            std::cerr << "unsupported operator" << std::endl; 
            assert(false && "unsupported binary operator (codegen)");
        }
    }

    // 6. return value
    if (expr.dest.is_mem()) {
        ASM << "  sw t0, " << -expr.dest.mem_offset << "(fp)" << " // store: value of BinaryExpression" << std::endl;
    }

    // 7. release temp
    symbol_table.pop_stack(1);
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << 2*WORD_SIZE << " // release left and right Expression" << std::endl;

    ASM << "  // <BinaryExpression/>" << std::endl;
}

void Visitor::visit(CallExpression &expr) {
    expr.return_type = T_INT;

    ASM << "  // <CallExpression>" << std::endl;

    // 1. allocate temp for the arguments
    int args_n = expr.argument_list.size();
    int args_offset = symbol_table.push_stack(args_n, scope.get_scope()) * WORD_SIZE;
    ASM << "  addi sp, sp, " << -args_n*WORD_SIZE << std::endl;

    // 2. deal with arguments
    for (int i = 0; i < args_n; i++) {
        auto &arg = expr.argument_list[i];
        arg->set_save_to_mem(args_offset + i * WORD_SIZE); 
        arg->set_to_rvalue(); 
        arg->accept(*this);
    }

    // 3. deal with function call
    expr.expr->accept(*this);

    // 4. load arguments into reg
    for (int i = 0; i < args_n; i++) {
        ASM << "  lw " << arg_regs[i] << ", " << -(args_offset + i * WORD_SIZE) << "(fp)" << std::endl;
    }

    // 5. store caller preserved registers
    save_regs_on_stack("caller", caller_preserved_registers);

    // 6. jumps to the function
    ASM << "  jal ra, " << expr.expr->token << std::endl;

    // 7. store return value
    if (expr.dest.is_mem()) { // might not need to return
        ASM << "  sw a0, " << -expr.dest.mem_offset << "(fp)" << " // store: return value of CallExpression" << std::endl;
    }

    // 8. restore caller preserved registers
    restore_regs_from_stack("caller", caller_preserved_registers);

    // 9. release temp
    symbol_table.pop_stack(args_n);
    ASM << "  addi sp, sp, " << args_n*WORD_SIZE << std::endl;

    ASM << "  // <CallExpression/>" << std::endl;
}

void Visitor::visit(ArraySubscriptExpression &expr) {
    expr.return_type = T_INT;

    ASM << "  // <ArraySubscriptExpression>" << std::endl;

    // 3. allocate temp for array and subscript
    int expr_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE;
    int subscript_offset = symbol_table.push_stack(1, scope.get_scope()) * WORD_SIZE; 
    expr.expr->set_save_to_mem(expr_offset);
    expr.subscript->set_save_to_mem(subscript_offset);
    ASM << "  addi sp, sp, " << -2*WORD_SIZE << " // for Array and SubscriptExpression" << std::endl;

    expr.expr->set_to_rvalue();
    expr.subscript->set_to_rvalue();

    // 4. deal with array and subscript
    expr.expr->accept(*this);
    expr.subscript->accept(*this);

    // 5. load into register
    ASM << "  lw t0, " << -expr_offset << "(fp)" << std::endl;
    ASM << "  lw t1, " << -subscript_offset << "(fp)" << std::endl;

    // 6. find the correct address to access
    // for a[3], t1 = 3*4, means need to move 12 bytes from start
    ASM << "  slli t1, t1, " << LG_WORD_SIZE << " // t1 * (2^" << LG_WORD_SIZE << ")" << std::endl; 
    ASM << "  add t0, t0, t1" << std::endl;

    if (expr.dest.is_rvalue()) ASM << "  lw t0, 0(t0)" << " // get value from array" << std::endl;

    // 7. store result
    ASM << "  sw t0, " << -expr.dest.mem_offset << "(fp)" << std::endl;
    
    // 8. release temp
    symbol_table.pop_stack(1);
    symbol_table.pop_stack(1);
    ASM << "  addi sp, sp, " << 2*WORD_SIZE << std::endl;

    ASM << "  // <ArraySubscriptExpression/>" << std::endl;
}

void Visitor::visit(Identifier &id) {
    Symbol *sym = symbol_table.lookup(id.token);
    if (sym == nullptr) return;  // probably it is id of function
    id.return_type = (sym->type == T_ARR? T_PTR : sym->type);

    int offset = sym->offset;

    ASM << "  // <Identifier>" << std::endl;

    // 1. load value, either a addr (lvalue) or a value (rvalue)
    if (sym->type == T_ARR) {
        // array type can only be r_value, return base address
        ASM << "  addi t0, fp, " << -offset << " // Array: " << id.token << std::endl;
    } else if (id.dest.is_lvalue()) {
        ASM << "  addi t0, fp, " << -offset << " // lvalue: " << id.token << std::endl;
    } else if (id.dest.is_rvalue()) {
        ASM << "  lw t0, " << -offset << "(fp)" << " // rvalue: " << id.token << std::endl;
    }

    // 2. store IDENTIFIER
    ASM << "  sw t0, " << -id.dest.mem_offset << "(fp)" << " // store: value of IDENTIFIER: " << id.token << std::endl;
    
    ASM << "  // <Identifier/>" << std::endl;
}

void Visitor::visit(Literal &lit) {
    lit.return_type = T_INT;
    ASM << "  // <Literal>" << std::endl;

    // 1. store literal
    ASM << "  li t0, " << lit.token << std::endl;
    ASM << "  sw t0, " << -lit.dest.mem_offset << "(fp)" << " // store: value of LITERAL" << std::endl;

    ASM << "  // <Literal/>" << std::endl;
}
