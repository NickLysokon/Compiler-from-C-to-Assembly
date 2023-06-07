#ifndef COMPI_HW5_GENERATOR_H
#define COMPI_HW5_GENERATOR_H

#include <string>
#include "types.h"
#include "bp.hpp"

using std::string;


class Generator{
    int register_max_num;
    int label_max_num;
public:
    Generator(): register_max_num(0), label_max_num(0){}

    std::string allocate_register();
    std::string allocate_global_register();
    string allocate_label(string label);
    void generate_global_code();
    string generate_load_var(string rbp, int offset);
    void generate_store_var(string rbp, int offset, string reg);
    void binop_code(Exp* res, const Exp& operand1, const Exp& operand2, const string& op);
    void relop_code(Exp* res, const Exp* operand1, const Exp* operand2, const string& op);
    void bool_eval_code(Exp* res, const Exp* operand1, const Exp* operand2, const string& op, const string& label);
    void assign_code(Exp* exp, int offset, bool is_bool);
    void return_code(string& type, string& reg);
    void return_value(string& type, string& value);
    void next_label_code(Exp* exp);
    string allocate_function_stack();
    void function_code(Call* func, ExpList* args);
    void function_declaration_code(Node* func_id, Formals* params, RetType* ret_type);
    void close_function(RetType* type);
    void merge_statement(Statement* statement1, Statement* statement2);
    void label_block_code(string label);
    void ruleID(Exp* exp);
    void ruleNum(Exp* exp);
    void ruleStr(Exp* exp);
    void ruleBool(Exp* exp);
    Exp* bool_exp(Exp* exp);

};
#endif //COMPI_HW5_GENERATOR_H
