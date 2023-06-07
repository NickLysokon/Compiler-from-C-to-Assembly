#ifndef SYMBOL_TABLE_HPP
#define SYMBOL_TABLE_HPP

#include <iostream>
#include <string>
#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <exception>
#include <vector>
#include "bp.hpp"

using std::exception;
using std::find;
using std::map;
using std::set;
using std::shared_ptr;
using std::string;
using std::vector;

typedef int Offset;

typedef enum
{
    Statement,
    Statements,
    Id,
    Expression,
    String,
    Call,
    Std,
    RetType
} SymType;

class symbolGen
{
public:
    SymType symbol_type;

    symbolGen(SymType symbol_type);
    virtual ~symbolGen() = default;
};

class symbolReturn : public symbolGen
{
protected:
    string return_type;

public:
    const string &getTypeName() const;
    symbolReturn(const string &type);
};

class symbolVar : public symbolReturn
{
public:
    const string &getVarName() const;
    symbolVar(const string &type);
};

class symbolId : public symbolGen
{
protected:
    string name;
    string type;
    string registerName;

public:
    Offset offset;
    symbolId(const string &name, const string &type);

    inline const string &getIdName() const
    {
        return this->name;
    }

    inline const string &getType() const
    {
        return this->type;
    }

    inline void setOffset(Offset newOffset)
    {
        this->offset = newOffset;
    }

    inline Offset getOffset() const
    {
        return this->offset;
    }

    inline const string &getRegisterName() const
    {
        return this->registerName;
    }

    inline void setRegisterName(string registerName)
    {
        this->registerName = registerName;
    }
};

class symbolFunc : public symbolId
{
    vector<string> arg_types;
    map<string, string> mapFormalNameToReg;
    string return_type;

public:
    symbolFunc(const string &name, const string &type, const vector<shared_ptr<symbolId>> &formals, bool isPredefined = false);
    static shared_ptr<symbolFunc> startSymbolFuncWithDomain(const string &name, shared_ptr<symbolReturn> type, const vector<shared_ptr<symbolId>> &formals);
    static void endSymbolFuncDomain();

    inline const vector<string> &getArgTypes() const
    {
        return this->arg_types;
    }

    inline const string &getType() const
    {
        return this->return_type;
    }

    inline vector<string> &getArgTypes()
    {
        return this->arg_types;
    }
};

class symbolExp : public symbolGen
{
    string type;
    string registerOrImmediate;

public:
    symbolExp(const string &type);
    symbolExp(const string &type, const string &regOrImmStr);

    string getRegOrImmResult();

    inline const string &getType() const
    {
        return this->type;
    }

    inline string &getType()
    {
        return this->type;
    }

    inline bool isBool() const
    {
        return this->type == "BOOL";
    }

    inline bool isInt() const
    {
        return this->type == "INT";
    }

    inline bool isString() const
    {
        return this->type == "STRING";
    }

    inline bool isByte() const
    {
        return this->type == "BYTE";
    }

    // Get result of bin operation on two expressions
    static shared_ptr<symbolExp> getBinOpResult(shared_ptr<symbolGen> stype1, shared_ptr<symbolGen> stype2, int op);

    // Get shared_ptr<symbolExp> by casting exp from srcType to dstType
    static shared_ptr<symbolExp> getCastResult(shared_ptr<symbolGen> dstStype, shared_ptr<symbolGen> expStype);

    // Get shared_ptr<symbolExp> from a function call
    static shared_ptr<symbolExp> getCallResult(shared_ptr<symbolFunc> funcIdStype, shared_ptr<symbolGen> argsStype);

    // Get shared_ptr<symbolExp> from variable ID
    static shared_ptr<symbolExp> loadIdValue(shared_ptr<symbolId> idSymbol, string pRegisterStackVar);

    // Get shared_ptr<symbolExp> from string literal
    static shared_ptr<symbolExp> loadStringLiteralAddr(string literal);

    // Get shared_ptr<symbolExp> from the result of comparing this and otherScExp
    static shared_ptr<symbolExp> getCmpResult(shared_ptr<symbolGen> stype1, shared_ptr<symbolGen> stype2, int op);
};

class symbolCall : public symbolGen
{
    string type;
    string symbol;

public:
    symbolCall(const string &type, const string &symbol);
    const string &getType() const;
};

class symbol_table
{
    map<string, shared_ptr<symbolId>> symbolTable;
    vector<string> formals;
    vector<vector<string>> domainSymbols;
    vector<int> domainOffset;
    Offset currOffset;
    vector<AddressList> breakListStack;
    vector<string> loopCondStartLabelStack;

public:
    shared_ptr<symbolReturn> retType;
    string pRegisterStackVar;
    int Depth;
    symbol_table();
    ~symbol_table();
    void addDomain(int numOfArgs = 0);
    void removeDomain();
    void insertSymbol(shared_ptr<symbolId> type);
    void insertFormal(shared_ptr<symbolId> type);
    void addContinue();
    void addBreak();
    void startLoop(const string &loopCondStart);
    void endLoop(AddressList &falseList);
    shared_ptr<symbolId> getVarSymbol(const string &name);
    shared_ptr<symbolFunc> getFuncSymbol(const string &name, bool shouldError = true);
    void printSymbolTable();

    inline int getCurrentDomainDepth() const
    {
        return this->domainSymbols.size() - 1;
    }
};

const string &verifyAllTypes(const string &type);
const string &verifyRetTypes(const string &type);
const string &verifyVarTypes(const string &type);

// // helper functions:
bool castingAllow(shared_ptr<symbolGen> rawExp1, shared_ptr<symbolGen> rawExp2);
bool strTypesCompareCheck(const string &typeStr1, const string &typeStr2);
void booleanCheck(shared_ptr<symbolGen> exp);
void verifyMainExists(symbol_table &symbol_table);

void tryinsertSymbolWithExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawSymbol,
                            shared_ptr<symbolGen> rawExp);
void addAutoSymbolWithExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawId,
                          shared_ptr<symbolGen> rawExp);
void tryAssignExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawId, shared_ptr<symbolGen> rawExp);

void emitAssign(shared_ptr<symbolId> symbol, shared_ptr<symbolExp> exp, string pRegisterStackVar);
void addUninitializedSymbol(symbol_table &symbolTable, shared_ptr<symbolGen> rawSymbol);
void handleReturn(shared_ptr<symbolReturn> retType);
void handleReturnExp(shared_ptr<symbolReturn> retType, shared_ptr<symbolGen> rawExp);
void emitReturn(shared_ptr<symbolReturn> retType, shared_ptr<symbolExp> exp);

shared_ptr<symbolGen> handleExpList(shared_ptr<symbolGen> exp, shared_ptr<symbolGen> list);
string typeNameToLlvmType(const string &typeName);
void handleIfStart(shared_ptr<symbolGen> conditionStype);
shared_ptr<symbolGen> handleIfEnd(shared_ptr<symbolGen> conditionStype, bool hasElse = false);
void handleElseEnd(shared_ptr<symbolGen> endIfListStype);
void handleWhileStart(shared_ptr<symbolGen> conditionStype, string startLabel);
void handleWhileEnd(shared_ptr<symbolGen> endIfListStype);

template <typename T>
class SymbolType : public symbolGen
{
    T value;

public:
    SymbolType(T value) : symbolGen(Std), value(value){};
    const T &getValue() const
    {
        std::cout << "2" << std::endl;
        return value;
    };
    T &getValue()
    {
        return value;
    };
};

class ShortCircuitBool : public symbolExp, public std::enable_shared_from_this<ShortCircuitBool>
{
    // Used only by bool expressions
    AddressList boolFalseList;
    AddressList boolTrueList;

public:
    ShortCircuitBool(const AddressList &trueList, const AddressList &falseList);
    ShortCircuitBool(shared_ptr<symbolExp> boolExp);

    // Short-Circuit eval bool value
    shared_ptr<ShortCircuitBool> evalBool(shared_ptr<symbolGen> otherScExpStype, shared_ptr<symbolGen> rightOperandStartStype, int op);

    inline AddressList &getFalseList()
    {
        return this->boolFalseList;
    }

    inline AddressList &getTrueList()
    {
        return this->boolTrueList;
    }
    shared_ptr<symbolExp> finallizeTosymbolExp();
};

shared_ptr<symbolExp> assureSymbolExp(shared_ptr<symbolGen> expStype);
shared_ptr<ShortCircuitBool> assureScBool(shared_ptr<symbolGen> scBoolStype);

static shared_ptr<ShortCircuitBool> lastScBool = nullptr;

inline shared_ptr<ShortCircuitBool> saveScBool(shared_ptr<symbolGen> scBoolStype)
{
    lastScBool = assureScBool(scBoolStype);
    return lastScBool;
}

inline shared_ptr<ShortCircuitBool> getLastScBool()
{
    auto boolExp = lastScBool;
    lastScBool = nullptr;
    return boolExp;
}

struct Exception : public std::exception
{
    string s;
    Exception(string ss) : s(ss) {}
    ~Exception() throw() {} // Updated
    const char *what() const throw() { return s.c_str(); }
};

typedef shared_ptr<symbolGen> pSymbolGen;

#define YYSTYPE pSymbolGen

#endif // SYMBOLTABLE_HPP