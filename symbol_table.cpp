#include "symbol_table.hpp"
#include "hw3_output.hpp"
#include "registerAlloc.hpp"
#include "parser.tab.hpp"

using namespace output;
using std::get;

extern symbol_table symbolTable;

template <typename Base, typename T>
inline bool instanceof (const T *ptr)
{
    return dynamic_cast<const Base *>(ptr) != nullptr;
}

symbolGen::symbolGen(SymType sym_type_src) : symbol_type(sym_type_src)
{}

const string &symbolReturn::getTypeName() const
{
    if (!this)
        throw("Null Symbol");
    return this->return_type;
}

const string &symbolVar::getVarName() const
{
    if (!this)
        throw("Null Symbol");
    return this->return_type;
}

symbolReturn::symbolReturn(const string &type) : symbolGen(RetType), return_type(verifyRetTypes(type))
{}

const string &verifyRetTypes(const string &type)
{
    if (verifyAllTypes(type) != "STRING")
        return type;
    else
        errorMismatch(yylineno);
    return type;
}

const string &verifyAllTypes(const string &type)
{
    if (type == "INT" || type == "BOOL" || type == "SC_BOOL" || type == "BYTE" || type == "VOID" || type == "STRING" || type == "BAD_CALL")
        return type;
    else
        errorMismatch(yylineno);
    return type;
}

const string &verifyVarTypes(const string &type)
{
    if (verifyAllTypes(type) != "VOID")
        return type;
    else
        errorMismatch(yylineno);
    return type;
}

symbolVar::symbolVar(const string &type) : symbolReturn(verifyAllTypes(type))
{}

symbolId::symbolId(const string &name_src, const string &type_src) : symbolGen(Id), name(name_src), type(verifyVarTypes(type_src))
{}

static vector<string> getTypesFromIds(const vector<shared_ptr<symbolId>> &ids)
{
    vector<string> types;

    for (auto id : ids)
        types.push_back(id->getType());

    return types;
}

symbolFunc::symbolFunc(const string &name, const string &type, const vector<shared_ptr<symbolId>> &formals, bool isPredefined) : symbolId(name, "BAD_CALL"),
                                                                                                                                 arg_types(getTypesFromIds(formals)), mapFormalNameToReg(), return_type(verifyRetTypes(type))
{
    CodeBuffer &buffer = CodeBuffer::instance();
    Ralloc &ralloc = Ralloc::instance();
    string retTypeStr = typeNameToLlvmType(this->getType());

    string formalsStr = "";
    string regName = "";

    for (auto &formal : formals)
    {
        regName = ralloc.getNextRegister("formal_" + formal->getIdName());
        formalsStr += typeNameToLlvmType(formal->getType()) + " " + regName + ", ";
        this->mapFormalNameToReg[formal->getIdName()] = regName;
        formal->setRegisterName(regName);
    }

    //remove comma and space
    if (formalsStr.size() > 0)
    {
        formalsStr.pop_back();
        formalsStr.pop_back();
    }

    if (isPredefined)
        return;

    buffer.emit("define " + retTypeStr + " @" + name + "(" + formalsStr.substr() + ") {");
    symbolTable.pRegisterStackVar = ralloc.getNextRegister("symbolFuncStackVarPtrReg");
    buffer.emit("\t" + symbolTable.pRegisterStackVar + " = alloca i32, i32 50");
}

shared_ptr<symbolFunc> symbolFunc::startSymbolFuncWithDomain(const string &name, shared_ptr<symbolReturn> type, const vector<shared_ptr<symbolId>> &formals)
{
    auto funcId = (shared_ptr<symbolFunc>(new symbolFunc(name, type->getTypeName(), formals)));

    symbolTable.insertSymbol(funcId);

    symbolTable.addDomain(formals.size());
    for (auto i = 0; i < formals.size(); i++)
    {
        symbolTable.insertFormal(formals[i]);
    }
    symbolTable.retType = type;
    return funcId;
}

void symbolFunc::endSymbolFuncDomain()
{
    symbolTable.removeDomain();
    string retTypeLlvm = "default";
    string defaultRetVal = "default";
    retTypeLlvm = typeNameToLlvmType(symbolTable.retType->getTypeName());
    defaultRetVal = retTypeLlvm + (retTypeLlvm == "void" ? "" : " 0");
    symbolTable.retType = nullptr;
    auto &codeBuffer = CodeBuffer::instance();
    string returnString = "ret " + defaultRetVal;
    codeBuffer.emit(returnString);
    // To balance rainbow brackets {
    codeBuffer.emit("}");
    codeBuffer.emit("");
}

static vector<shared_ptr<symbolExp>> &assureSymbolExpList(shared_ptr<symbolGen> listStype)
{
    if (listStype == nullptr)
        throw Exception("nullptr can't be assured as symbolExpList");

    shared_ptr<SymbolType<vector<shared_ptr<symbolExp>>>> expList = dynamic_pointer_cast<SymbolType<vector<shared_ptr<symbolExp>>>>(listStype);
    if (expList == nullptr)
        throw Exception("Expected list of expressions");
    return expList->getValue();
}

shared_ptr<symbolExp> assureSymbolExp(shared_ptr<symbolGen> expStype)
{
    if (expStype == nullptr)
        throw Exception("nullptr can't be assured as symbolExp");

    auto exp = (dynamic_pointer_cast<symbolExp>(expStype));
    if (expStype == nullptr)
        throw Exception("Failed to assure symbolExp from Stype");

    if (exp->getType() == "SC_BOOL")
        throw Exception("Got symbolExp which is actually SC_BOOL. You probably don't want to use it like that");

    return exp;
}

shared_ptr<symbolGen> handleExpList(shared_ptr<symbolGen> expStype, shared_ptr<symbolGen> listStype)
{
    if (listStype == nullptr)
        listStype = (shared_ptr<SymbolType<vector<shared_ptr<symbolExp>>>>(new SymbolType<vector<shared_ptr<symbolExp>>>(vector<shared_ptr<symbolExp>>())));

    auto &list = assureSymbolExpList(listStype);
    auto exp = assureSymbolExp(expStype);
    list.push_back(exp);
    return listStype;
}

symbolExp::symbolExp(const string &type) : symbolGen(Expression), type(verifyVarTypes(type))
{}

symbolExp::symbolExp(const string &type, const string &regOrImmStr) : symbolGen(Expression), type(verifyVarTypes(type)), registerOrImmediate(regOrImmStr)
{
    if (regOrImmStr[0] != '%' and type == "BYTE" and stoi(regOrImmStr) > 255)
        errorByteTooLarge(yylineno, regOrImmStr);

    if (type != "SC_BOOL" and regOrImmStr == "")
        throw Exception("Only SC_BOOL can have no register");
}



ShortCircuitBool::ShortCircuitBool(const AddressList &trueList, const AddressList &falseList)
    : symbolExp("SC_BOOL", "0"), boolTrueList(trueList), boolFalseList(falseList) {}

ShortCircuitBool::ShortCircuitBool(shared_ptr<symbolExp> boolExp) : ShortCircuitBool(AddressList(), AddressList())
{
    if (boolExp == nullptr)
        throw Exception("Can't convert nullptr symbolExp to ShortCircuitBool");
    else if (not boolExp->isBool())
        throw Exception("Can't convert non BOOL symbolExp to ShortCircuitBool");
    auto &buffer = CodeBuffer::instance();
    int instrAddr = buffer.emit("br i1 " + boolExp->getRegOrImmResult() + ", label @, label @");
    this->boolTrueList.push_back(make_pair(instrAddr, FIRST));
    this->boolFalseList.push_back(make_pair(instrAddr, SECOND));
}

shared_ptr<ShortCircuitBool> assureScBool(shared_ptr<symbolGen> scBoolStype)
{
    if (scBoolStype == nullptr)
        throw Exception("nullptr can't be assured as ShortCircuitBool");
    auto scBool = (dynamic_pointer_cast<ShortCircuitBool>(scBoolStype));
    if (scBool == nullptr)
        throw Exception("Can't assure ScBool");

    return scBool;
}

void handleIfStart(shared_ptr<symbolGen> scBoolStype)
{
    auto scBool = assureScBool(scBoolStype);
    auto &buffer = CodeBuffer::instance();
    auto trueList = scBool->getTrueList();
    string trueLabel = buffer.genLabel("ifStatementStart");
    buffer.bpatch(trueList, trueLabel);
}

shared_ptr<symbolGen> handleIfEnd(shared_ptr<symbolGen> scBoolStype, bool hasElse)
{
    auto scBool = assureScBool(scBoolStype);
    auto &buffer = CodeBuffer::instance();
    shared_ptr<symbolGen> brEndElseInstrStype = nullptr;

    if (hasElse)
    {
        AddressIndPair brEndElseInstr = make_pair(buffer.emit("br label @"), FIRST);
        brEndElseInstrStype = (shared_ptr<SymbolType<AddressIndPair>>(new SymbolType<AddressIndPair>(AddressIndPair(brEndElseInstr))));
    }

    auto falseList = scBool->getFalseList();

    string falseLabel = buffer.genLabel("ifEnd");
    buffer.bpatch(falseList, falseLabel);

    return brEndElseInstrStype;
}

void handleElseEnd(shared_ptr<symbolGen> endIfListStype)
{
    AddressIndPair &endIfInstr = (dynamic_pointer_cast<SymbolType<AddressIndPair>>(endIfListStype)->getValue());
    auto &buffer = CodeBuffer::instance();

    string elseEndLabel = buffer.genLabel("elseEnd");

    buffer.bpatch(endIfInstr, elseEndLabel);
}

void handleWhileStart(shared_ptr<symbolGen> scBoolStype, string startLabel)
{
    handleIfStart(scBoolStype);
    symbolTable.startLoop(startLabel);
}

void handleWhileEnd(shared_ptr<symbolGen> scBoolStype)
{
    auto scBool = assureScBool(scBoolStype);

    symbolTable.addContinue();

    symbolTable.endLoop(scBool->getFalseList());
}


string symbolExp::getRegOrImmResult()
{
    if (instanceof <ShortCircuitBool>(this))
        throw Exception("Trying to get RegOrImm for ShortCircuitBool");
    if (this->registerOrImmediate == "")
        throw Exception("symbolExp without register or immediate");
    const char *result = this->registerOrImmediate.c_str();
    return this->registerOrImmediate;
}

shared_ptr<symbolExp> symbolExp::getBinOpResult(shared_ptr<symbolGen> stype1, shared_ptr<symbolGen> stype2, int op)
{
    shared_ptr<symbolExp> exp1 = (dynamic_pointer_cast<symbolExp>(stype1));
    shared_ptr<symbolExp> exp2 = (dynamic_pointer_cast<symbolExp>(stype2));
    shared_ptr<symbolExp> resultExp;
    Ralloc &ralloc = Ralloc::instance();
    CodeBuffer &codeBuffer = CodeBuffer::instance();
    string resultSizeof;
    string resultType;
    string divOp;
    string opStr;
    string resultReg = ralloc.getNextRegister("getBinOpResult");
    string exp1Reg = exp1->getRegOrImmResult();
    string exp2Reg = exp2->getRegOrImmResult();

    if (not castingAllow(stype1, stype2))
        errorMismatch(yylineno);

    if (exp1->isInt() or exp2->isInt())
    {
        if (exp1->isByte())
        {
            string resultReg = ralloc.getNextRegister("binOpResExp1");
            codeBuffer.emit(resultReg + " = zext i8 " + exp1Reg + " to i32");
            exp1Reg = resultReg;
        }
        else if (exp2->isByte())
        {
            string resultReg = ralloc.getNextRegister("binOpResExp1");
            codeBuffer.emit(resultReg + " = zext i8 " + exp2Reg + " to i32");
            exp2Reg = resultReg;
        }
        resultSizeof = "i32";
        resultType = "INT";
        divOp = "sdiv";
    }
    else
    {
        resultSizeof = "i8";
        resultType = "BYTE";
        divOp = "udiv";
    }

    int instAddr, instAddr2;
    string ifShouldErrorDivBy0;
    string labelDivBy0;
    string labelNotDivBy0;

    switch (op)
    {
    case ADDOP:
        opStr = "add";
        break;
    case SUBOP:
        opStr = "sub";
        break;
    case MULOP:
        opStr = "mul";
        break;
    case DIVOP:
        ifShouldErrorDivBy0 = ralloc.getNextRegister("divBy0icmp");

        codeBuffer.emit(ifShouldErrorDivBy0 + " = icmp eq " + typeNameToLlvmType(exp2->getType()) + " " + exp2->getRegOrImmResult() + ", 0");
        instAddr = codeBuffer.emit("br i1 " + ifShouldErrorDivBy0 + ", label @, label @");
        labelDivBy0 = codeBuffer.genLabel("labelDivBy0");
        codeBuffer.emit("call void @error_division_by_zero()");
        labelNotDivBy0 = codeBuffer.genLabel("labelNotDivBy0");

        codeBuffer.bpatch(make_pair(instAddr, FIRST), labelDivBy0);
        codeBuffer.bpatch(make_pair(instAddr, SECOND), labelNotDivBy0);
        opStr = divOp;
        break;
    default:
        errorMismatch(yylineno);
    }

    codeBuffer.emit(resultReg + " = " + opStr + " " + resultSizeof + " " + exp1Reg + ", " + exp2Reg);
    return shared_ptr<symbolExp>((shared_ptr<symbolExp>(new symbolExp(resultType, resultReg))));
}

static void insertToListFromList(AddressList &listTo, AddressList &listFrom)
{
    listTo.insert(listTo.end(), listFrom.begin(), listFrom.end());
}

shared_ptr<ShortCircuitBool> ShortCircuitBool::evalBool(shared_ptr<symbolGen> otherScExpStype, shared_ptr<symbolGen> rightOperandStartStype, int op)
{
    auto &buffer = CodeBuffer::instance();
    shared_ptr<ShortCircuitBool> otherScExp;
    string secondOperandStartLabel;

    AddressList trueList;
    AddressList falseList;
    if (op == OR)
    {
        {
            otherScExp = assureScBool(otherScExpStype);
            secondOperandStartLabel = (dynamic_pointer_cast<SymbolType<string>>(rightOperandStartStype)->getValue());
        }
        buffer.bpatch(this->boolFalseList, secondOperandStartLabel);
        this->boolFalseList = otherScExp->boolFalseList;
        insertToListFromList(this->boolTrueList, otherScExp->boolTrueList);
        return shared_from_this();
    }
    else if (op == AND)
    {
        {
            otherScExp = assureScBool(otherScExpStype);
            secondOperandStartLabel = (dynamic_pointer_cast<SymbolType<string>>(rightOperandStartStype)->getValue());
        }
        buffer.bpatch(this->boolTrueList, secondOperandStartLabel);
        insertToListFromList(this->boolFalseList, otherScExp->boolFalseList);
        this->boolTrueList = otherScExp->boolTrueList;
        return shared_from_this();
    }
    else if (op == NOT)
    {
        falseList = this->boolTrueList;
        trueList = this->boolFalseList;
        this->boolTrueList = trueList;
        this->boolFalseList = falseList;
        return shared_from_this();
    }
    else
    {
        errorMismatch(yylineno);
        throw Exception("Impossible to reach here");
    }
}

shared_ptr<symbolExp> symbolExp::getCmpResult(shared_ptr<symbolGen> stype1, shared_ptr<symbolGen> stype2, int op)
{
    shared_ptr<symbolExp> exp1 = (dynamic_pointer_cast<symbolExp>(stype1));
    shared_ptr<symbolExp> exp2 = (dynamic_pointer_cast<symbolExp>(stype2));
    if (not exp1 or not exp2)
        throw Exception("getCmpResult must get _Nonnull expressions");

    string cmpOpStr;
    string regSizeofDecorator;

    if (not castingAllow(stype1, stype2))
    {
        errorMismatch(yylineno);
        return nullptr;
    }

    Ralloc &ralloc = Ralloc::instance();
    CodeBuffer &buffer = CodeBuffer::instance();

    string resultReg = ralloc.getNextRegister("cmpOpRes");

    string exp1RegOrImm = exp1->getRegOrImmResult();
    string exp2RegOrImm = exp2->getRegOrImmResult();

    shared_ptr<symbolExp> resultExp = (shared_ptr<symbolExp>(new symbolExp("BOOL", resultReg)));

    if (exp1->isInt() or exp2->isInt())
    {
        regSizeofDecorator = " i32 ";
        if (exp1->isByte() and exp1RegOrImm[0] == '%')
        {
            string newReg = ralloc.getNextRegister("cmpOpRegOrImmExp1");
            buffer.emit(newReg + " = zext i8 " + exp1RegOrImm + " to i32");
            exp1RegOrImm = newReg;
        }
        else if (exp2->isByte() and exp2RegOrImm[0] == '%')
        {
            string newReg = ralloc.getNextRegister("cmpOpRegOrImmExp2");
            buffer.emit(newReg + " = zext i8 " + exp2RegOrImm + " to i32");
            exp2RegOrImm = newReg;
        }
    }
    else
        regSizeofDecorator = " i8 ";

    switch (op)
    {
    case EQOP:
        cmpOpStr = " eq ";
        break;
    case NEOP:
        cmpOpStr = " ne ";
        break;
    case GEOP:
        cmpOpStr = " sge ";
        break;
    case GTOP:
        cmpOpStr = " sgt ";
        break;
    case LEOP:
        cmpOpStr = " sle ";
        break;
    case LTOP:
        cmpOpStr = " slt ";
        break;
    default:
        throw Exception("Unsupported operation to getCmpResult");
    }

    buffer.emit(resultReg + " = icmp " + cmpOpStr + regSizeofDecorator + exp1RegOrImm + ", " + exp2RegOrImm);
    return resultExp;
}

shared_ptr<symbolExp> symbolExp::getCastResult(shared_ptr<symbolGen> dstStype, shared_ptr<symbolGen> expStype)
{
    shared_ptr<symbolExp> exp = (dynamic_pointer_cast<symbolExp>(expStype));
    if (not exp)
        throw Exception("getCastResult got Null expStype");

    shared_ptr<symbolVar> dstType = (dynamic_pointer_cast<symbolVar>(dstStype));

    if (not dstType)
        throw Exception("getCastResult got Null dstStype");

    if (not strTypesCompareCheck(exp->getType(), dstType->getTypeName()) and not strTypesCompareCheck(dstType->getTypeName(), exp->getType()))
    {
        errorMismatch(yylineno);
        return nullptr;
    }

    exp->getRegOrImmResult();

    Ralloc &ralloc = Ralloc::instance();
    CodeBuffer &codeBuffer = CodeBuffer::instance();
    string resultReg = ralloc.getNextRegister("castRes");

    shared_ptr<symbolExp> resultExp = (shared_ptr<symbolExp>(new symbolExp(dstType->getTypeName(), resultReg)));

    if (exp->isInt() and dstType->getTypeName() == "BYTE")
        codeBuffer.emit(resultReg + " = trunc i32 " + exp->getRegOrImmResult() + " to i8");

    else if (exp->isByte() and dstType->getTypeName() == "INT")
        codeBuffer.emit(resultReg + " = zext i8 " + exp->getRegOrImmResult() + " to i32");

    else
        codeBuffer.emit(resultReg + " = add " + typeNameToLlvmType(exp->getType()) + " " + exp->getRegOrImmResult() + ", 0");

    codeBuffer.emit("; DEBUG: got cast result (" + dstType->getTypeName() + ") from " + exp->getType());
    return resultExp;
}

shared_ptr<symbolExp> symbolExp::getCallResult(shared_ptr<symbolFunc> funcId, shared_ptr<symbolGen> argsStype)
{
    vector<shared_ptr<symbolExp>> args = (dynamic_pointer_cast<SymbolType<vector<shared_ptr<symbolExp>>>>(argsStype)->getValue());
    auto &ralloc = Ralloc::instance();
    auto &buffer = CodeBuffer::instance();
    string llvmRetType = typeNameToLlvmType(funcId->getType());
    string resultReg = ralloc.getNextRegister("callRes_" + funcId->getIdName());
    string resultAssignment = llvmRetType == "void" ? "" : (resultReg + " = ");
    string expListStr = "";
    auto &formalsTypes = funcId->getArgTypes();

    if (formalsTypes.size() != args.size())
        errorPrototypeMismatch(yylineno, funcId->getIdName(), formalsTypes);

    string argReg;

    for (int i = 0; i < args.size(); i++)
    {
        if (not strTypesCompareCheck(formalsTypes[i], args[i]->getType()))
        {
            errorMismatch(yylineno);
            return nullptr;
        }

        argReg = args[i]->getRegOrImmResult();
        if (args[i]->getType() != formalsTypes[i])
        {
            string zextExpReg = ralloc.getNextRegister("zextCallFuncArg" + to_string(i) + "_");
            buffer.emit(zextExpReg + " = zext " + typeNameToLlvmType(args[i]->getType()) + " " + argReg + " to " + typeNameToLlvmType(formalsTypes[i]));
            argReg = zextExpReg;
        }

        expListStr += typeNameToLlvmType(formalsTypes[i]) + " " + argReg + ", ";
    }

    if (expListStr.size() > 0)
    {
        expListStr.pop_back();
        expListStr.pop_back();
    }

    shared_ptr<symbolExp> resultExp = nullptr;

    if (funcId->getType() != "VOID")
        resultExp = (shared_ptr<symbolExp>(new symbolExp(funcId->getType(), resultReg)));

    buffer.emit(resultAssignment + "call " + llvmRetType + " @" + funcId->getIdName() + "(" + expListStr + ")");

    return resultExp;
}

shared_ptr<symbolExp> symbolExp::loadIdValue(shared_ptr<symbolId> idSymbol, string pRegisterStackVar)
{
    CodeBuffer &codeBuffer = CodeBuffer::instance();
    Ralloc &ralloc = Ralloc::instance();

    if (idSymbol->getRegisterName() != "")
        return (shared_ptr<symbolExp>(new symbolExp(idSymbol->getType(), idSymbol->getRegisterName())));

    string llvmType = typeNameToLlvmType(idSymbol->getType());
    string offsetReg = ralloc.getNextRegister("loadIdOffset");
    string idAddrReg = ralloc.getNextRegister("loadIdIdAddr");
    string expReg = ralloc.getNextRegister("idVal_" + idSymbol->getIdName());
    shared_ptr<symbolExp> idValuesymbolExp = (shared_ptr<symbolExp>(new symbolExp(idSymbol->getType(), expReg)));

    codeBuffer.emit(offsetReg + " = add i32 0, " + std::to_string(idSymbol->getOffset()));
    codeBuffer.emit(idAddrReg + " = getelementptr i32, i32* " + pRegisterStackVar + ", i32 " + offsetReg);
    string idAddrRegCorrectSize = idAddrReg;

    if (llvmType != "i32")
    {
        idAddrRegCorrectSize = ralloc.getNextRegister("idAddrCorrect");
        codeBuffer.emit(idAddrRegCorrectSize + " = bitcast i32* " + idAddrReg + " to " + llvmType + "*");
    }

    codeBuffer.emit(expReg + " = load " + llvmType + ", " + llvmType + "* " + idAddrRegCorrectSize);

    return idValuesymbolExp;
}

shared_ptr<symbolExp> symbolExp::loadStringLiteralAddr(string literal)
{
    literal = literal.substr(1, literal.length() - 2);
    auto &ralloc = Ralloc::instance();
    auto &codeBuffer = CodeBuffer::instance();
    string strLiteralAutoGeneratedName = ralloc.getNextVarName();
    string resultReg = ralloc.getNextRegister("loadStringLiteralResult");
    int literalLength = literal.length() + 1; // + 1 for '\0'
    string literalLenStr = std::to_string(literalLength);

    codeBuffer.emitGlobal(strLiteralAutoGeneratedName + " = constant [" +
                          literalLenStr + " x i8] c\"" + literal + "\\00\"");

    codeBuffer.emit(resultReg + " = getelementptr [" + literalLenStr + " x i8], [" + literalLenStr + " x i8]* " +
                    strLiteralAutoGeneratedName + ", i32 0, i32 0");

    return (shared_ptr<symbolExp>(new symbolExp("STRING", resultReg)));
}

shared_ptr<symbolExp> ShortCircuitBool::finallizeTosymbolExp()
{
    if (this->boolTrueList.size() == 0 or this->boolFalseList.size() == 0)
        throw Exception("Can't finallizeTosymbolExp ShortCircuitBool expression when true/false list is empty");

    auto &ralloc = Ralloc::instance();
    auto &buffer = CodeBuffer::instance();

    string resultRegister = ralloc.getNextRegister("finallizedScBool");
    shared_ptr<symbolExp> resultExp = (shared_ptr<symbolExp>(new symbolExp("BOOL", resultRegister)));
    AddressList resultLabelList;

    string trueLabel = buffer.genLabel("finallizeScBoolTrue");
    resultLabelList.push_back(make_pair(buffer.emit("br label @"), FIRST));

    string falseLabel = buffer.genLabel("finallizeScBoolFalse");
    resultLabelList.push_back(make_pair(buffer.emit("br label @"), FIRST));
    buffer.bpatch(this->boolTrueList, trueLabel);
    buffer.bpatch(this->boolFalseList, falseLabel);
    
    string phi = buffer.genLabel("finallizeScBoolPhi");
    buffer.emit(resultRegister + " = phi i1 [true, %" + trueLabel + "], [false, %" + falseLabel + "]");
    buffer.bpatch(resultLabelList, phi);
    return resultExp;
}

symbolCall::symbolCall(const string &type, const string &symbol) : symbolGen(Call), type(verifyRetTypes(type)), symbol(symbol)
{}


symbol_table::symbol_table()
{
    this->Depth = 0;
    this->currOffset = 0;
    this->addDomain();
    this->insertSymbol((shared_ptr<symbolFunc>(new symbolFunc("print", "VOID", vector<shared_ptr<symbolId>>({(shared_ptr<symbolId>(new symbolId("msg", "STRING")))}), true))));
    this->insertSymbol((shared_ptr<symbolFunc>(new symbolFunc("printi", "VOID", vector<shared_ptr<symbolId>>({(shared_ptr<symbolId>(new symbolId("i", "INT")))}), true))));
    this->insertSymbol((shared_ptr<symbolFunc>(new symbolFunc("error_division_by_zero", "VOID", vector<shared_ptr<symbolId>>({}), true))));

    auto &buffer = CodeBuffer::instance();
    buffer.emitGlobal("declare i32 @printf(i8*, ...)");
    buffer.emitGlobal("declare void @exit(i32)");
    buffer.emitGlobal("@.int_specifier = constant [4 x i8] c\"%d\\0A\\00\"");
    buffer.emitGlobal("@.str_specifier = constant [4 x i8] c\"%s\\0A\\00\"");
    buffer.emitGlobal("@.error_div_zero_msg = constant [23 x i8] c\"Error division by zero\\00\"");
    buffer.emitGlobal("");
    buffer.emitGlobal("define void @printi(i32) {");
    buffer.emitGlobal("\t%spec_ptr = getelementptr [4 x i8], [4 x i8]* @.int_specifier, i32 0, i32 0");
    buffer.emitGlobal("\tcall i32 (i8*, ...) @printf(i8* %spec_ptr, i32 %0)");
    buffer.emitGlobal("\tret void");
    buffer.emitGlobal("}");
    buffer.emitGlobal("");
    buffer.emitGlobal("define void @print(i8*) {");
    buffer.emitGlobal("\t%spec_ptr = getelementptr [4 x i8], [4 x i8]* @.str_specifier, i32 0, i32 0");
    buffer.emitGlobal("\tcall i32 (i8*, ...) @printf(i8* %spec_ptr, i8* %0)");
    buffer.emitGlobal("\tret void");
    buffer.emitGlobal("}");
    buffer.emitGlobal("");
    buffer.emitGlobal("define void @error_division_by_zero() {");
    buffer.emitGlobal("\t%spec_ptr = getelementptr [23 x i8], [23 x i8]* @.error_div_zero_msg, i32 0, i32 0");
    buffer.emitGlobal("\tcall void (i8*) @print(i8* %spec_ptr)");
    buffer.emitGlobal("\tcall void (i32) @exit(i32 0)");
    buffer.emitGlobal("\tret void");
    buffer.emitGlobal("}");
    buffer.emitGlobal("");
}

symbol_table::~symbol_table()
{}

void symbol_table::addDomain(int numOfArgs)
{
    if (not((numOfArgs >= 0 and this->domainOffset.size() == 1) || (this->domainOffset.size() > 1 and numOfArgs == 0) || this->domainOffset.size() == 0))
        throw Exception("Code error. We should only add a scope of a function when we are in the global scope");

    this->domainSymbols.push_back(vector<string>());
    this->domainOffset.push_back(this->currOffset);
}

void symbol_table::insertSymbol(shared_ptr<symbolId> type)
{
    if (type == nullptr)
        throw Exception("Can't add a nullptr symbol to the symbol table");

    const string &name = type->getIdName();

    if (type->getType() == "STRING")
        errorMismatch(yylineno);

    if (this->symbolTable[name] != nullptr)
        errorDef(yylineno, name);

    this->domainSymbols.back().push_back(name);

    type->setOffset(this->currOffset);
    this->symbolTable[name] = type;

    if (this->domainOffset.size() > 1)
        this->currOffset++;
}

void symbol_table::removeDomain()
{

    string funcTypeStr;
    shared_ptr<symbolFunc> funcId;
    vector<string> argTypes;
    if (this->domainSymbols.size() == 2)
    {
        this->currOffset = 0;
        Offset offset = -1;
        for (int i = this->formals.size() - 1; i >= 0; i--)
        {
            this->symbolTable.erase(this->formals[i]);
        }
        this->formals.clear();
    }
    else
        this->currOffset -= this->domainSymbols.back().size();

    Offset offset = this->domainOffset.back();
    for (string s : this->domainSymbols.back())
    {
        if ((funcId = dynamic_pointer_cast<symbolFunc>(this->symbolTable[s])) != nullptr)
            funcTypeStr = makeFunctionType(funcId->getType(), funcId->getArgTypes());
        this->symbolTable.erase(s);
    }

    domainSymbols.pop_back();
    domainOffset.pop_back();
}



void symbol_table::addContinue()
{
    if (this->Depth == 0)
        errorUnexpectedContinue(yylineno);

    auto &buffer = CodeBuffer::instance();
    buffer.emit("; DEBUG: " + to_string(yylineno) + ": adding continue statement for loop in depth " + to_string(this->Depth));
    buffer.emit("br label %" + this->loopCondStartLabelStack.back());
}

void symbol_table::addBreak()
{
    if (this->Depth == 0)
        errorUnexpectedBreak(yylineno);
    auto &buffer = CodeBuffer::instance();
    buffer.emit("; DEBUG: " + to_string(yylineno) + ": adding break to loop in depth " + to_string(this->Depth));
    AddressIndPair instruction = make_pair(buffer.emit("br label @"), FIRST);
    this->breakListStack.back().push_back(instruction);
}

void symbol_table::startLoop(const string &loopCondStartLabel)
{
    this->Depth++;
    this->loopCondStartLabelStack.push_back(loopCondStartLabel);
    this->breakListStack.push_back(vector<AddressIndPair>());
}

void symbol_table::endLoop(AddressList &falseList)
{
    auto &buffer = CodeBuffer::instance();
    string endLoopLabel = buffer.genLabel("endLoopDepth" + to_string(this->Depth));
    buffer.bpatch(this->breakListStack.back(), endLoopLabel);
    buffer.bpatch(falseList, endLoopLabel);

    this->breakListStack.pop_back();
    this->loopCondStartLabelStack.pop_back();
    this->Depth--;
}

shared_ptr<symbolId> symbol_table::getVarSymbol(const string &name)
{
    auto symbol = this->symbolTable[name];

    // Check that the symbol exists in the symbol table
    if (symbol == nullptr or dynamic_pointer_cast<symbolFunc>(symbol) != nullptr)
        errorUndef(yylineno, name);
    return symbol;
}

void symbol_table::insertFormal(shared_ptr<symbolId> type)
{
    if (type == nullptr)
        throw Exception("Can't add a nullptr formal to the symbol table");
    const string &typeId = type->getIdName();

    if (this->symbolTable[typeId] != nullptr)
        errorDef(yylineno, typeId);

    this->formals.push_back(typeId);
    this->symbolTable[typeId] = type;
}

shared_ptr<symbolFunc> symbol_table::getFuncSymbol(const string &name, bool shouldError)
{
    auto symbol = this->symbolTable[name];

    // Check that the symbol exists in the symbol table
    shared_ptr<symbolFunc> funcSym = nullptr;

    if ((symbol == nullptr or (funcSym = dynamic_pointer_cast<symbolFunc>(symbol)) == nullptr) and shouldError)
        errorUndefFunc(yylineno, name);

    return funcSym;
}

void symbol_table::printSymbolTable()
{
    Offset offset = 0;
    for (auto it = this->symbolTable.begin(); it != this->symbolTable.end(); ++it)
        printID(it->first, offset++, it->second->getType());
}


bool castingAllow(shared_ptr<symbolGen> rawExp1, shared_ptr<symbolGen> rawExp2)
{
    auto exp1 = dynamic_pointer_cast<symbolExp>(rawExp1);
    auto exp2 = dynamic_pointer_cast<symbolExp>(rawExp2);

    bool isExp1IntOrByte = exp1->isByte() or exp1->isInt();
    bool isExp2IntOrByte = exp2->isByte() or exp2->isInt();

    bool canCastImplicitly = isExp1IntOrByte and isExp2IntOrByte;

    return canCastImplicitly;
}

bool strTypesCompareCheck(const string &typeStr1, const string &typeStr2)
{
    bool areEqual = typeStr1 == typeStr2;
    bool areIntAndByte = typeStr1 == "INT" and typeStr2 == "BYTE";

    bool canCastImplicitly = areEqual or areIntAndByte;

    return canCastImplicitly;
}



string typeNameToLlvmType(const string &typeName)
{
    if (typeName == "VOID")
        return "void";
    else if (typeName == "INT")
        return "i32";
    else if (typeName == "BYTE")
        return "i8";
    else if (typeName == "BOOL")
        return "i1";
    else if (typeName == "STRING")
        return "i8*";
    else
        throw Exception("Unsupported return type");
}

void verifyMainExists(symbol_table &symbol_table)
{
    auto token = yylex();
    if (token)
    {
    }
    auto mainFunc = symbol_table.getFuncSymbol("main", false);
    if (mainFunc == nullptr or mainFunc->getType() != "VOID" or mainFunc->getArgTypes().size() != 0)
        errorMainMissing();

    symbol_table.removeDomain();
}

void booleanCheck(shared_ptr<symbolGen> exp)
{
    auto expType = dynamic_pointer_cast<symbolExp>(exp);
    if (not expType->isBool())
        errorMismatch(yylineno);
}



void addUninitializedSymbol(symbol_table &symbolTable, shared_ptr<symbolGen> rawSymbol)
{
    shared_ptr<symbolId> symbol = (dynamic_pointer_cast<symbolId>(rawSymbol));
    shared_ptr<symbolExp> zeroExp = (shared_ptr<symbolExp>(new symbolExp(symbol->getType(), "0")));

    symbolTable.insertSymbol(symbol);

    emitAssign(symbol, zeroExp, symbolTable.pRegisterStackVar);
}

void tryinsertSymbolWithExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawSymbol, shared_ptr<symbolGen> rawExp)
{
    shared_ptr<symbolId> symbolToEmit = (dynamic_pointer_cast<symbolId>(rawSymbol));
    shared_ptr<symbolExp> expToEmit = (dynamic_pointer_cast<symbolExp>(rawExp));

    if (not strTypesCompareCheck(symbolToEmit->getType(), expToEmit->getType()))
        errorMismatch(yylineno);

    symbolTable.insertSymbol(symbolToEmit);

    emitAssign(symbolToEmit, expToEmit, symbolTable.pRegisterStackVar);
}

void addAutoSymbolWithExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawId,
                          shared_ptr<symbolGen> rawExp)
{
    string id = (dynamic_pointer_cast<SymbolType<string>>(rawId)->getValue());
    shared_ptr<symbolExp> exp = (dynamic_pointer_cast<symbolExp>(rawExp));
    shared_ptr<symbolId> symbol = (shared_ptr<symbolId>(new symbolId(id, exp->getType())));

    symbolTable.insertSymbol(symbol);

    emitAssign(symbol, exp, symbolTable.pRegisterStackVar);
}

void tryAssignExp(symbol_table &symbolTable, shared_ptr<symbolGen> rawId, shared_ptr<symbolGen> rawExp)
{
    string id = (dynamic_pointer_cast<SymbolType<string>>(rawId)->getValue());
    shared_ptr<symbolId> symbol = symbolTable.getVarSymbol(id);
    shared_ptr<symbolExp> exp = (dynamic_pointer_cast<symbolExp>(rawExp));

    string symbolGen = symbol->getType();
    string expType = exp->getType();

    if (not strTypesCompareCheck(symbol->getType(), exp->getType()))
        errorMismatch(yylineno);

    emitAssign(symbol, exp, symbolTable.pRegisterStackVar);
}

void emitAssign(shared_ptr<symbolId> symbol, shared_ptr<symbolExp> exp, string pRegisterStackVar)
{
    CodeBuffer &codeBuffer = CodeBuffer::instance();
    Ralloc &ralloc = Ralloc::instance();

    string llvmLvalType = typeNameToLlvmType(symbol->getType());
    string llvmRvalType = typeNameToLlvmType(exp->getType());
    string offsetReg = ralloc.getNextRegister("offsetEmitAssign_" + symbol->getIdName());
    string idAddrReg = ralloc.getNextRegister("idAddrEmitAssign_" + symbol->getIdName());
    string expReg = exp->getRegOrImmResult();

    codeBuffer.emit(offsetReg + " = add i32 0, " + std::to_string(symbol->getOffset()));
    codeBuffer.emit(idAddrReg + " = getelementptr i32, i32* " + pRegisterStackVar + ", i32 " + offsetReg);

    string idAddrRegCorrectSize = idAddrReg;

    if (llvmRvalType != llvmLvalType)
    {
        string zextExpReg = ralloc.getNextRegister("zextEmitAssign_" + symbol->getIdName());
        codeBuffer.emit(zextExpReg + " = zext " + llvmRvalType + " " + expReg + " to " + llvmLvalType);
        expReg = zextExpReg;
    }

    if (llvmLvalType != "i32")
    {
        idAddrRegCorrectSize = ralloc.getNextRegister("idAddrCorrect_" + symbol->getIdName());
        codeBuffer.emit(idAddrRegCorrectSize + " = bitcast i32* " + idAddrReg + " to " + llvmLvalType + "*");
    }

    codeBuffer.emit("store " + llvmLvalType + " " + expReg + ", " + llvmLvalType + "* " + idAddrRegCorrectSize);
}

void handleReturn(shared_ptr<symbolReturn> retType)
{
    if (retType == nullptr)
        throw Exception("This should be impossible. Syntax error wise");
    else if (retType->getTypeName() != "VOID")
        errorMismatch(yylineno);

    emitReturn(retType, nullptr);
}

void handleReturnExp(shared_ptr<symbolReturn> retType, shared_ptr<symbolGen> rawExp)
{
    shared_ptr<symbolExp> exp = (dynamic_pointer_cast<symbolExp>(rawExp));
    exp->getRegOrImmResult();

    if (retType == nullptr)
        throw Exception("This should be impossible. Syntax error wise");
    else if (not strTypesCompareCheck(retType->getTypeName(), exp->getType()))
        errorMismatch(yylineno);

    emitReturn(retType, exp);
}

void emitReturn(shared_ptr<symbolReturn> retType, shared_ptr<symbolExp> exp)
{
    CodeBuffer &codeBuffer = CodeBuffer::instance();

    if (exp == nullptr)
    {
        if (retType->getTypeName() != "VOID")
            errorMismatch(yylineno);
        codeBuffer.emit("ret void");
        return;
    }

    if (retType->getTypeName() != exp->getType())
        throw Exception("this should be impossible. wrong function usage.");

    string llvmType = typeNameToLlvmType(exp->getType());
    codeBuffer.emit("ret " + llvmType + " " + exp->getRegOrImmResult());
}
