#include <stdlib.h>
#include <iostream>
#include <sstream>

#include "hw3_output.hpp"
using namespace std;

void output::endScope()
{
    cout << "---end scope---" << endl;
}

void output::printID(const string &id, int offset, const string &type)
{
    cout << id << " " << type << " " << offset << endl;
}

string typeListToString(const std::vector<string> &argTypes)
{
    stringstream res;
    res << "(";
    for (int i = argTypes.size()-1; i >= 0; --i)
    {
        res << argTypes[i];
        if (i > 0)
            res << ",";
    }
    res << ")";
    return res.str();
}

string valueListsToString(const std::vector<string> &values)
{
    stringstream res;
    res << "{";
    for (int i = 0; i < values.size(); ++i)
    {
        res << values[i];
        if (i + 1 < values.size())
            res << ",";
    }
    res << "}";
    return res.str();
}

string output::makeFunctionType(const string &retType, std::vector<string> &argTypes)
{
    stringstream res;
    res << typeListToString(argTypes) << "->" << retType;
    return res.str();
}

void output::errorLex(int lineno)
{
    cout << "line " << lineno << ":"
         << " lexical error" << endl;
}

void output::errorSyn(int lineno)
{
    cout << "line " << lineno << ":"
         << " syntax error" << endl;
}

void output::errorUndef(int lineno, const string &id)
{
    cout << "line " << lineno << ":"
         << " variable " << id << " is not defined" << endl;
}

void output::errorDef(int lineno, const string &id)
{
    cout << "line " << lineno << ":"
         << " identifier " << id << " is already defined" << endl;
}

void output::errorUndefFunc(int lineno, const string &id)
{
    cout << "line " << lineno << ":"
         << " function " << id << " is not defined" << endl;
}

void output::errorMismatch(int lineno)
{
    cout << "line " << lineno << ":"
         << " type mismatch" << endl;
}

void output::errorPrototypeMismatch(int lineno, const string &id, std::vector<string> &argTypes)
{
    cout << "line " << lineno << ": prototype mismatch, function " << id << " expects arguments " << typeListToString(argTypes) << endl;
}

void output::errorUnexpectedBreak(int lineno)
{
    cout << "line " << lineno << ":"
         << " unexpected break statement" << endl;
}

void output::errorUnexpectedContinue(int lineno)
{
    cout << "line " << lineno << ":"
         << " unexpected continue statement" << endl;
}

void output::errorMainMissing()
{
    cout << "Program has no 'void main()' function" << endl;
}

void output::errorByteTooLarge(int lineno, const string &value)
{
    cout << "line " << lineno << ": byte value " << value << " out of range" << endl;
}

int yyerror(const char *message)
{
    output::errorSyn(yylineno);

    return -1;
}
