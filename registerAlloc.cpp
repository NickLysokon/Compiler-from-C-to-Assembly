#include "registerAlloc.hpp"
using std::string;

Ralloc::Ralloc() : nextReg(1) {}

// Get the singleton object instance
Ralloc &Ralloc::instance() {
    static Ralloc instance;
    return instance;
}

// Get the next register
string Ralloc::getNextRegister(const string &prefix) {
    return "%" + prefix + (prefix == "" ? "" : "_") + std::to_string(nextReg++);
}

string Ralloc::getNextVarName() {
    return "@." + std::to_string(nextReg++);
}