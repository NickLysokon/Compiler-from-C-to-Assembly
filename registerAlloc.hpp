#ifndef RALLOC_H_
#define RALLOC_H_

#include <string>

// class for dynamically allocating LLVM registers
class Ralloc {
   private:
    int nextReg;

    // Copy Constructor
    Ralloc(const Ralloc &) = delete;
    // Constructor
    Ralloc();

   public:
    // Get the next available register
    std::string getNextRegister(const std::string &prefix = "reg");
    // Get the singleton instance
    static Ralloc &instance();
    // Get the next available variable name
    std::string getNextVarName();
};

#endif