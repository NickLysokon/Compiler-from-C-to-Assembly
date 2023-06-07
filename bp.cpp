#include "bp.hpp"

#include <iostream>
#include <sstream>
#include <vector>

#include "registerAlloc.hpp"
#include "symbol_table.hpp"

extern int yylineno;
using namespace std;

extern symbol_table symbolTable;


//extern Generator code_gen;

bool replace(string& str, const string& from, const string& to, const BranchLabelIndex index);

CodeBuffer::CodeBuffer() : buffer(), globalDefs() {}

CodeBuffer &CodeBuffer::instance() {
	static CodeBuffer inst;//only instance
	return inst;
}

string CodeBuffer::genLabel(const string& prefix){
    std::stringstream label;
    if (prefix != "") {
        label << prefix << "_label_";
    } else {
        label << "label_";
    }
    label << buffer.size();
    std::string ret(label.str());
    label << ":";
    emit("br label %" + ret, true);
    emit(label.str());
    return ret;
}

int CodeBuffer::emit(const string& s, bool canSkip) {
    int indentationDepth = symbolTable.getCurrentDomainDepth();
    string indentationStrDef = "";
    if (indentationDepth > 0){
        string indentationStr(indentationDepth, '\t');
        indentationStrDef = indentationStr;
    }
    
    // Skip both this and prev emit start with br
    if (canSkip and s.substr(0, 9) == "br label " and (buffer.back().find("br ") != string::npos or buffer.back().find("ret ") != string::npos)) {
        return -1;
    }

    buffer.push_back(indentationStrDef + s + " ; " + to_string(yylineno));
    return buffer.size() - 1;
}

void CodeBuffer::bpatch(vector<pair<int, BranchLabelIndex>>& address_list, const std::string& label) {
    for (vector<pair<int, BranchLabelIndex>>::const_iterator i = address_list.begin(); i != address_list.end(); i++) {
        int address = (*i).first;
        if (address == -1) {
            continue;
        }
        BranchLabelIndex labelIndex = (*i).second;
        replace(buffer[address], "@", "%" + label, labelIndex);
    }
    address_list.clear();
}

void CodeBuffer::bpatch(pair<int, BranchLabelIndex> pair, const std::string& label) {
    int address = pair.first;
    BranchLabelIndex labelIndex = pair.second;
    replace(buffer[address], "@", "%" + label, labelIndex);
}

void CodeBuffer::printCodeBuffer(){
	for (std::vector<string>::const_iterator it = buffer.begin(); it != buffer.end(); ++it) 
	{
		cout << *it << endl;
    }
}

vector<pair<int,BranchLabelIndex>> CodeBuffer::makelist(pair<int,BranchLabelIndex> item)
{
	vector<pair<int,BranchLabelIndex>> newList;
	newList.push_back(item);
	return newList;
}

vector<pair<int,BranchLabelIndex>> CodeBuffer::merge(const vector<pair<int,BranchLabelIndex>> &l1,const vector<pair<int,BranchLabelIndex>> &l2)
{
	vector<pair<int,BranchLabelIndex>> newList(l1.begin(),l1.end());
	newList.insert(newList.end(),l2.begin(),l2.end());
	return newList;
}

// ******** Methods to handle the global section ********** //
void CodeBuffer::emitGlobal(const std::string& dataLine) 
{
	globalDefs.push_back(dataLine);
}

void CodeBuffer::printGlobalBuffer()
{
	for (vector<string>::const_iterator it = globalDefs.begin(); it != globalDefs.end(); ++it)
	{
		cout << *it << endl;
	}
}

// ******** Helper Methods ********** //
bool replace(string& str, const string& from, const string& to, const BranchLabelIndex index) {
	size_t pos;
	if (index == SECOND) {
		pos = str.find_last_of(from);
	}
	else { //FIRST
		pos = str.find_first_of(from);
	}
    if (pos == string::npos)
        return false;
    str.replace(pos, from.length(), to);
    return true;
}
