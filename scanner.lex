%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "symbol_table.hpp"
    #include "hw3_output.hpp"
    #include "parser.tab.hpp"



using namespace output;

%}

%option yylineno
%option noyywrap

digit           ([0-9])
digit_no_zero     ([1-9])
letter          ([a-zA-Z])
whitespace      ([\t\n\r ])
escape     ([\\"nrt0])


%%
{whitespace}                                    ;
(\/\/[^\r\n]*[ \r|\n|\r\n]?)                    {;}
(int)                                           {return INT;}
(void)                                          {return VOID;}
(b)                                             {return B;}
(byte)                                          {return BYTE;}
(bool)                                          {return BOOL;}
(or)                                            {return OR;}
(and)                                           {return AND;}
(not)                                           {return NOT;}
(false)                                         {return FALSE;}
(true)                                          {return TRUE;}
(return)                                        {return RETURN;}
(else)                                          {return ELSE;}
(if)                                            {return IF;}
(break)                                         {return BREAK;}
(while)                                         {return WHILE;}
(continue)                                      {return CONTINUE;}
(auto)                                          {return AUTO;}
(\{)                                            {return LBRACE;}
(\,)                                            {return COMMA;}
(\))                                            {return RPAREN;}
(\()                                            {return LPAREN;}
(\})                                            {return RBRACE;}
(\;)                                            {return SC;}
(=)                                             {return ASSIGN;}
(\>=)                                           {return GEOP;}
(\>)                                            {return GTOP;}
(\<=)                                           {return LEOP;}
(\<)                                            {return LTOP;}
((==))                                          {return EQOP;}
((!=))                                          {return NEOP;}
(\+)                                            {return ADDOP;}
(\-)                                            {return SUBOP;}
(\*)                                            {return MULOP;}
(\/)                                            {return DIVOP;}
(0{digit}+)                                     {output::errorLex(yylineno);}
({letter}({letter}|{digit})*)                   {yylval = (std::shared_ptr<SymbolType<std::string> >(new SymbolType<std::string>(std::string (yytext)))); return ID;}
(0|{digit_no_zero}{digit}*)                     {yylval = (std::shared_ptr<SymbolType<std::string> >(new SymbolType<std::string>(std::string (yytext)))); return NUM;}
(\"([^\n\r\"\\]|\\[rnt"\\])+\")                 {yylval = (std::shared_ptr<SymbolType<std::string> >(new SymbolType<std::string>(std::string (yytext)))); return STRING;}
.                                               {output::errorLex(yylineno);}
%%