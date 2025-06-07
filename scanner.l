%option noyywrap
%{
#include "parser.tab.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
%}

DIGIT      [0-9]
ID_START   [A-Za-z_]
ID_CONT    [A-Za-z0-9_]
ID         {ID_START}{ID_CONT}*
REAL       {DIGIT}+"."{DIGIT}+
HEX        0[xX][0-9A-Fa-f]+

%x COMMENT

%%

[ \t\r\n]+                  /* skip whitespace */
"//".*                      /* skip single line comments */

"/#->"                      { BEGIN(COMMENT); }
<COMMENT>"<-#/"             { BEGIN(INITIAL); }
<COMMENT>.                  /* ignore comment content */
<COMMENT>\n                 /* ignore newlines in comments */

"def"                       return DEF;
"returns"                   return RETURNS;
"var"                       return VAR;
"type"                      return TYPE;
"begin"                     return T_BEGIN;
"end"                       return T_END;
"if"                        return IF;
"elif"                      return ELIF;
"else"                      return ELSE;
"while"                     return WHILE;
"do"                        return DO;
"for"                       return FOR;
"call"                      return CALL;
"return"                    return RETURN;
"null"                      return NULL_T;
"to"                        return TO;
"and"                       return AND_AND;
"or"                        return OR_OR;

"TRUE"                      { yylval.bval = 1; return BOOL_LIT; }
"FALSE"                     { yylval.bval = 0; return BOOL_LIT; }
"true"                      { yylval.bval = 1; return BOOL_LIT; }
"false"                     { yylval.bval = 0; return BOOL_LIT; }

{REAL}                      { yylval.rval = atof(yytext); return REAL_LIT; }
{HEX}                       { yylval.ival = strtol(yytext, NULL, 0); return INT_LIT; }
{DIGIT}+                    { yylval.ival = atoi(yytext); return INT_LIT; }

"=="                        return EQ;
"!="                        return NEQ;
">="                        return GE;
"<="                        return LE;
"&&"                        return AND_AND;
"||"                        return OR_OR;
">"                         return GT;
"<"                         return LT;
"="                         return ASSIGN;
";"                         return SEMI;
":"                         return COLON;
","                         return COMMA;
"("                         return LPAREN;
")"                         return RPAREN;
"["                         return LBRACK;
"]"                         return RBRACK;
"!"                         return NOT;
"|"                         return BAR;

"+"                         return '+';
"-"                         return '-';
"*"                         return '*';
"/"                         return '/';
"&"                         return '&';

\"([^\\\"]|\\.)*\"          { yylval.str = strdup(yytext); return STRING_LIT; }
\'([^\\\'\n]|\\.)\'         { yylval.str = strdup(yytext); return CHAR_LIT; }

{ID}                        { yylval.str = strdup(yytext); return ID; }

.                           { fprintf(stderr, "Lexical error: Unknown character '%s'\n", yytext); exit(1); }

%%