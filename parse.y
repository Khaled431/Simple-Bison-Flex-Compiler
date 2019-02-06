%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;

%}

%union {tokentype token;
        regInfo targetReg;
        IdentifierNode* iNode;
        TypeInfo typeInfo;
        Type_Expression sTypeInfo;
        ConditionExpr exprInfo;
        ControlExit controlInfo;
       }

%token PROG PERIOD VAR
%token INT BOOL PRINT THEN IF DO
%token ARRAY OF
%token BEG END ASG
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token ELSE
%token WHILE
%token <token> ID ICONST

%type <targetReg> exp
%type <targetReg> lhs

%type <iNode> idlist;
%type <typeInfo> type;
%type <sTypeInfo> stype;
%type <exprInfo> ifhead condexp;

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%%
program : {emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
           emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);}
           PROG ID ';' block PERIOD { }
        ;

block   : variables cmpdstmt { }
        ;

variables: /* empty */
        | VAR vardcls { }
        ;

vardcls : vardcls vardcl ';' { }
        | vardcl ';' { }
        | error ';' { yyerror("***Error: illegal variable declaration\n");}
        ;

vardcl  : idlist ':' type {
                           IdentifierNode* node = $1;
                           while(node) {
                               insert(node->str, $3.type, NextOffset($3.numElements));
                               node = node->next;
                           }
                          }
        ;

idlist  : idlist ',' ID {
                            size_t len = strlen($3.str);
                            char* str = (char*) malloc(sizeof(char) * (len + 1));
                            if (str) {
                                strncpy(str, $3.str, len);

                                IdentifierNode* finalNode;
                                for (finalNode = $1; finalNode->next; finalNode = finalNode->next);

                                IdentifierNode* next = (IdentifierNode*) malloc(sizeof(IdentifierNode));
                                if (next) {
                                    next->str = str;

                                    finalNode->next = next;
                                }
                            }
                        }

        | ID            {
                            size_t length = strlen($1.str);
                            char* str = (char*) malloc(sizeof(char) * (length + 1));
                            if (str) {
                                strncpy(str, $1.str, length);

                                IdentifierNode* node = (IdentifierNode*) malloc(sizeof(IdentifierNode));
                                if (node) {
                                    node->str = str;
                                    $$ = node;
                                }
                            }
                        }
        ;


type    : ARRAY '[' ICONST ']' OF stype { $$.type = $6; $$.numElements = $3.num; }

        | stype { $$.type = $1; $$.numElements = 1;}
        ;

stype   : INT { $$ = TYPE_INT; }
        | BOOL { $$ = TYPE_BOOL; }
        ;

stmtlist : stmtlist ';' stmt { }
        | stmt { }
        | error { yyerror("***Error: ';' expected or illegal statement \n");}
        ;

stmt    : ifstmt {

                 }
        | wstmt {

                }
        | astmt {
                }
        | writestmt {

                    }
        | cmpdstmt {

                   }
        ;

cmpdstmt: BEG stmtlist END
        ;

ifstmt : ifhead THEN stmt ELSE {
                                   $<controlInfo>1.exitLabel = NextLabel();

                                   emit(NOLABEL, BR, $<controlInfo>1.exitLabel, EMPTY, EMPTY);
                                   emit($1.falseBranch,  EMPTY, EMPTY, EMPTY, EMPTY);
                               }
                            stmt {
                                    emit($<controlInfo>1.exitLabel, EMPTY, EMPTY, EMPTY, EMPTY);
                                 }
        ;

ifhead : IF condexp {
                      $$.trueBranch = $2.trueBranch;
                      $$.falseBranch = $2.falseBranch;

                      emit($$.trueBranch, EMPTY, EMPTY, EMPTY, EMPTY);
                    }
        ;

writestmt: PRINT '(' exp ')' {
                                 int printOffset = -4; /* default location for printing */
                                 emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
                                 emit(NOLABEL, OUTPUTAI, 0, printOffset, EMPTY);
                               }
        ;

wstmt   : WHILE  {
                    $<controlInfo>1.exitLabel = NextLabel();
                    emit($<controlInfo>1.exitLabel, EMPTY, EMPTY, EMPTY, EMPTY);
                 }
          condexp {
                    emit($3.trueBranch, EMPTY, EMPTY, EMPTY, EMPTY);
                  }
          DO stmt  {
                     emit(NOLABEL, BR, $<controlInfo>1.exitLabel, EMPTY, EMPTY);
                     emit($3.falseBranch, EMPTY, EMPTY, EMPTY, EMPTY);
                   }
        ;


astmt : lhs ASG exp             {
                                  if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ||
                                         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
                                    printf("*** ERROR ***: Assignment types do not match. %d %d \n", $1.type, $3.type);
                                  }

                                  emit(NOLABEL, STORE, $3.targetRegister, $1.targetRegister, EMPTY);
                                }
        ;

lhs     : ID                    {

                                  SymTabEntry* entry = lookup($1.str);
                                  int newReg1 = NextRegister();
                                  int newReg2 = NextRegister();

                                  $$.targetRegister = newReg2;

                                  if (entry) {
                                    $$.type = entry->type;

                                    emit(NOLABEL, LOADI, entry->offset, newReg1, EMPTY);
                                    emit(NOLABEL, ADD, EMPTY, newReg1, newReg2);
                                  }
                                 }


                                |  ID '[' exp ']' {
                                       if ($3.type != TYPE_INT) {
                                          printf("\n*** ERROR ***: Array variable %s %d index type must be integer.\n", $1.str, $3.type);
                                       }
                                        SymTabEntry* entry = lookup($1.str);

                                        int newReg = NextRegister();
                                        int constFourReg = NextRegister();
                                        int offsetReg = NextRegister();
                                        int baseAddrReg = NextRegister();
                                        int relativeAddrReg = NextRegister();

                                        $$.targetRegister = newReg;

                                        if (entry) {
                                           $$.type = entry->type;

                                          emit(NOLABEL, LOADI, 4, constFourReg, EMPTY);
                                          emit(NOLABEL, MULT,  $3.targetRegister, constFourReg, offsetReg);
                                          emit(NOLABEL, LOADI, entry->offset, baseAddrReg, EMPTY);
                                          emit(NOLABEL, ADD, baseAddrReg, offsetReg, relativeAddrReg);
                                          emit(NOLABEL, ADD, 0, relativeAddrReg, newReg);
                                        } else {
                                            printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                                        }
                                   }
                                ;


exp     : exp '+' exp           {
                                  int newReg = NextRegister();
                                  if ($1.type != TYPE_INT || $3.type != TYPE_INT) {
                                    printf("*** ERROR ***: Operator types must be integer. %d %d \n", $1.type, $3.type);
                                  }

                                  $$.type = $1.type;
                                  $$.targetRegister = newReg;

                                  emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
                                }

        | exp '-' exp           {
                                  int newReg = NextRegister();
                                  if ($1.type != TYPE_INT || $3.type != TYPE_INT) {
                                    printf("*** ERROR ***: Operator types must be integer.\n");
                                  }

                                  $$.type = $1.type;
                                  $$.targetRegister = newReg;

                                  emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
                                }

        | exp '*' exp           {
                                  int newReg = NextRegister();
                                  if ($1.type != TYPE_INT || $3.type != TYPE_INT) {
                                     printf("*** ERROR ***: Operator types must be integer.\n");
                                  }

                                  $$.type = $1.type;
                                  $$.targetRegister = newReg;

                                  emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
                                }

        | exp AND exp          {
                                int newReg = NextRegister();
                                if (! ( ($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL) )) {
                                    printf("*** ERROR ***: Operator types must be boolean.\n");
                                }

                                $$.type = $1.type;
                                $$.targetRegister = newReg;

                                emit(NOLABEL, AND_INSTR, $1.targetRegister, $3.targetRegister, newReg);
                               }


        | exp OR exp            {
                                 int newReg = NextRegister();
                                 if (! ( ($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL) )) {
                                     printf("*** ERROR ***: Operator types must be boolean.\n");
                                 }

                                 $$.type = $1.type;
                                 $$.targetRegister = newReg;

                                 emit(NOLABEL, OR_INSTR, $1.targetRegister, $3.targetRegister, newReg);
                                }

        | ID                    {
                                  SymTabEntry* entry = lookup($1.str);
                                  int newReg = NextRegister();

                                  $$.targetRegister = newReg;

                                  if (entry) {
                                        $$.type = entry->type;

                                        emit(NOLABEL, LOADAI, EMPTY, entry->offset, newReg);
                                  } else {
                                      printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                                  }
                                }

        | ID '[' exp ']'        {
                                    int newReg = NextRegister();
                                    int constFourReg = NextRegister();
                                    int offsetReg = NextRegister();
                                    int baseAddrReg = NextRegister();
                                    int relativeAddrReg = NextRegister();

                                    SymTabEntry * entry = lookup($1.str);

                                    $$.targetRegister = newReg;

                                    if (entry) {
                                        $$.type = entry->type;

                                        emit(NOLABEL, LOADI, 4, constFourReg, EMPTY);
                                        emit(NOLABEL, MULT, $3.targetRegister, constFourReg, offsetReg);
                                        emit(NOLABEL, LOADI, entry->offset, baseAddrReg, EMPTY);
                                        emit(NOLABEL, ADD, baseAddrReg, offsetReg, relativeAddrReg);
                                        emit(NOLABEL, LOADAO, 0, relativeAddrReg, newReg);
                                    } else {
                                        printf("\n*** ERROR ***: Variable %s not declared.\n", $1.str);
                                    }
                                }

        | ICONST                 {
                                   int newReg = NextRegister();

                                   $$.targetRegister = newReg;
                                   $$.type = TYPE_INT;

                                   emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);
                                 }

        | TRUE                   {
                                   int newReg = NextRegister(); /* TRUE is encoded as value '1' */

                                   $$.targetRegister = newReg;
                                   $$.type = TYPE_BOOL;

                                   emit(NOLABEL, LOADI, 1, newReg, EMPTY);
                                 }

        | FALSE                  {
                                   int newReg = NextRegister(); /* TRUE is encoded as value '0' */

                                   $$.targetRegister = newReg;
                                   $$.type = TYPE_BOOL;

                                   emit(NOLABEL, LOADI, 0, newReg, EMPTY);
                                 }

        | error { yyerror("***Error: illegal expression\n");}
        ;


condexp : exp NEQ exp           {
                                    int newReg = NextRegister();\

                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: != operator with different types.");
                                    }

                                    emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, newReg);
                                    emit(NOLABEL, CBR, newReg, $$.trueBranch, $$.falseBranch);
                                }

        | exp EQ exp            {
                                    int newReg = NextRegister();
                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: == operator with different types.");
                                    }

                                     emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, newReg);
                                     emit(NOLABEL, CBR, newReg, $$.trueBranch, $$.falseBranch);
                                }

        | exp LT exp            {
                                    int newReg = NextRegister();
                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: < operator with different types.");
                                    }

                                     emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, newReg);
                                     emit(NOLABEL, CBR, newReg, $$.trueBranch, $$.falseBranch);
                                }

        | exp LEQ exp           {
                                    int newReg = NextRegister();
                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: <= operator with different types.");
                                    }

                                     emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, newReg);
                                     emit(NOLABEL, CBR, newReg,  $$.trueBranch, $$.falseBranch);
                                }

        | exp GT exp            {
                                    int newReg = NextRegister();
                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: > operator with different types.");
                                    }

                                     emit(NOLABEL, CMPGT, $1.targetRegister, $3.targetRegister, newReg);
                                     emit(NOLABEL, CBR, newReg,  $$.trueBranch,  $$.falseBranch);
                                }

        | exp GEQ exp           {
                                    int newReg = NextRegister();
                                    $$.trueBranch = NextLabel();
                                    $$.falseBranch = NextLabel();

                                    if ($1.type != $3.type) {
                                         puts("\n*** ERROR ***: >= operator with different types.");
                                    }

                                     emit(NOLABEL, CMPGE, $1.targetRegister, $3.targetRegister, newReg);
                                     emit(NOLABEL, CBR, newReg, $$.trueBranch,  $$.falseBranch);
                                }

        | error { yyerror("***Error: illegal conditional expression\n");}
        ;
%%

void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
        }


int
main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2018 Compiler\n\n");

  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) {
    printf("ERROR: cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(650);
  InitSymbolTable();

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();

  fclose(outfile);

  return 1;
}