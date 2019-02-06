/**********************************************
        CS415  Project 2
        Spring  2015
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H

typedef union {
    int num;
    char *str;
} tokentype;

typedef enum type_expression {
    TYPE_INT = 0, TYPE_BOOL, TYPE_ERROR
} Type_Expression;

typedef struct {
    Type_Expression type;
    int targetRegister;
} regInfo;

typedef struct {
    char *str;
    struct IdentifierNode *next;
} IdentifierNode;

typedef struct {
    int trueBranch;
    int falseBranch;
} ConditionExpr;

typedef struct {
    int exitLabel;
} ControlExit;

typedef struct {
    Type_Expression type;
    int numElements;
} TypeInfo;

#endif


  
