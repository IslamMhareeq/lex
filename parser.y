/* parser.y - Updated for test file syntax */

%define parse.error verbose
%expect 10

/*----------------------------------------------------------------------*/
/* C declarations visible to parser and actions                         */
/*----------------------------------------------------------------------*/
%code requires {
  #include <stdlib.h>
  #include <string.h>
  #include <ctype.h>

  typedef enum {
    N_FUNC, N_PARS, N_PARAM, N_RET,
    N_BLOCK, N_IFELSE, N_ASSIGN, N_BINOP,
    N_UNOP, N_CALL, N_WHILE, N_DOWHILE, N_FOR, N_INDEX,
    N_ID, N_LIT
  } NodeType;

  typedef struct AST {
    NodeType      type;
    char         *lexeme;
    char         *typ;
    int            n;
    struct AST  **c;
  } AST;

  static char *cur_func_name = NULL;
  static AST  *cur_param_ast  = NULL;

  typedef enum { SYM_VAR, SYM_FUNC, SYM_PARAM } SymKind;
  typedef struct Sym {
    char      *name;
    SymKind    kind;
    char      *type;
    int        param_count;
    char     **param_types;
    struct Sym *next;
  } Sym;

  typedef struct Scope {
    Sym         *symbols;
    struct Scope *next;
  } Scope;
}

/*----------------------------------------------------------------------*/
/* C helper functions and globals                                       */
/*----------------------------------------------------------------------*/
%code {
  #include <stdio.h>
  #include <stdarg.h>
  #include "parser.tab.h"
  void print_node(AST *z, int d);
  void free_node(AST *z);

  static Scope *scope_stack = NULL;
  static char  *cur_ret_type = NULL;
  static int main_count = 0;
  static int parsing_functions = 1;

  typedef struct FuncDecl {
    char *name;
    int param_count;
    char **param_types;
    char *return_type;
    struct FuncDecl *next;
  } FuncDecl;
  
  static FuncDecl *func_declarations = NULL;
  
  typedef struct UnresolvedCall {
    char *func_name;
    int param_count;
    char **param_types;
    struct UnresolvedCall *next;
  } UnresolvedCall;
  
  static UnresolvedCall *unresolved_calls = NULL;
  
  void add_unresolved_call(const char *name, int pc, char **ptypes) {
    UnresolvedCall *uc = malloc(sizeof *uc);
    uc->func_name = strdup(name);
    uc->param_count = pc;
    if (pc > 0) {
      uc->param_types = malloc(pc * sizeof(char*));
      for (int i = 0; i < pc; i++) {
        uc->param_types[i] = strdup(ptypes[i]);
      }
    } else {
      uc->param_types = NULL;
    }
    uc->next = unresolved_calls;
    unresolved_calls = uc;
  }
  
  void validate_unresolved_calls();
  
  void add_func_declaration(const char *name, int pc, char **ptypes, const char *ret_type) {
    FuncDecl *fd = malloc(sizeof *fd);
    fd->name = strdup(name);
    fd->param_count = pc;
    if (pc > 0) {
      fd->param_types = malloc(pc * sizeof(char*));
      for (int i = 0; i < pc; i++) {
        fd->param_types[i] = strdup(ptypes[i]);
      }
    } else {
      fd->param_types = NULL;
    }
    fd->return_type = strdup(ret_type);
    fd->next = func_declarations;
    func_declarations = fd;
  }
  
  FuncDecl *find_func_declaration(const char *name) {
    for (FuncDecl *fd = func_declarations; fd; fd = fd->next) {
      if (!strcmp(fd->name, name)) return fd;
    }
    return NULL;
  }

  void enter_scope() {
    Scope *s = malloc(sizeof *s);
    s->symbols = NULL;
    s->next    = scope_stack;
    scope_stack = s;
  }

  void exit_scope() {
    Scope *s = scope_stack;
    if (s) {
      scope_stack = s->next;
      free(s);
    }
  }

  Sym *lookup(const char *name) {
    for (Scope *sc = scope_stack; sc; sc = sc->next)
      for (Sym *p = sc->symbols; p; p = p->next)
        if (!strcmp(p->name,name)) return p;
    return NULL;
  }

  Sym *lookup_current_scope(const char *name) {
    if (!scope_stack) return NULL;
    for (Sym *p = scope_stack->symbols; p; p = p->next)
      if (!strcmp(p->name, name)) return p;
    return NULL;
  }

  void add_symbol(const char *name, SymKind kind,
                  const char *type, int pcount, char **ptypes)
  {
    if (lookup_current_scope(name)) {
      if (kind == SYM_FUNC) {
        fprintf(stderr,"Error: Function `%s` already declared in this scope\n",name);
      } else {
        fprintf(stderr,"Error: Variable `%s` already declared in this scope\n",name);
      }
      exit(1);
    }
    
    Sym *s = malloc(sizeof *s);
    s->name        = strdup(name);
    s->kind        = kind;
    s->type        = strdup(type);
    s->param_count = pcount;
    if (pcount>0) {
      s->param_types = malloc(pcount*sizeof(char*));
      for (int i = 0; i < pcount; i++) {
        s->param_types[i] = strdup(ptypes[i]);
      }
    } else s->param_types = NULL;
    s->next = scope_stack->symbols;
    scope_stack->symbols = s;
  }

  int types_compatible(const char *expected, const char *actual) {
    if (strcmp(expected, actual) == 0) return 1;
    if (strcmp(expected, "REAL") == 0 && strcmp(actual, "INT") == 0) return 1;
    if (strcmp(actual, "PTR(ANY)") == 0 && strncmp(expected, "PTR(", 4) == 0) return 1;
    return 0;
  }

  void validate_unresolved_calls() {
    for (UnresolvedCall *uc = unresolved_calls; uc; uc = uc->next) {
      Sym *s = lookup(uc->func_name);
      FuncDecl *fd = find_func_declaration(uc->func_name);
      
      if ((!s || s->kind != SYM_FUNC) && !fd) {
        fprintf(stderr, "Error: Function `%s` not defined\n", uc->func_name);
        exit(1);
      }
      
      int expected_params = (s && s->kind == SYM_FUNC) ? s->param_count : fd->param_count;
      char **expected_types = (s && s->kind == SYM_FUNC) ? s->param_types : fd->param_types;
      
      if (expected_params != uc->param_count) {
        fprintf(stderr,"Error: Function `%s` expects %d parameters, got %d\n",
                uc->func_name, expected_params, uc->param_count);
        exit(1);
      }
      
      for(int i = 0; i < uc->param_count; i++) {
        if (strcmp(uc->func_name, "print") == 0 && i == 0 && 
            (strcmp(uc->param_types[i], "INT") == 0 || strcmp(uc->param_types[i], "REAL") == 0)) {
          continue;
        }
        
        if(!types_compatible(expected_types[i], uc->param_types[i])){
          fprintf(stderr,"Error: Parameter %d of function `%s` should be %s, got %s\n",
                  i+1, uc->func_name, expected_types[i], uc->param_types[i]);
          exit(1);
        }
      }
    }
  }

  AST *ast_new(NodeType t, const char *lex, int n, ...) {
    AST *z = malloc(sizeof *z);
    z->type   = t;
    z->lexeme = lex ? strdup(lex) : NULL;
    z->typ    = NULL;
    z->n      = n;
    z->c      = n ? malloc(n * sizeof(AST*)) : NULL;
    va_list ap; va_start(ap,n);
    for(int i=0;i<n;i++) z->c[i] = va_arg(ap,AST*);
    va_end(ap);
    return z;
  }

  extern int yylex(void);
  void yyerror(const char *s){
    fprintf(stderr,"Parse error: %s\n",s);
    exit(1);
  }
}

/*----------------------------------------------------------------------*/
/* Semantic‐value union                                                 */
/*----------------------------------------------------------------------*/
%union {
  int     ival;
  char   *str;
  AST    *node;
  double  rval;
  int     bval;
}

/*----------------------------------------------------------------------*/
/* Token declarations                                                  */
/*----------------------------------------------------------------------*/
%token <str>    ID CHAR_LIT STRING_LIT
%token <ival>   INT_LIT
%token <rval>   REAL_LIT
%token <bval>   BOOL_LIT

%token          DEF RETURNS VAR TYPE
%token          T_BEGIN T_END
%token          IF ELIF ELSE WHILE DO FOR CALL RETURN NULL_T

%token          EQ NEQ GE LE GT LT ASSIGN
%token          SEMI COLON COMMA LPAREN RPAREN TO
%token          AND_AND OR_OR NOT LBRACK RBRACK BAR

/*----------------------------------------------------------------------*/
/* Operator precedence                                                  */
/*----------------------------------------------------------------------*/
%nonassoc ELSE
%right ASSIGN
%left OR_OR
%left AND_AND
%left EQ NEQ
%left GT LT GE LE
%left '+' '-'
%left '*' '/'
%right UMINUS UPLUS NOT
%left LBRACK
%left LPAREN

/*----------------------------------------------------------------------*/
/* Nonterminal‐type declarations                                        */
/*----------------------------------------------------------------------*/
%type <node>
    params
    nonempty_params
    param
    type
    stmt_list
    stmt
    var_decl
    var_list
    args
    nonempty_args
    expr
    primary_expr
    if_stmt
    elif_chain
    variable
%%

/*----------------------------------------------------------------------*/
/* Top‐level "program"                                                  */
/*----------------------------------------------------------------------*/
program
  : /* empty */
    {
      enter_scope();
      char *pt0[1] = { "INT" };
      add_symbol("print", SYM_FUNC, "NONE", 1, pt0);
    }
    declarations
    functions
    {
      parsing_functions = 0;
      
      if (main_count == 0) {
        fprintf(stderr, "Error: No main function found\n");
        exit(1);
      }
      if (main_count > 1) {
        fprintf(stderr, "Error: Multiple main functions found\n");
        exit(1);
      }
      
      validate_unresolved_calls();
      exit_scope();
    }
  ;

/*----------------------------------------------------------------------*/
/* Global declarations                                                  */
/*----------------------------------------------------------------------*/
declarations
  : /* empty */
  | declarations var_decl
  ;

/*----------------------------------------------------------------------*/
/* Variable declarations                                                */
/*----------------------------------------------------------------------*/
var_decl
  : VAR TYPE type COLON var_list SEMI
    {
      /* Add each variable in the list to symbol table */
      for (int i = 0; i < $5->n; i++) {
        AST *var_node = $5->c[i];
        char *var_name = var_node->lexeme;
        
        /* Handle array declarations like s1[100] */
        if (var_node->n > 0) {
          /* This is an array declaration */
          char *array_type = malloc(strlen($3->lexeme) + 10);
          sprintf(array_type, "%s[]", $3->lexeme);
          add_symbol(var_name, SYM_VAR, array_type, 0, NULL);
          free(array_type);
        } else {
          add_symbol(var_name, SYM_VAR, $3->lexeme, 0, NULL);
        }
      }
      $$ = NULL;
    }
  ;

var_list
  : ID
    {
      $$ = ast_new(N_PARS, "", 1, ast_new(N_ID, $1, 0));
    }
  | ID LBRACK INT_LIT RBRACK
    {
      AST *array_node = ast_new(N_ID, $1, 1, ast_new(N_LIT, NULL, 0));
      $$ = ast_new(N_PARS, "", 1, array_node);
    }
  | ID COLON INT_LIT
    {
      /* Handle initial value assignment like i:0 */
      $$ = ast_new(N_PARS, "", 1, ast_new(N_ID, $1, 0));
    }
  | var_list COMMA ID
    {
      AST *lst = $1;
      lst->n++;
      lst->c = realloc(lst->c, lst->n * sizeof(AST*));
      lst->c[lst->n-1] = ast_new(N_ID, $3, 0);
      $$ = lst;
    }
  | var_list COMMA ID LBRACK INT_LIT RBRACK
    {
      AST *lst = $1;
      lst->n++;
      lst->c = realloc(lst->c, lst->n * sizeof(AST*));
      AST *array_node = ast_new(N_ID, $3, 1, ast_new(N_LIT, NULL, 0));
      lst->c[lst->n-1] = array_node;
      $$ = lst;
    }
  | var_list COMMA ID COLON INT_LIT
    {
      AST *lst = $1;
      lst->n++;
      lst->c = realloc(lst->c, lst->n * sizeof(AST*));
      lst->c[lst->n-1] = ast_new(N_ID, $3, 0);
      $$ = lst;
    }
  ;

/*----------------------------------------------------------------------*/
/* Function list                                                        */
/*----------------------------------------------------------------------*/
functions
  : /* empty */
  | functions func
  ;

/*----------------------------------------------------------------------*/
/* Function definition                                                  */
/*----------------------------------------------------------------------*/
func
  : func_prologue_with_ret T_BEGIN stmt_list T_END
    {
      exit_scope();
      AST *body = $3;
      AST *nm = ast_new(N_ID, cur_func_name, 0);
      AST *ps = cur_param_ast;
      AST *rv = ast_new(N_RET, cur_ret_type, 1,
                        ast_new(N_ID, cur_ret_type, 0));
      AST *f  = ast_new(N_FUNC, "FUNC", 4, nm, ps, rv, body);
      print_node(f, 1);
      free_node(f);

      free(cur_ret_type);     cur_ret_type     = NULL;
      free(cur_func_name);    cur_func_name    = NULL;
      cur_param_ast = NULL;
    }

  | func_prologue_no_ret T_BEGIN stmt_list T_END
    {
      exit_scope();
      AST *body = $3;
      AST *nm = ast_new(N_ID, cur_func_name, 0);
      AST *ps = cur_param_ast;
      AST *rv = ast_new(N_RET, "NONE", 1,
                        ast_new(N_ID, "NONE", 0));
      AST *f  = ast_new(N_FUNC, "FUNC", 4, nm, ps, rv, body);
      print_node(f, 1);
      free_node(f);

      free(cur_ret_type);     cur_ret_type     = NULL;
      free(cur_func_name);    cur_func_name    = NULL;
      cur_param_ast = NULL;
    }
  ;

/*----------------------------------------------------------------------*/
/* Function prologues                                                   */
/*----------------------------------------------------------------------*/
func_prologue_with_ret
  : DEF ID LPAREN params RPAREN COLON RETURNS type
    {
      if (strcmp($2, "main") == 0) {
        main_count++;
        if ($4->n != 0) {
          fprintf(stderr, "Error: main function cannot have parameters\n");
          exit(1);
        }
        if (strcmp($8->lexeme, "NONE") != 0) {
          fprintf(stderr, "Error: main function must return NONE\n");
          exit(1);
        }
      }

      if (strcmp($8->lexeme, "STRING") == 0) {
        fprintf(stderr, "Error: Function cannot return STRING type\n");
        exit(1);
      }

      cur_func_name  = strdup($2);
      cur_param_ast  = $4;

      int pc = $4->n;
      char **pt = NULL;
      if (pc > 0) {
        pt = malloc(pc * sizeof *pt);
        for (int i = 0; i < pc; i++) {
          AST *param = $4->c[i];
          AST *type_node = param->c[0];
          pt[i] = strdup(type_node->lexeme);
        }
      }
      
      add_func_declaration(cur_func_name, pc, pt, $8->lexeme);
      add_symbol(cur_func_name, SYM_FUNC, $8->lexeme, pc, pt);
      cur_ret_type = strdup($8->lexeme);

      enter_scope();
      
      for (int i = 0; i < $4->n; i++) {
        AST *param = $4->c[i];
        const char *param_type = param->c[0]->lexeme;
        const char *param_name = param->lexeme;
        const char *source_name = param->typ;
        
        add_symbol(param_name, SYM_PARAM, param_type, 0, NULL);
        if (source_name && strcmp(source_name, param_name) != 0) {
          add_symbol(source_name, SYM_PARAM, param_type, 0, NULL);
        }
        
        if (param->typ) {
          free(param->typ);
          param->typ = NULL;
        }
      }
    }
  ;

func_prologue_no_ret
  : DEF ID LPAREN params RPAREN COLON
    {
      if (strcmp($2, "main") == 0) {
        main_count++;
        if ($4->n != 0) {
          fprintf(stderr, "Error: main function cannot have parameters\n");
          exit(1);
        }
      }

      cur_func_name = strdup($2);
      cur_param_ast = $4;

      int pc = $4->n;
      char **pt = NULL;
      if (pc > 0) {
        pt = malloc(pc * sizeof *pt);
        for (int i = 0; i < pc; i++) {
          AST *param = $4->c[i];
          AST *type_node = param->c[0];
          pt[i] = strdup(type_node->lexeme);
        }
      }

      add_func_declaration(cur_func_name, pc, pt, "NONE");
      add_symbol(cur_func_name, SYM_FUNC, "NONE", pc, pt);
      cur_ret_type = strdup("NONE");
      enter_scope();
      
      for (int i = 0; i < pc; i++) {
        AST *param = $4->c[i];
        const char *param_type = param->c[0]->lexeme;
        const char *param_name = param->lexeme;
        const char *source_name = param->typ;
        
        add_symbol(param_name, SYM_PARAM, param_type, 0, NULL);
        if (source_name && strcmp(source_name, param_name) != 0) {
          add_symbol(source_name, SYM_PARAM, param_type, 0, NULL);
        }
        
        if (param->typ) {
          free(param->typ);
          param->typ = NULL;
        }
      }
    }

  /* Special rule for "_ main" function - handle the space */
  | DEF '_' ID LPAREN params RPAREN COLON
    {
      if (strcmp($3, "main") == 0) {
        main_count++;
        if ($5->n != 0) {
          fprintf(stderr, "Error: main function cannot have parameters\n");
          exit(1);
        }
        
        cur_func_name = strdup("main");
        cur_param_ast = $5;
        
        add_func_declaration("main", 0, NULL, "NONE");
        add_symbol("main", SYM_FUNC, "NONE", 0, NULL);
        cur_ret_type = strdup("NONE");
        enter_scope();
      } else {
        fprintf(stderr, "Error: Invalid function name '_ %s'\n", $3);
        exit(1);
      }
    }
  ;

/*----------------------------------------------------------------------*/
/* Parameter lists                                                      */
/*----------------------------------------------------------------------*/
params
  : /* none */            { $$ = ast_new(N_PARS,"",0); }
  | nonempty_params       { $$ = $1; }
  | nonempty_params SEMI  { $$ = $1; }
  ;

nonempty_params
  : param                  { 
      $$ = ast_new(N_PARS,"",1,$1); 
    }
  | nonempty_params SEMI param {
      AST *lst = $1;
      lst->n++;
      lst->c = realloc(lst->c,lst->n*sizeof(AST*));
      lst->c[lst->n-1] = $3;
      $$ = lst;
    }
  ;

param
  : ID type COLON ID {
      AST *type_node = $2;
      AST *param_node = ast_new(N_PARAM, $4, 1, type_node);
      param_node->typ = strdup($1);
      $$ = param_node;
    }
  ;

type
  : ID {
      char *upper_type = strdup($1);
      for(char *c = upper_type; *c; ++c) *c = toupper(*c);
      AST *type_ast = ast_new(N_ID, upper_type, 0);
      free(upper_type);
      $$ = type_ast;
    }
  | ID LPAREN type RPAREN {
      char *upper_base = strdup($1);
      for(char *c = upper_base; *c; ++c) *c = toupper(*c);
      
      if (strcmp(upper_base, "PTR") == 0) {
        char *ptr_type = malloc(strlen($3->lexeme) + 10);
        sprintf(ptr_type, "PTR(%s)", $3->lexeme);
        AST *type_ast = ast_new(N_ID, ptr_type, 0);
        free(ptr_type);
        free(upper_base);
        $$ = type_ast;
      } else {
        fprintf(stderr, "Error: Unknown constructed type %s\n", upper_base);
        free(upper_base);
        exit(1);
      }
    }
  ;

/*----------------------------------------------------------------------*/
/* Variable reference                                                   */
/*----------------------------------------------------------------------*/
variable
  : ID    { $$ = ast_new(N_ID, $1, 0); }
  ;

/*----------------------------------------------------------------------*/
/* Statement lists and statements                                       */
/*----------------------------------------------------------------------*/
stmt_list
  : /* empty */       { $$ = ast_new(N_BLOCK,"",0); }
  | stmt_list stmt    {
      AST *lst = $1;
      if ($2) {
        lst->n++;
        lst->c = realloc(lst->c,lst->n*sizeof(AST*));
        lst->c[lst->n-1] = $2;
      }
      $$ = lst;
    }
  ;

stmt
  : var_decl { $$ = $1; }
  
  | T_BEGIN 
    {
      enter_scope();
    }
    stmt_list T_END 
    {
      exit_scope();
      $$ = $3;
    }
  
  | variable ASSIGN expr SEMI {
      Sym *s = lookup($1->lexeme);
      if (!s) {
        fprintf(stderr, "Error: Variable `%s` not declared\n", $1->lexeme);
        exit(1);
      }

      if (!types_compatible(s->type, $3->typ)) {
        fprintf(stderr,
                "Error: Type mismatch in assignment - cannot assign %s to %s\n",
                $3->typ, s->type);
        exit(1);
      }

      if (strcmp(s->type, "REAL") == 0 && strcmp($3->typ, "INT") == 0) {
        free($3->typ);
        $3->typ = strdup("REAL");
      }

      $$ = ast_new(N_ASSIGN, "=", 2, $1, $3);
    }

  | '*' expr ASSIGN expr SEMI {
      if (strncmp($2->typ, "PTR(", 4)) {
        fprintf(stderr, "Error: Dereference operator can only be applied to pointer types\n");
        exit(1);
      }
      char *inner = strdup($2->typ + 4);
      inner[strlen(inner) - 1] = 0;
      
      if (!types_compatible(inner, $4->typ)) {
        fprintf(stderr, "Error: Type mismatch in pointer assignment - cannot assign %s to %s\n",
                $4->typ, inner);
        exit(1);
      }
      
      if (strcmp(inner, "REAL") == 0 && strcmp($4->typ, "INT") == 0) {
        free($4->typ);
        $4->typ = strdup("REAL");
      }
      
      AST *deref_node = ast_new(N_UNOP, "*", 1, $2);
      deref_node->typ = strdup(inner);
      $$ = ast_new(N_ASSIGN, "=", 2, deref_node, $4);
      free(inner);
    }

  | expr LBRACK expr RBRACK ASSIGN expr SEMI {
      if (strcmp($1->typ, "STRING") && !strstr($1->typ, "STRING[]")) {
        fprintf(stderr, "Error: Indexing can only be applied to STRING or STRING[] type\n");
        exit(1);
      }
      if (strcmp($3->typ, "INT")) {
        fprintf(stderr, "Error: Array index must be INT type\n");
        exit(1);
      }
      if (strcmp($6->typ, "CHAR")) {
        fprintf(stderr, "Error: Can only assign CHAR to string element\n");
        exit(1);
      }
      
      AST *index_node = ast_new(N_INDEX, "[]", 2, $1, $3);
      index_node->typ = strdup("CHAR");
      $$ = ast_new(N_ASSIGN, "=", 2, index_node, $6);
    }

  | if_stmt { $$ = $1; }
  
  /* Updated while syntax: while : condition begin */
  | WHILE COLON expr T_BEGIN stmt_list T_END {
      if(strcmp($3->typ,"BOOL")){ 
        fprintf(stderr,"Error: Condition in while loop must be BOOL type\n"); 
        exit(1); 
      }
      $$ = ast_new(N_WHILE,"WHILE",2,$3,$5);
    }
    
  | DO T_BEGIN stmt_list T_END WHILE expr SEMI {
      if(strcmp($6->typ,"BOOL")){ 
        fprintf(stderr,"Error: Condition in do-while loop must be BOOL type\n"); 
        exit(1); 
      }
      $$ = ast_new(N_DOWHILE,"DOWHILE",2,$3,$6);
    }
    
  | FOR ID ASSIGN expr TO expr COLON T_BEGIN stmt_list T_END {
      if(strcmp($4->typ,"INT")||strcmp($6->typ,"INT")){
        fprintf(stderr,"Error: For loop bounds must be INT type\n"); 
        exit(1);
      }
      AST *loop_var = ast_new(N_ID, $2, 0);
      loop_var->typ = strdup("INT");
      $$ = ast_new(N_FOR,"FOR",4,loop_var,$4,$6,$9);
    }
    
  | expr SEMI {
      $$ = $1;
    }
    
  | RETURN expr SEMI {
      if(cur_ret_type && !types_compatible(cur_ret_type,$2->typ)){
        fprintf(stderr,"Error: Return type mismatch - expected %s, got %s\n",
                cur_ret_type,$2->typ);
        exit(1);
      }
      if(!strcmp($2->typ,"STRING")){
        fprintf(stderr,"Error: Cannot return STRING type\n"); 
        exit(1);
      }
      if (cur_ret_type && strcmp(cur_ret_type, "REAL") == 0 && strcmp($2->typ, "INT") == 0) {
        free($2->typ);
        $2->typ = strdup("REAL");
      }
      $$ = ast_new(N_RET,"RET",1,$2);
    }
    
  /* Handle invalid left-hand side assignments like 3+6 = 9 */
  | expr ASSIGN expr SEMI {
      fprintf(stderr, "Error: Invalid left-hand side in assignment\n");
      exit(1);
    }
  ;

/*----------------------------------------------------------------------*/
/* IF statement handling                                                 */
/*----------------------------------------------------------------------*/
if_stmt
  : IF expr COLON T_BEGIN stmt_list T_END %prec ELSE {
      if (strcmp($2->typ, "BOOL")) {
        fprintf(stderr, "Error: Condition in if statement must be BOOL type\n");
        exit(1);
      }
      $$ = ast_new(N_IFELSE, "", 2, $2, $5);
    }
  | IF expr COLON T_BEGIN stmt_list T_END ELSE COLON T_BEGIN stmt_list T_END {
      if(strcmp($2->typ,"BOOL")){ 
        fprintf(stderr,"Error: Condition in if statement must be BOOL type\n"); 
        exit(1); 
      }
      $$ = ast_new(N_IFELSE,"",3,$2,$5,$10);
    }
  | IF expr COLON T_BEGIN stmt_list T_END elif_chain {
      if (strcmp($2->typ, "BOOL")) {
        fprintf(stderr, "Error: Condition in if statement must be BOOL type\n");
        exit(1);
      }
      $$ = ast_new(N_IFELSE, "", 3, $2, $5, $7);
    }
  ;

elif_chain
  : ELIF expr COLON T_BEGIN stmt_list T_END %prec ELSE {
      if (strcmp($2->typ, "BOOL")) {
        fprintf(stderr, "Error: Condition in elif statement must be BOOL type\n");
        exit(1);
      }
      $$ = ast_new(N_IFELSE, "", 2, $2, $5);
    }
  | ELIF expr COLON T_BEGIN stmt_list T_END ELSE COLON T_BEGIN stmt_list T_END {
      if (strcmp($2->typ, "BOOL")) {
        fprintf(stderr, "Error: Condition in elif statement must be BOOL type\n");
        exit(1);
      }
      $$ = ast_new(N_IFELSE, "", 3, $2, $5, $10);
    }
  | ELIF expr COLON T_BEGIN stmt_list T_END elif_chain {
      if (strcmp($2->typ, "BOOL")) {
        fprintf(stderr, "Error: Condition in elif statement must be BOOL type\n");
        exit(1);
      }
      AST *elif_branch = ast_new(N_IFELSE, "", 3, $2, $5, $7);
      $$ = elif_branch;
    }
  ;

/*----------------------------------------------------------------------*/
/* Argument lists                                                       */
/*----------------------------------------------------------------------*/
args
  : /* none */      { $$ = ast_new(N_PARS,"",0); }
  | nonempty_args   { $$ = $1; }
  ;

nonempty_args
  : expr                     { $$ = ast_new(N_PARS,"",1,$1); }
  | nonempty_args COMMA expr {
      AST *lst = $1;
      lst->n++;
      lst->c = realloc(lst->c,lst->n*sizeof(AST*));
      lst->c[lst->n-1] = $3;
      $$ = lst;
    }
  ;

/*----------------------------------------------------------------------*/
/* Expression rules                                                     */
/*----------------------------------------------------------------------*/
expr
  : primary_expr { $$ = $1; }
  | expr OR_OR expr    {
      if(strcmp($1->typ,"BOOL")||strcmp($3->typ,"BOOL")){
        fprintf(stderr,"Error: Logical OR operator requires BOOL operands\n"); 
        exit(1);
      }
      AST *z = ast_new(N_BINOP,"||",2,$1,$3);
      z->typ = strdup("BOOL"); $$ = z;
    }
    
  | expr AND_AND expr  {
      if(strcmp($1->typ,"BOOL")||strcmp($3->typ,"BOOL")){
        fprintf(stderr,"Error: Logical AND operator requires BOOL operands\n"); 
        exit(1);
      }
      AST *z = ast_new(N_BINOP,"&&",2,$1,$3);
      z->typ = strdup("BOOL"); $$ = z;
    }
    
  | NOT expr %prec NOT {
      if(strcmp($2->typ,"BOOL")){ 
        fprintf(stderr,"Error: Logical NOT operator requires BOOL operand\n"); 
        exit(1); 
      }
      AST *z = ast_new(N_UNOP,"!",1,$2);
      z->typ = strdup("BOOL"); $$ = z;
    }
    
  | expr '+' expr       {
      int i1=!strcmp($1->typ,"INT"), r1=!strcmp($1->typ,"REAL");
      int i2=!strcmp($3->typ,"INT"), r2=!strcmp($3->typ,"REAL");
      if(i1&&i2){ 
        AST*z=ast_new(N_BINOP,"+",2,$1,$3); 
        z->typ=strdup("INT"); $$ = z; 
      }
      else if((i1&&r2)||(r1&&i2)||(r1&&r2)){
        AST*z=ast_new(N_BINOP,"+",2,$1,$3); 
        z->typ=strdup("REAL"); $$ = z;
      } else {
        fprintf(stderr,"Error: Addition operator requires INT or REAL operands\n"); 
        exit(1);
      }
    }
    
  | expr '-' expr       {
      int i1=!strcmp($1->typ,"INT"), r1=!strcmp($1->typ,"REAL");
      int i2=!strcmp($3->typ,"INT"), r2=!strcmp($3->typ,"REAL");
      if(i1&&i2){ 
        AST*z=ast_new(N_BINOP,"-",2,$1,$3); 
        z->typ=strdup("INT"); $$ = z; 
      }
      else if((i1&&r2)||(r1&&i2)||(r1&&r2)){
        AST*z=ast_new(N_BINOP,"-",2,$1,$3); 
        z->typ=strdup("REAL"); $$ = z;
      } else {
        fprintf(stderr,"Error: Subtraction operator requires INT or REAL operands\n"); 
        exit(1);
      }
    }
    
  | expr '*' expr       {
      int i1=!strcmp($1->typ,"INT"), r1=!strcmp($1->typ,"REAL");
      int i2=!strcmp($3->typ,"INT"), r2=!strcmp($3->typ,"REAL");
      if(i1&&i2){ 
        AST*z=ast_new(N_BINOP,"*",2,$1,$3); 
        z->typ=strdup("INT"); $$ = z; 
      }
      else if((i1&&r2)||(r1&&i2)||(r1&&r2)){
        AST*z=ast_new(N_BINOP,"*",2,$1,$3); 
        z->typ=strdup("REAL"); $$ = z;
      } else {
        fprintf(stderr,"Error: Multiplication operator requires INT or REAL operands\n"); 
        exit(1);
      }
    }
    
  | expr '/' expr       {
      int i1=!strcmp($1->typ,"INT"), r1=!strcmp($1->typ,"REAL");
      int i2=!strcmp($3->typ,"INT"), r2=!strcmp($3->typ,"REAL");
      if(i1&&i2){ 
        AST*z=ast_new(N_BINOP,"/",2,$1,$3); 
        z->typ=strdup("INT"); $$ = z; 
      }
      else if((i1&&r2)||(r1&&i2)||(r1&&r2)){
        AST*z=ast_new(N_BINOP,"/",2,$1,$3); 
        z->typ=strdup("REAL"); $$ = z;
      } else {
        fprintf(stderr,"Error: Division operator requires INT or REAL operands\n"); 
        exit(1);
      }
    }
    
  | expr GT expr        {
      int ok1=!strcmp($1->typ,"INT")||!strcmp($1->typ,"REAL");
      int ok3=!strcmp($3->typ,"INT")||!strcmp($3->typ,"REAL");
      if(!ok1||!ok3){ 
        fprintf(stderr,"Error: Comparison operator requires INT or REAL operands\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,">",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | expr LT expr        {
      int ok1=!strcmp($1->typ,"INT")||!strcmp($1->typ,"REAL");
      int ok3=!strcmp($3->typ,"INT")||!strcmp($3->typ,"REAL");
      if(!ok1||!ok3){ 
        fprintf(stderr,"Error: Comparison operator requires INT or REAL operands\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,"<",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | expr GE expr        {
      int ok1=!strcmp($1->typ,"INT")||!strcmp($1->typ,"REAL");
      int ok3=!strcmp($3->typ,"INT")||!strcmp($3->typ,"REAL");
      if(!ok1||!ok3){ 
        fprintf(stderr,"Error: Comparison operator requires INT or REAL operands\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,">=",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | expr LE expr        {
      int ok1=!strcmp($1->typ,"INT")||!strcmp($1->typ,"REAL");
      int ok3=!strcmp($3->typ,"INT")||!strcmp($3->typ,"REAL");
      if(!ok1||!ok3){ 
        fprintf(stderr,"Error: Comparison operator requires INT or REAL operands\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,"<=",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | expr EQ expr        {
      int same = !strcmp($1->typ,$3->typ);
      int both_ptrs = (strncmp($1->typ,"PTR(",4)==0 && strncmp($3->typ,"PTR(",4)==0);
      int int_real = (!strcmp($1->typ,"INT") && !strcmp($3->typ,"REAL")) || 
                     (!strcmp($1->typ,"REAL") && !strcmp($3->typ,"INT"));
      
      if(!(same || both_ptrs || int_real)){ 
        fprintf(stderr,"Error: Equality operator requires compatible types\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,"==",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | expr NEQ expr       {
      int same = !strcmp($1->typ,$3->typ);
      int both_ptrs = (strncmp($1->typ,"PTR(",4)==0 && strncmp($3->typ,"PTR(",4)==0);
      int int_real = (!strcmp($1->typ,"INT") && !strcmp($3->typ,"REAL")) || 
                     (!strcmp($1->typ,"REAL") && !strcmp($3->typ,"INT"));
      
      if(!(same || both_ptrs || int_real)){ 
        fprintf(stderr,"Error: Inequality operator requires compatible types\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_BINOP,"!=",2,$1,$3); 
      z->typ=strdup("BOOL"); $$ = z;
    }
    
  | BAR expr BAR        {
      if(strcmp($2->typ,"STRING") && !strstr($2->typ, "STRING[]")){ 
        fprintf(stderr,"Error: Absolute value operator can only be applied to STRING\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_UNOP,"| |",1,$2); 
      z->typ=strdup("INT"); $$ = z;
    }
    
  | expr LBRACK expr RBRACK {
      if(strcmp($1->typ,"STRING") && !strstr($1->typ, "STRING[]")){ 
        fprintf(stderr,"Error: Indexing can only be applied to STRING or STRING[] type\n"); 
        exit(1); 
      }
      if(strcmp($3->typ,"INT")){ 
        fprintf(stderr,"Error: Array index must be INT type\n"); 
        exit(1); 
      }
      AST*z=ast_new(N_INDEX,"[]",2,$1,$3); 
      z->typ=strdup("CHAR"); $$ = z;
    }
    
  | '*' expr {
      if(strncmp($2->typ,"PTR(",4)){ 
        fprintf(stderr,"Error: Dereference operator can only be applied to pointer types\n"); 
        exit(1); 
      }
      char *inner = strdup($2->typ+4);
      inner[strlen(inner)-1] = 0;
      AST*z=ast_new(N_UNOP,"*",1,$2);
      z->typ = inner; $$ = z;
    }
  ;

primary_expr
  : BOOL_LIT   {
      AST *L = ast_new(N_LIT,NULL,0);
      L->lexeme = strdup($1 ? "TRUE" : "FALSE");
      L->typ    = strdup("BOOL"); $$ = L;
    }
  | REAL_LIT   {
      AST *L = ast_new(N_LIT,NULL,0);
      char buf[64]; snprintf(buf,sizeof buf,"%g",$1);
      L->lexeme = strdup(buf);
      L->typ    = strdup("REAL"); $$ = L;
    }
  | INT_LIT    {
      AST *L = ast_new(N_LIT,NULL,0);
      L->lexeme = malloc(32);
      snprintf(L->lexeme,32,"%d",$1);
      L->typ    = strdup("INT"); $$ = L;
    }

  | CHAR_LIT {
      AST *L = ast_new(N_LIT, NULL, 0);
      L->lexeme = strdup($1);
      L->typ    = strdup("CHAR");
      $$ = L;
    }

  | STRING_LIT {
      AST *L = ast_new(N_LIT, NULL, 0);
      L->lexeme = strdup($1);
      L->typ    = strdup("STRING");
      $$ = L;
    }

  | variable {
      Sym *s = lookup($1->lexeme);
      if(!s){ 
        fprintf(stderr,"Error: Variable `%s` not declared\n",$1->lexeme); 
        exit(1); 
      }
      $1->typ = strdup(s->type); 
      $$ = $1;
    }
    
  | LPAREN expr RPAREN { $$ = $2; }
  
  | '&' variable {
      Sym *s = lookup($2->lexeme);
      if (!s) {
        fprintf(stderr,"Error: Variable `%s` not declared\n",$2->lexeme);
        exit(1);
      }
      if (s->kind != SYM_VAR && s->kind != SYM_PARAM) {
        fprintf(stderr,"Error: Address-of operator can only be applied to variables\n");
        exit(1);
      }
      if (strcmp(s->type, "INT") && strcmp(s->type, "REAL") && 
          strcmp(s->type, "CHAR") && strcmp(s->type, "STRING")) {
        fprintf(stderr,"Error: Address-of operator can only be applied to INT, REAL, CHAR, or STRING variables\n");
        exit(1);
      }
      AST *z = ast_new(N_UNOP, "&", 1, $2);
      z->typ = malloc(strlen(s->type) + 8);
      sprintf(z->typ, "PTR(%s)", s->type);
      $$ = z;
    }
    
  | '&' variable LBRACK expr RBRACK {
      Sym *s = lookup($2->lexeme);
      if (!s) {
        fprintf(stderr,"Error: Variable `%s` not declared\n",$2->lexeme);
        exit(1);
      }
      if (strcmp(s->type, "STRING") && !strstr(s->type, "STRING[]")) {
        fprintf(stderr,"Error: Address-of operator on index requires STRING or STRING[]\n");
        exit(1);
      }
      if (strcmp($4->typ, "INT")) {
        fprintf(stderr,"Error: Array index must be INT type\n");
        exit(1);
      }
      $2->typ = strdup("STRING");
      AST *index_node = ast_new(N_INDEX, "[]", 2, $2, $4);
      index_node->typ = strdup("CHAR");
      AST *z = ast_new(N_UNOP, "&", 1, index_node);
      z->typ = strdup("PTR(CHAR)");
      $$ = z;
    }
    
  | NULL_T               {
      AST*z=ast_new(N_LIT,"null",0);
      z->typ = strdup("PTR(ANY)"); $$ = z;
    }
    
  | CALL ID LPAREN args RPAREN {
      Sym *s = lookup($2);
      FuncDecl *fd = find_func_declaration($2);
      
      if ((!s || s->kind != SYM_FUNC) && !fd) { 
        if (cur_func_name && strcmp(cur_func_name, "main") != 0 && parsing_functions) {
          char **param_types = NULL;
          if ($4->n > 0) {
            param_types = malloc($4->n * sizeof(char*));
            for (int i = 0; i < $4->n; i++) {
              param_types[i] = strdup($4->c[i]->typ);
            }
          }
          add_unresolved_call($2, $4->n, param_types);
          
          AST *call = ast_new(N_CALL,$2,$4->n);
          for(int i = 0; i < $4->n; i++) 
            call->c[i] = $4->c[i];
          call->typ=strdup("BOOL");
          $$ = call;
        } else {
          fprintf(stderr, "Error: Function `%s` not defined\n", $2);
          exit(1);
        }
      } else {
        int expected_params = (s && s->kind == SYM_FUNC) ? s->param_count : fd->param_count;
        char **expected_types = (s && s->kind == SYM_FUNC) ? s->param_types : fd->param_types;
        char *return_type = (s && s->kind == SYM_FUNC) ? s->type : fd->return_type;
        
        if(expected_params != $4->n){ 
          fprintf(stderr,"Error: Function `%s` expects %d parameters, got %d\n",
                  $2, expected_params, $4->n); 
          exit(1); 
        }
        
        for(int i=0;i<$4->n;i++){
          if (strcmp($2, "print") == 0 && i == 0 && 
              (strcmp($4->c[i]->typ, "INT") == 0 || strcmp($4->c[i]->typ, "REAL") == 0)) {
            continue;
          }
          
          if(!types_compatible(expected_types[i], $4->c[i]->typ)){
            fprintf(stderr,"Error: Parameter %d of function `%s` should be %s, got %s\n",
                    i+1,$2,expected_types[i],$4->c[i]->typ);
            exit(1);
          }
          
          if (strcmp(expected_types[i], "REAL") == 0 && strcmp($4->c[i]->typ, "INT") == 0) {
            free($4->c[i]->typ);
            $4->c[i]->typ = strdup("REAL");
          }
        }
        AST *call = ast_new(N_CALL,$2,$4->n);
        for(int i = 0; i < $4->n; i++) 
          call->c[i] = $4->c[i];
        call->typ=strdup(return_type); 
        $$ = call;
      }
    }
  ;

%%

static const char *NodeTypeNames[] = {
    "FUNC", "PARS", "PARAM", "RET",
    "BLOCK", "IFELSE", "ASSIGN", "BINOP",
    "UNOP", "CALL", "WHILE", "DOWHILE", "FOR", "INDEX",
    "ID", "LIT"
};

static void indent(int d) {
    for (int i = 0; i < d; i++)
        putchar(' ');
}

void print_node(AST *z, int d) {
    if (!z) return;
    indent(d);

    if (z->lexeme && strlen(z->lexeme) > 0) {
        printf("%s", z->lexeme);
    } else {
        printf("%s", NodeTypeNames[z->type]);
    }

    if (z->typ && strlen(z->typ) > 0) {
        printf(" : %s", z->typ);
    }
    putchar('\n');

    for (int i = 0; i < z->n; i++)
        print_node(z->c[i], d + 1);
}

void free_node(AST *z) {
    if (!z) return;
    for (int i = 0; i < z->n; i++)
        free_node(z->c[i]);

    free(z->lexeme);
    free(z->typ);
    free(z->c);
    free(z);
}

int main(void) {
    return yyparse();
}