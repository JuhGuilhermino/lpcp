
\<DefProgram> $arrow.r$ <DefSub> <DefMain>
\
\<DefMain> $arrow.r$ 'main' \<Id> '{' '}' '(' \<Stmts> ')'

\<DefSub> $arrow.r$ \<DefProc> \<DefSub> | \<DefFunc> \<DefSub> | $lambda$
\
\<DefFunc> $arrow.r$ 'function' \<Type> \<Id> '{' \<Params> '}' '(' \<Stmts> 'return' \<Expr> ';'')' 
\
\<DefProc> $arrow.r$ 'procedure' \<Id> '{' \<Params> '}' '(' \<Stmts> ')'
\
\<Params>  $arrow.r$ \<Param> | \<Param> ',' \<Params> | $lambda$
\
\<Param> $arrow.r$ \<Type> \<Id>
\
\<Type> $arrow.r$ 'int' | 'float' | 'list' '<' \<Type> '>'
\
\<Stmts> $arrow.r$ \<St> ';' | \<St> ';' \<Stmts>
\
\<St> $arrow.r$ \<Cond>

|
\<Attr>

|
\<Decl>

|
\<Call> //?

|
\<Return>
\
\<Cond> $arrow.r$ 'if' '{'\<Expr>'}' '('\<Stmts>')' 'endif'
\
\<Attr> $arrow.r$ \<Var> '<\-' \<Expr>
\
\<Decl> $arrow.r$ \<Type> \<Id> '<\-' \<Expr>
\
\<Call> $arrow.r$ \<Id> '(' \<ListExpr> ')' | \<Var> '.' \<Id> '(' \<ListExpr> ')'
\
\<Return> $arrow.r$ 'return' | 'return' \<Expr>
\
\<Var> $arrow.r$ \<Id> | \<Id>'['\<Expr>']'
\
\<Expr> $arrow.r$ \<ExprOr>
\
\<ExprOr> $arrow.r$ \<ExprAnd> \<ExprOr'>
\
\<ExprOr'> $arrow.r$ '||' \<ExprAnd> \<ExprOr'> | $lambda$
\
\<ExprAnd> $arrow.r$ \<ExprNot> \<ExprAnd'>
\
\<ExprAnd'> $arrow.r$ '&&' \<ExprNot> \<ExprAnd'> | $lambda$
\
\<ExprNot> $arrow.r$ '!' \<ExprNot> | \<ExprRel>
\
\<ExprRel> $arrow.r$ \<ExprAdd> \<ExprRel'>
\
\<ExprRel'> $arrow.r$ \<Op> \<ExprAdd> | $lambda$
\
\<ExprAdd> $arrow.r$ \<ExprMul> \<ExprAdd'>
\
\<ExprAdd'> $arrow.r$ '+' \<ExprMul> \<ExprAdd'> | '-' \<ExprMul> \<ExprAdd'> | $lambda$
\
\<ExprMul> $arrow.r$ \<ExprPrim> \<ExprMul'>
\
\<ExprMul'> $arrow.r$ '\*' \<ExprPrim> \<ExprMul'> | '/' \<ExprPrim> \<ExprMul'> | $lambda$
\
\<ExprPrim> $arrow.r$ '('\<Expr>')' | \<Compr> | \<Id> \<ExprPrim'>| \<Number> | 'true' | 'false'
\
\<ExprPrim'> $arrow.r$ '['\<Expr>']' | '('\<ListExpr>')' | '.' \<Id> '('\<ListExpr>')' | $lambda$
\
\<Compr> $arrow.r$ '['\<Id> '->' \<Id> '|' \<Expr> ']'
\
\<ListExpr> $arrow.r$ \<Expr> \<ListExpr'> | $lambda$
\
\<ListExpr'> $arrow.r$ ',' \<ListExpr> | $lambda$
\
\<Op> $arrow.r$ <= | >= | < | > | == | !=
\
\<Id> $arrow.r$ \<Char> \<Word> | \<Char>
\
\<Word> $arrow.r$ \<Char> \<Word> | \<Number> \<Word> | \<Char> | \<Number>
\
\<Char> $arrow.r$ 'a' | ... | 'z' | 'A' | ... | 'Z'
\
\<Number> $arrow.r$ '0' | ... | '9' 
\
\<Type> $arrow.r$
/*
DECISOES
NOSSO MAIN VAI TER PARAMETROS?
    Alex: Nao!
    carol: acho que umberto vai nos forcar a fazer, se sim acho que precisamos fazer um paramsmain separado
    mari: nao

VAMOS SEPARAR PROCEDURE DE FUNCTION EM SUBPROGRAM?
    mari: acho jogo mas por mim tanto faz

    pq ficaria assim ó

    DEFSUB --> defProc DEFSUB | defFunc DEFSUB
    defFunc -->  ai eh o Func + tipo +  id + lista de params
    defProc --> so nao tem retorno
    
    bo



*/