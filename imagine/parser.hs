module Main (main) where

import Scanner
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment

-- (mari) pelo q entendi a assinatura de todas as regras de produçao eh assim
-- REGRA :: ParsecT [Token] [(Token,Token)] IO ([Token])
-- i.e. ParsecT <ENTRADA> <ESTADO_USUARIO> <IO> <TIPO_RETORNO> 
-- entao conseguimos fazer um apelido p diminuir retrabalho ! 
-- (caso a gnt mude alguma coisa na tabela de simbolos)

type P res = ParsecT [Token] [(Token,Token)] IO res


-- parsers p/ tokens
-- palavra reservada para int
kw_intToken :: P Token
kw_intToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_INT) = Just token
  get_token _ = Nothing

-- int propriamente dito
intToken :: P Token
intToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Int _)) = Just token
  get_token _  = Nothing

kw_boolToken :: P Token
kw_boolToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_BOOL) = Just token
  get_token _ = Nothing

-- palavra reservada para float
kw_floatToken :: P Token
kw_floatToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_FLOAT) = Just token
  get_token _ = Nothing

-- float propriamente dito
floatToken :: P Token
floatToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Float _)) = Just token
  get_token _  = Nothing

-- palavra reservada para string
kw_stringToken :: P Token
kw_stringToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_STRING) = Just token
  get_token _ = Nothing

-- a string propriamente dita
stringToken :: P Token
stringToken = tokenPrim show update_pos get_token where 
  get_token token@(Token _ (String _)) = Just token
  get_token _ = Nothing

kw_u8Token :: P Token
kw_u8Token = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_U8) = Just token
  get_token _ = Nothing

kw_complexToken :: P Token
kw_complexToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_COMPLEX) = Just token
  get_token _ = Nothing

kw_kernelToken :: P Token
kw_kernelToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_KERNEL) = Just token
  get_token _ = Nothing

kw_imageToken :: P Token
kw_imageToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_IMAGE) = Just token
  get_token _ = Nothing

kw_signalToken :: P Token
kw_signalToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_SIGNAL) = Just token
  get_token _ = Nothing

kw_typeToken :: P Token
kw_typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_TYPE) = Just token
  get_token _ = Nothing
  
kw_enumToken :: P Token
kw_enumToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_ENUM) = Just token
  get_token _ = Nothing

kw_structToken :: P Token
kw_structToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_STRUCT) = Just token
  get_token _ = Nothing

kw_listToken :: P Token
kw_listToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_LIST) = Just token
  get_token _ = Nothing

kw_trueToken :: P Token
kw_trueToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_TRUE) = Just token
  get_token _ = Nothing

kw_falseToken :: P Token
kw_falseToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_FALSE) = Just token
  get_token _ = Nothing


-- subprogramas
kw_procedureToken :: P Token
kw_procedureToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_PROCEDURE) = Just token
  get_token _ = Nothing

kw_functionToken :: P Token
kw_functionToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_FUNCTION) = Just token
  get_token _ = Nothing

kw_mainToken :: P Token
kw_mainToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_MAIN) = Just token
  get_token _ = Nothing

kw_returnToken :: P Token
kw_returnToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_RETURN) = Just token
  get_token _ = Nothing


-- controle & comandos
kw_ifToken :: P Token
kw_ifToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_IF) = Just token
  get_token _ = Nothing

kw_endifToken :: P Token
kw_endifToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_ENDIF) = Just token
  get_token _ = Nothing

kw_elseToken :: P Token
kw_elseToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_ELSE) = Just token
  get_token _ = Nothing

kw_untilToken :: P Token
kw_untilToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_UNTIL) = Just token
  get_token _ = Nothing

kw_performToken :: P Token
kw_performToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_PERFORM) = Just token
  get_token _ = Nothing

kw_forToken :: P Token
kw_forToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_FOR) = Just token
  get_token _ = Nothing

kw_shoutToken :: P Token
kw_shoutToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_SHOUT) = Just token
  get_token _ = Nothing

kw_getToken :: P Token
kw_getToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_GET) = Just token
  get_token _ = Nothing

idToken :: P Token
idToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Id _)) = Just token
  get_token _  = Nothing


-- operadores 
op_assignToken :: P Token
op_assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_ASSIGN) = Just token
  get_token _ = Nothing

op_iteratorToken :: P Token
op_iteratorToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_ITERATOR) = Just token
  get_token _ = Nothing

op_comprehensionToken :: P Token
op_comprehensionToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_COMPREHENSION) = Just token
  get_token _ = Nothing

op_plusToken :: P Token
op_plusToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_PLUS) = Just token
  get_token _ = Nothing

op_minusToken :: P Token
op_minusToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_MINUS) = Just token
  get_token _ = Nothing

op_asteriskToken :: P Token
op_asteriskToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_ASTERISK) = Just token
  get_token _ = Nothing

op_slashToken :: P Token
op_slashToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_SLASH) = Just token
  get_token _ = Nothing

op_eqToken :: P Token
op_eqToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_EQ) = Just token
  get_token _ = Nothing

op_neqToken :: P Token
op_neqToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_NEQ) = Just token
  get_token _ = Nothing

op_greaterToken :: P Token
op_greaterToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_GREATER) = Just token
  get_token _ = Nothing

op_lesserToken :: P Token
op_lesserToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_LESSER) = Just token
  get_token _ = Nothing

op_geqToken :: P Token
op_geqToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_GEQ) = Just token
  get_token _ = Nothing

op_leqToken :: P Token
op_leqToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_LEQ) = Just token
  get_token _ = Nothing

op_andToken :: P Token
op_andToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_AND) = Just token
  get_token _ = Nothing

op_orToken :: P Token
op_orToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_OR) = Just token
  get_token _ = Nothing

op_notToken :: P Token
op_notToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ OP_NOT) = Just token
  get_token _ = Nothing


-- pontuacao
punc_lparenToken :: P Token
punc_lparenToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_LPAREN) = Just token
  get_token _ = Nothing

punc_rparenToken :: P Token
punc_rparenToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_RPAREN) = Just token
  get_token _ = Nothing

punc_lbraceToken :: P Token
punc_lbraceToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_LBRACE) = Just token
  get_token _ = Nothing

punc_rbraceToken :: P Token
punc_rbraceToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_RBRACE) = Just token
  get_token _ = Nothing

punc_lbracketToken :: P Token
punc_lbracketToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_LBRACKET) = Just token
  get_token _ = Nothing

punc_rbracketToken :: P Token
punc_rbracketToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_RBRACKET) = Just token
  get_token _ = Nothing

punc_semicolonToken :: P Token
punc_semicolonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_SEMICOLON) = Just token
  get_token _ = Nothing

punc_colonToken :: P Token
punc_colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_COLON) = Just token
  get_token _ = Nothing

punc_commaToken :: P Token
punc_commaToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_COMMA) = Just token
  get_token _ = Nothing

punc_dotToken :: P Token
punc_dotToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ PUNC_DOT) = Just token
  get_token _ = Nothing
  
  

-- posicoes dos tokens (linha, coluna)
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos


--- parsers p nao terminais

-- DefProgram → DefSub DefMain
defProgram :: P [Token]
defProgram = do
  a <- defSub
  b <- defMain
  eof

  return (a++b)

-- DefMain → 'main' '{' '}' '(' Stmts ')'
defMain :: P [Token]
defMain = do
  a <- kw_mainToken
  b <- punc_lbraceToken
  c <- punc_rbraceToken
  d <- punc_lparenToken
  e <- stmts
  f <- punc_rparenToken
  
  return ([a,b,c,d]++e++[f])

-- DefSub → 'procedure' DefProc DefSub | 'function' DefFunc DefSub | ε
defSub :: P [Token]
defSub = 
  do
    a <- kw_procedureToken
    b <- defProc
    c <- defSub
    return ([a]++b++c)
  <|>
  do
    a <- kw_functionToken
    b <- defFunc
    c <- defSub
    return ([a]++b++c)
  <|>
    return []

-- DefFunc → Type Id '{' Params '}' '(' Stmts 'return' Expr ';' ')'
defFunc :: P [Token]
defFunc = do
  a <- defType
  b <- idToken
  c <- punc_lbraceToken
  d <- params
  e <- punc_rbraceToken
  f <- punc_lparenToken
  g <- stmts
  h <- kw_returnToken
  i <- expr
  j <- punc_semicolonToken
  k <- punc_rparenToken

  return(a++[b,c]++d++[e,f]++g++[h]++i++[j,k])

-- DefProc → Id '{' Params '}' '(' Stmts ')'
defProc :: P [Token]
defProc = do
  a <- idToken
  b <- punc_lbraceToken
  c <- params
  d <- punc_rbraceToken
  e <- punc_lparenToken
  f <- stmts
  g <- punc_rparenToken

  return([a,b]++c++[d,e]++f++[g])

-- Params → Type Id Params' | ε
params :: P [Token]
params = fail "Params nao implementado!"

-- Params' → ',' Type Id Params' | ε
params' :: P [Token]
params' = fail "Params' nao implementado!"

-- Type → PrimType | StructType
defType :: P [Token]
defType =
  do
    a <- primType
    return ([a])
  <|>
  do
    b <- structType
    return (b)

-- PrimType → 'int' | 'bool' | 'float' | 'string' | 'u8' | 'complex' | 'kernel' | 'image' | 'signal'
primType :: P Token
primType =
  do
    a <- kw_intToken
    return (a)
  <|>
  do
    a <- kw_boolToken
    return (a)
  <|>
  do
    a <- kw_floatToken
    return (a)
  <|>
  do
    a <- kw_stringToken
    return (a)
  <|>
  do
    a <- kw_u8Token
    return (a)
  <|>
  do
    a <- kw_complexToken
    return (a)
  <|>
  do
    a <- kw_kernelToken
    return (a)
  <|>
  do
    a <- kw_imageToken
    return (a)
  <|>
  do
    a <- kw_signalToken
    return (a)


-- StructType → 'list' '<' Type '>'
structType :: P [Token]
structType = 
  do
    a <- kw_listToken
    b <- op_lesserToken
    c <- defType
    d <- op_greaterToken
    return ([a]++[b]++c++[d])

-- Stmts → St ';' Stmts | ε
stmts :: P [Token]
stmts = 
  do
    a <- st 
    b <- punc_semicolonToken
    c <- stmts 
    return (a++[b]++c)
  <|>
    return []

-- St → CondStmt | WhileStmt | ForStmt | DeclStmt | Id StTail | PrintStmt
st :: P [Token]
st =
  do
    a <- condStmt
    return(a)
  <|>
  do
    a <- whileStmt
    return(a)
  <|>
  do
    a <- forStmt
    return(a)
  <|>
  do
    a <- declStmt
    return(a)
  <|>
  do
    a <- idToken
    b <- stTail
    return([a]++b)
  <|>
  do
    a <- printStmt
    return(a)
  <|>
  do
    a <- getStmt
    return(a)

-- StTail → AttrStmt | '[' Expr ']' StTail' | '(' ListExpr ')' | '.' Id '(' ListExpr ')' | ε
stTail :: P [Token]
stTail = 
  do
    a <- attrStmt
    return (a)
  <|>
  do
    a <- punc_lbracketToken
    b <- expr
    c <- punc_rbracketToken
    d <- stTail'
    return([a]++b++[c]++d)
  <|>
  do
    a <- punc_lparenToken
    b <- listExpr
    c <- punc_rparenToken
    return([a]++b++[c])
  <|>
  do
    a <- punc_dotToken
    b <- idToken
    c <- punc_lparenToken
    d <- listExpr
    e <- punc_rparenToken
    return([a,b,c]++d++[e])
  <|>
    return []
    
-- StTail' → AttrStmt | '.' Id '(' ListExpr ')'
stTail' :: P [Token]
stTail' = fail "StTail' nao implementado!"

-- CondStmt → 'if' '{' Expr '}' '(' Stmts ')' ElseStmt 'endif'
condStmt :: P [Token]
condStmt = do
  a <- kw_ifToken
  b <- punc_lbraceToken
  c <- expr
  d <- punc_rbraceToken
  e <- punc_lparenToken
  f <- stmts
  g <- punc_rparenToken
  h <- elseStmt
  i <- kw_endifToken
  return([a,b]++c++[d,e]++f++[g]++h++[i])

-- ElseStmt → 'else' ElseStmt' | ε
elseStmt :: P [Token] 
elseStmt =
  do
    a <- kw_elseToken
    b <- elseStmt'
    return([a]++b)
  <|>
    return []

-- ElseStmt' → 'if' '{' Expr '}' '(' Stmts ')' ElseStmt | '(' Stmts ')'
elseStmt' :: P [Token]
elseStmt' = 
  do
    a <- kw_ifToken
    b <- punc_lbraceToken
    c <- expr
    d <- punc_rbraceToken
    e <- punc_lparenToken
    f <- stmts
    g <- punc_rparenToken
    h <- elseStmt
    return([a,b]++c++[d,e]++f++[g]++h)
  <|>
  do
    a <- punc_lparenToken
    b <- stmts
    c <- punc_rparenToken
    return([a]++b++[c])

-- PrintStmt → 'shout' '{' Expr '}'
printStmt :: P [Token]
printStmt = 
  do
    a <- kw_shoutToken
    b <- punc_lbraceToken
    c <- expr
    d <- punc_rbraceToken
    return ([a]++[b]++c++[d])

-- GetStmt → 'get' '{' Var '}'
getStmt :: P [Token]
getStmt = do
  do
    a <- kw_getToken
    b <- punc_lbraceToken
    c <- var
    d <- punc_rbraceToken
    return ([a]++[b]++c++[d])

-- WhileStmt → 'until' '{' Expr '}' 'perform' '(' Stmts ')'
whileStmt :: P [Token]
whileStmt = do
  a <- kw_untilToken
  b <- punc_lbraceToken
  c <- expr
  d <- punc_rbraceToken
  e <- kw_performToken
  f <- punc_lparenToken
  g <- stmts
  h <- punc_rparenToken
  return([a,b]++c++[d,e,f]++g++[h])

-- ForStmt → 'for' '{' ForStmt'
forStmt :: P [Token]
forStmt = fail "ForStmt nao implementado!"

-- ForStmt' → Id ForDistinguish | DeclStmt ForRegular
forStmt' :: P [Token]
forStmt' = fail "ForStmt' nao implementado!"

-- ForDistinguish → '->' Expr '}' '(' Stmts ')' | Var DeclStmt' ForRegular
forDistinguish :: P [Token]
forDistinguish = fail "ForDistinguish nao implementado!"

-- ForRegular → ',' Expr ',' Var AttrStmt '}' '(' Stmts ')'
forRegular :: P [Token]
forRegular = fail "ForRegular nao implementado!"

-- AttrStmt → '<-' Expr
attrStmt :: P [Token]
attrStmt =
  do
    a <- op_assignToken
    b <- expr
    return([a]++b)

-- DeclStmt → Type Id DeclStmt'
declStmt :: P [Token]
declStmt = 
  do
    a <- defType
    b <- idToken
    c <- declStmt'
    return (a++[b]++c)

-- DeclStmt' → AttrStmt | ε
declStmt' :: P [Token]
declStmt' = 
  do
    a <- attrStmt
    return (a)
  <|>
  do
    return []

-- Var → Id Var'
var :: P [Token]
var = do
  a <- idToken
  b <- var'
  return([a]++b)

-- Var' → '[' Expr ']' | ε
var' :: P [Token]
var' = 
  do
    a <- punc_lbracketToken
    b <- expr
    c <- punc_rbracketToken
    return([a]++b++[c])
  <|>
    return []

-- Expr → ExprOr
expr :: P [Token]
expr = exprOr

-- ExprOr → ExprAnd ExprOr'
exprOr :: P [Token]
exprOr = do
  a <- exprAnd
  b <- exprOr'

  return(a++b)

-- ExprOr' → '||' ExprAnd ExprOr' | ε
exprOr' :: P [Token]
exprOr' = 
  do
    a <- op_andToken
    b <- exprAnd 
    c <- exprOr'
    return ([a]++b++c)
  <|>
    return []

-- ExprAnd → ExprNot ExprAnd'
exprAnd :: P [Token]
exprAnd = do
  a <- exprNot
  b <- exprAnd'
  
  return(a++b)

-- ExprAnd' → '&&' ExprNot ExprAnd' | ε
exprAnd' :: P [Token]
exprAnd' = 
  do
    a <- op_andToken
    b <- exprNot
    c <- exprAnd'
    return ([a]++b++c)
  <|>
    return []

-- ExprNot  → '!' ExprNot | ExprRel
exprNot :: P [Token]
exprNot = 
  do
    a <- op_notToken
    b <- exprNot
    return (a:b)
  <|>
  do
    a <- exprRel
    return (a)

-- ExprRel → ExprAdd ExprRel'
exprRel :: P [Token]
exprRel = do
  a <- exprAdd
  b <- exprRel' 

  return(a++b)

-- ExprRel' → Op ExprAdd | ε
exprRel' :: P [Token]
exprRel' = 
  do
    a <- op
    b <- exprAdd
    return(a:b)
  <|>
    return []

-- ExprAdd → ExprMul ExprAdd'
exprAdd :: P [Token]
exprAdd = do
  a <- exprMul
  b <- exprAdd'
  
  return(a++b)

-- ExprAdd' → '+' ExprMul ExprAdd' | '-' ExprMul ExprAdd' | ε
exprAdd' :: P [Token]
exprAdd' = 
  do
    a <- op_plusToken
    b <- exprMul
    c <- exprAdd'
    return([a]++b++c)
  <|>
  do
    a <- op_minusToken
    b <- exprMul
    c <- exprAdd'
    return([a]++b++c)
  <|>
    return []

-- ExprMul → ExprPrim ExprMul'
exprMul :: P [Token]
exprMul = do
  a <- exprPrim
  b <- exprMul'

  return (a++b)

-- ExprMul' → '*' ExprPrim ExprMul' | '/' ExprPrim ExprMul' | ε
exprMul' :: P [Token]
exprMul' = 
  do
    a <- op_asteriskToken
    b <- exprPrim
    c <- exprMul'
    return([a]++b++c)
  <|>
  do
    a <- op_slashToken
    b <- exprPrim
    c <- exprMul'
    return([a]++b++c)
  <|>
    return []

-- ExprPrim → '(' Expr ')' | '[' ListTail | Id ExprPrim' | 'true' | 'false' | Int | Float | String
exprPrim :: P [Token]
exprPrim = 
  do
    a <- punc_lparenToken
    b <- expr
    c <- punc_rparenToken
    return([a]++b++[c])
  <|>
  do
    a <- punc_lbracketToken
    b <- listTail
    return(a:b)
  <|>
  do
    a <- idToken
    b <- exprPrim'
    return([a]++b)
  <|>
  do
    a <- kw_trueToken
    return([a])
  <|>
  do
    a <- kw_falseToken
    return([a])
  <|>
  do
    a <- intToken
    return([a])
  <|>
  do
    a <- floatToken
    return([a])
  <|>
  do
    a <- stringToken
    return([a])

-- ExprPrim' → '[' Expr ']' | '(' ListExpr ')' ExprPrim' | '.' Id ExprPrim' | ε
exprPrim' :: P [Token]
exprPrim' = 
  do
    a <- punc_lbracketToken
    b <- expr
    c <- punc_rbracketToken
    return ([a]++b++[c])
  <|>
  do
    a <- punc_lparenToken
    b <- listExpr
    c <- punc_rparenToken
    d <- exprPrim'
    return ([a]++b++[c]++d)
  <|>
  do
    a <- punc_dotToken
    b <- idToken
    c <- exprPrim'
    return ([a]++[b]++c)
  <|>
  do
    return ([])

-- ListExpr → Expr ListExpr' | ε
listExpr :: P [Token]
listExpr = 
  do
    a <- expr
    b <- listExpr'
    return (a++b)
  <|>
  do
    return ([])

-- ListExpr' → ',' ListExpr | ε
listExpr' :: P [Token]
listExpr' = 
  do
    a <- punc_commaToken
    b <- listExpr
    return ([a]++b)
  <|>
  do
    return([])

-- ListTail → Id '->' Expr '|' Expr ']' | ListExpr ']'
listTail :: P [Token]
listTail = 
  do
    a <- idToken
    b <- op_iteratorToken
    c <- expr
    d <- op_comprehensionToken
    e <- expr
    f <- punc_rbracketToken
    return ([a,b]++c++[d]++e++[f])
  <|>
  do
    a <- listExpr
    b <- punc_rbracketToken
    return(a++[b])

-- Op → '<=' | '>=' | '<' | '>' | '==' | '!='
op :: P Token
op = op_leqToken
  <|> op_geqToken
  <|> op_lesserToken
  <|> op_greaterToken
  <|> op_eqToken
  <|> op_neqToken
  

-----------------------------------------------------------------------------
-- FUNÇÕES PARA VERIFICAÇÃO DE TIPOS
-----------------------------------------------------------------------------
---

---


parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT defProgram [] "🚨 Erro sintático !!!" tokens

main :: IO ()
main = do
        args <- getArgs
        case args of 
          [fn] -> do
            tokens <- getTokens fn
            result <- parser tokens
            case result of
            { Left err -> print err; 
              Right ans -> print ans
            }
          _ -> putStrLn "Please inform the input filename. Closing application..." 