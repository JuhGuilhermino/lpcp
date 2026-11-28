module Main (main) where

import Scanner
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment
import qualified Data.Map as Map
import Data.List
import System.Exit
import System.IO
import Data.Ord (Ordering(LT, EQ))
import GHC.IO.Handle (hSetEncoding)

data IMType = TInt | TBool | TFloat | TString | TU8 | TComplex | TKernel | TImage | TSignal | TList IMType | TMatrix IMType 
  deriving (Eq, Show)

data IMValue = VInt Int | VBool Bool | VFloat Double | VString String | VList [IMValue] 
  deriving (Eq, Show)

data IMState = IMState{
  scopes :: [Map.Map String (IMType, IMValue)],
  -- buffer :: [String]
  executing :: Bool -- para executar ou apenas parsear
}

type P res = ParsecT [Token] IMState IO res

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

kw_matrixToken :: P Token
kw_matrixToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_MATRIX) = Just token
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

kw_listenToken :: P Token
kw_listenToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ KW_LISTEN) = Just token
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

op_moduloToken :: P Token
op_moduloToken = tokenPrim show update_pos get_token where
    get_token token@(Token _ OP_MODULO) = Just token
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
defProgram :: P ()
defProgram = do
  a <- defSub
  b <- defMain
  eof
  -- return (a++b)

-- DefMain → 'main' '{' '}' '(' Stmts ')'
defMain :: P ()
defMain = do
  a <- kw_mainToken
  b <- punc_lbraceToken
  c <- punc_rbraceToken
  d <- punc_lparenToken
  e <- stmts
  f <- punc_rparenToken
  return()
  -- return ([a,b,c,d]++e++[f])

-- [TO-DO] DefSub → 'procedure' DefProc DefSub | 'function' DefFunc DefSub | ε
defSub :: P ()
defSub = 
  do
    a <- kw_procedureToken
    b <- defProc
    c <- defSub
    return ()
    -- return ([a]++b++c)
  <|>
  do
    a <- kw_functionToken
    b <- defFunc
    c <- defSub
    return ()
    -- return ([a]++b++c)
  <|>
    return ()

-- [TO-DO] DefFunc → Type Id '{' Params '}' '(' Stmts 'return' Expr ';' ')'
defFunc :: P ()
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
  return ()
  -- return(a++[b,c]++d++[e,f]++g++[h]++i++[j,k])

-- [TO-DO] DefProc → Id '{' Params '}' '(' Stmts ')'
defProc :: P ()
defProc = do
  a <- idToken
  b <- punc_lbraceToken
  c <- params
  d <- punc_rbraceToken
  e <- punc_lparenToken
  f <- stmts
  g <- punc_rparenToken
  return ()
  -- return([a,b]++c++[d,e]++f++[g])

-- [TO-DO] Params → Type Id Params' | ε
params :: P ()
params = 
  do
    a <- defType
    b <- idToken
    c <- params'
    return ()
    -- return(a++[b]++c)
  <|>
    return ()

-- [TO-DO] Params' → ',' Type Id Params' | ε
params' :: P ()
params' = 
  do
    a <- punc_commaToken
    b <- defType
    c <- idToken
    d <- params'
    return ()
    -- return([a]++b++[c]++d)
  <|>
    return ()

-- Type → PrimType | StructType
defType :: P IMType
defType =
  do
    a <- primType
    return a
  <|>
  do
    b <- structType
    return b

-- PrimType → 'int' | 'bool' | 'float' | 'string' | 'u8' | 'complex' | 'kernel' | 'image' | 'signal'
primType :: P IMType
primType =
  do
    _ <- kw_intToken
    return TInt
  <|>
  do
    _ <- kw_boolToken
    return TBool
  <|>
  do
    _ <- kw_floatToken
    return TFloat
  <|>
  do
    _ <- kw_stringToken
    return TString
  <|>
  do
    _ <- kw_u8Token
    return TU8
  <|>
  do
    _ <- kw_complexToken
    return TComplex
  <|>
  do
    _ <- kw_kernelToken
    return TKernel
  <|>
  do
    _ <- kw_imageToken
    return TImage
  <|>
  do
    _ <- kw_signalToken
    return TSignal


-- StructType → 'list' '<' Type '>' | 'matrix' '<' PrimType '>'
structType :: P IMType
structType = 
  do
    _ <- kw_listToken
    _ <- op_lesserToken
    c <- defType
    _ <- op_greaterToken
    return (TList c)
  <|>
  do
    _ <- kw_matrixToken
    _ <- op_lesserToken
    c <- primType
    _ <- op_greaterToken
    return (TMatrix c)

-- Stmts → St Stmts | ε
stmts :: P ()
stmts = 
  do
    a <- st 
    b <- stmts
    return ()
    -- return (a++b)
  <|>
    return ()

-- St → SimpleStmt ';' | BlockStmt
st :: P ()
st =
  do
    a <- simpleStmt
    b <- punc_semicolonToken
    return ()
    -- return(a++[b])
  <|>
  do
    a <- blockStmt
    return ()
    -- return(a)

-- SimpleStmt → DeclStmt | Id StTail | PrintStmt | ListenStmt
simpleStmt :: P ()
simpleStmt = 
  declStmt
  <|>
  do
    pos <- getPosition
    a <- idToken
    stTail pos (idName a)
  <|>
  do
    printStmt
  <|>
  do
    listenStmt

-- [TO-DO] Block Stmt → CondStmt | WhileStmt | ForStmt
blockStmt :: P ()
blockStmt =
  condStmt
  <|>
  whileStmt
  <|>
  do
    pos <- getPosition
    a <- forStmt
    throwError pos ("ForStmt ainda n implementado")
    -- return(a)

-- [TO-DO] StTail → AttrStmt | '[' Expr ']' StTail' | '(' ListExpr ')' | '.' Id '(' ListExpr ')' | ε
stTail :: SourcePos -> String -> P ()
stTail pos name = 
  do
    a <- attrStmt
    ifExec () (do -- so atribui se tiver executing = True
      (typ, _) <- symbtable_lookup pos name
      b <- coerce pos typ a
      symbtable_update pos name b 
      )  
  <|>
  do
    a <- punc_lbracketToken
    throwError pos ("[Expr] de StTail ainda n implementado")
    -- b <- expr
    -- c <- punc_rbracketToken
    -- d <- stTail'
    -- return([a]++b++[c]++d)
  <|>
  do
    a <- punc_lparenToken
    throwError pos ("(ListExpr) de StTail ainda n implementado")
    -- b <- listExpr
    -- c <- punc_rparenToken
    -- return([a]++b++[c])
  <|>
  do
    a <- punc_dotToken
    throwError pos (".Id(ListExpr) de StTail ainda n implementado")
    -- b <- idToken
    -- c <- punc_lparenToken
    -- d <- listExpr
    -- e <- punc_rparenToken
    -- return([a,b,c]++d++[e])
  <|>
    return ()
    
-- [TO-DO] StTail' → AttrStmt | '.' Id '(' ListExpr ')'
stTail' :: P ()
stTail' = 
  do
    a <- attrStmt
    return ()
    -- return (a)
  <|>
  do
    a <- punc_dotToken
    b <- idToken
    c <- punc_lparenToken
    d <- listExpr
    e <- punc_rparenToken
    return ()
    -- return ([a,b,c]++d++[e])

-- CondStmt → 'if' '{' Expr '}' '(' Stmts ')' ElseStmt 'endif'
condStmt :: P ()
condStmt = do
  a <- kw_ifToken
  b <- punc_lbraceToken
  pos <- getPosition -- posiçao para caso de erro
  c <- expr -- valor da condicao
  d <- punc_rbraceToken
  exc <- isExec -- "o if esta numa parte do codigo com executing=True?"
  cond <- if exc then isBool pos c else return False -- se sim, verifica e usa a condiçao, senao falso
  runBlock (exc && cond) -- se estiver numa parte executavel e a condiçao for verdadeira, executa o bloco do if
  elseStmt (exc && not cond) -- se o bloco do if nao foi executado propaga para elseStmt
  e <- kw_endifToken
  return ()

-- ElseStmt → 'else' ElseStmt' | ε
elseStmt :: Bool -> P () 
elseStmt pending =
  do
    a <- kw_elseToken
    elseStmt' pending -- propaga essa info pros elseifs...else
    return ()
  <|>
    return ()

-- ElseStmt' → 'if' '{' Expr '}' '(' Stmts ')' ElseStmt | '(' Stmts ')'
elseStmt' :: Bool -> P ()
elseStmt' pending = 
  --elseif
  do
    a <- kw_ifToken
    b <- punc_lbraceToken
    pos <- getPosition
    c <- withExec pending expr -- condiçao avaliada se tiver podendo executar
    d <- punc_rbraceToken
    cond <- if pending then isBool pos c else return False -- se tivermos "pendencias" a serem executadas, vamos verificar e usar a condiçao, senao falso
    runBlock (pending && cond) -- executa o bloco se tiver numa parte executavel e a condiçao for verdadeira
    elseStmt (pending && not cond) -- senao propaga a info
  <|>
  --ultimo else
  do
    runBlock pending

-- PrintStmt → 'shout' '{' Expr '}'
printStmt :: P ()
printStmt = 
  do
    a <- kw_shoutToken
    b <- punc_lbraceToken
    c <- expr
    d <- punc_rbraceToken
    ifExec () (liftIO (putStrLn (showValue c))) -- so imprime se tiver executing=true

-- [TO-DO] ListenStmt → 'get' '{' Var VarList '}'
listenStmt :: P ()
listenStmt = do
  a <- kw_listenToken
  b <- punc_lbraceToken
  c <- var
  d <- varList
  e <- punc_rbraceToken
  return ()
  -- return ([a]++[b]++c++d++[e])

-- WhileStmt → 'until' '{' Expr '}' 'perform' '(' Stmts ')'
whileStmt :: P ()
whileStmt = do
  a <- kw_untilToken
  startInput <- getInput  -- dados do começo de {}
  startPos <- getPosition -- dados do começo de {}
  whileLoop startInput startPos
  where
    -- cada whileloop eh uma execucao do laço
    whileLoop startInput startPos = do
      setInput startInput     -- volta p começo
      setPosition startPos    -- volta p começo
      b <- punc_lbraceToken   
      pos <- getPosition      -- posiçao da condiçao
      c <- expr               -- reavaliacao da condiçao
      d <- punc_rbraceToken   
      e <- kw_performToken
      exc <- isExec           -- verifica se esta numa parte executavel do codigo
      stop <- if exc then isBool pos c else return True   -- condiçao foi atingida?
      runBlock (not stop) -- se nao foi atingida, roda o bloco
      if stop then return () else whileLoop startInput startPos -- se foi atingida, sai do loop, se nao foi repete o laço

-- [TO-DO] ForStmt → 'for' '{' ForStmt'
forStmt :: P ()
forStmt = do
  a <- kw_forToken
  b <- punc_lbraceToken
  c <- forStmt'
  return ()
  -- return ([a,b]++c)

-- [TO-DO] ForStmt' → Id ForDistinguish | DeclStmt ForRegular
forStmt' :: P ()
forStmt' = 
  do
    a <- idToken
    b <- forDistinguish
    return ()
    -- return([a]++b)
  <|>
  do
    a <- singleDecl
    b <- forRegular
    return ()
    -- return (a++b)

-- [TO-DO] ForDistinguish → '->' Expr '}' '(' Stmts ')' | Var DeclStmt' ForRegular
forDistinguish :: P ()
forDistinguish = 
  do
    a <- op_iteratorToken
    b <- expr
    c <- punc_rbraceToken
    d <- punc_lparenToken
    e <- stmts
    f <- punc_rparenToken
    return ()
    -- return ([a]++b++[c,d]++e++[f])
  <|>
  do
    a <- var
    b <- declStmt'
    c <- forRegular
    return ()
    -- return (a++b++c)

-- [TO-DO] ForRegular → ',' Expr ',' Var AttrStmt '}' '(' Stmts ')'
forRegular :: P ()
forRegular = do
  a <- punc_commaToken
  b <- expr
  c <- punc_commaToken
  d <- var
  e <- attrStmt
  f <- punc_rbraceToken
  g <- punc_lparenToken
  h <- stmts
  i <- punc_rparenToken
  return ()
  -- return ([a]++b++[c]++d++e++[f,g]++h++[i])

-- AttrStmt → '<-' Expr
attrStmt :: P IMValue
attrStmt =
  do
    _ <- op_assignToken
    b <- expr
    return b

-- DeclStmt → SingleDecl DeclList
declStmt :: P ()
declStmt = do
  typ <- singleDecl
  b <- declList typ
  return b

-- SingleDecl → Type Id DeclStmt'
singleDecl :: P IMType
singleDecl = do
  typ <- defType
  pos <- getPosition
  name <- idToken
  c <- declStmt'
  ifExec () (do -- so declara se tiver executing = True
    val <- case c of 
      Nothing -> get_default_value pos typ
      Just v -> coerce pos typ v

    symbtable_insert pos (idName name) typ val
    )

  return typ

-- DeclList → ',' Id DeclStmt' DeclList | ε
declList :: IMType -> P ()
declList typ =
  do
    _ <- punc_commaToken
    pos <- getPosition
    name <- idToken
    c <- declStmt'
    ifExec ()(do -- so declara se tiver executing = True
      val <- case c of 
        Nothing -> get_default_value pos typ
        Just v -> coerce pos typ v

      symbtable_insert pos (idName name) typ val 
      )

    d <- declList typ
    return d
  <|>
    return ()

-- DeclStmt' → AttrStmt | ε
declStmt' :: P (Maybe IMValue) 
declStmt' = 
  do
    a <- attrStmt
    return (Just a)
  <|>
  do
    return Nothing

-- [TO-DO] Var → Id Var'
var :: P ()
var = do
  a <- idToken
  b <- var'
  return ()
  -- return([a]++b)

-- [TO-DO] Var' → '[' Expr ']' | ε
var' :: P ()
var' = 
  do
    a <- punc_lbracketToken
    b <- expr
    c <- punc_rbracketToken
    return ()
    -- return([a]++b++[c])
  <|>
    return ()

-- [TO-DO] VarList → ',' Var VarList | ε
varList :: P ()
varList = 
  do
    a <- punc_commaToken
    b <- var
    c <- varList
    return ()
    -- return([a]++b++c)
  <|>
    return ()

-- Expr → ExprOr
expr :: P IMValue
expr = exprOr

-- ExprOr → ExprAnd ExprOr'
exprOr :: P IMValue
exprOr = do
  a <- exprAnd
  b <- exprOr' a

  return b

-- ExprOr' → '||' ExprAnd ExprOr' | ε
exprOr' :: IMValue -> P IMValue
exprOr' esq = 
  do
    pos <- getPosition
    a <- op_orToken
    b <- exprAnd 
    c <- logical pos "||" (||) esq b
    d <- exprOr' c
    return d
  <|>
    return esq

-- ExprAnd → ExprNot ExprAnd'
exprAnd :: P IMValue
exprAnd = do
  a <- exprNot
  b <- exprAnd' a
  
  return b

-- ExprAnd' → '&&' ExprNot ExprAnd' | ε
exprAnd' :: IMValue -> P IMValue
exprAnd' esq = 
  do
    pos <- getPosition
    a <- op_andToken
    b <- exprNot
    c <- logical pos "&&" (&&) esq b
    d <- exprAnd' c
    return d
  <|>
    return esq

-- ExprNot  → '!' ExprNot | ExprRel
exprNot :: P IMValue
exprNot = 
  do
    pos <- getPosition
    a <- op_notToken
    b <- exprNot
    case b of 
      VBool v -> return (VBool (not v))
      _ -> throwError pos ("!(not) exige booleano: " ++ showValue b)
  <|>
  do
    a <- exprRel
    return a

-- ExprRel → ExprAdd ExprRel'
exprRel :: P IMValue
exprRel = do
  a <- exprAdd
  b <- exprRel' a

  return b

-- ExprRel' → Op ExprAdd | ε
exprRel' :: IMValue -> P IMValue
exprRel' esq = 
  do
    pos <- getPosition
    a <- op
    b <- exprAdd
    c <- compareVals pos a esq b
    return c
  <|>
    return esq

-- ExprAdd → ExprMul ExprAdd'
exprAdd :: P IMValue
exprAdd = do
  a <- exprMul
  b <- exprAdd' a
  
  return b

-- ExprAdd' → '+' ExprMul ExprAdd' | '-' ExprMul ExprAdd' | ε
exprAdd' :: IMValue -> P IMValue
exprAdd' esq = 
  do
    pos <- getPosition
    a <- op_plusToken
    b <- exprMul
    c <- arithmetic pos "+" (+) (+) esq b
    d <- exprAdd' c
    return d
  <|>
  do
    pos <- getPosition
    a <- op_minusToken
    b <- exprMul
    c <- arithmetic pos "-" (-) (-) esq b
    d <- exprAdd' c
    return d
  <|>
    return esq

-- ExprMul → ExprUna ExprMul'
exprMul :: P IMValue
exprMul = do
  a <- exprUna
  b <- exprMul' a

  return b

-- ExprMul' → '*' ExprUna ExprMul' | '/' ExprUna ExprMul' | '%' ExprUna ExprMul' | ε
exprMul' :: IMValue -> P IMValue
exprMul' esq = 
  do
    pos <- getPosition
    a <- op_asteriskToken
    b <- exprUna
    c <- arithmetic pos "*" (*) (*) esq b
    d <- exprMul' c
    return d
  <|>
  do
    pos <- getPosition
    a <- op_slashToken
    b <- exprUna
    c <- divide pos esq b
    d <- exprMul' c
    return d
  <|>
  do
    pos <- getPosition
    a <- op_moduloToken
    b <- exprUna
    c <- modulo pos esq b
    d <- exprMul' c
    return d
  <|>
    return esq

-- ExprUna → '-' ExprUna | ExprPrim
exprUna :: P IMValue
exprUna = 
  do
    pos <- getPosition
    a <- op_minusToken
    b <- exprUna
    c <- unaryNegative pos b 
    return c
  <|>
  do 
    a <- exprPrim
    return a

-- ExprPrim → '(' Expr ')' | '[' ListTail | Id ExprPrim' | 'true' | 'false' | Int | Float | String
exprPrim :: P IMValue
exprPrim = 
  do
    a <- punc_lparenToken
    b <- expr
    c <- punc_rparenToken
    return b
  <|>
  do
    a <- punc_lbracketToken
    b <- listTail
    return b
  <|>
  do
    pos <- getPosition
    a <- idToken
    b <- exprPrim' pos (idName a)
    return b
  <|>
  do
    a <- kw_trueToken
    return (VBool True)
  <|>
  do
    a <- kw_falseToken
    return(VBool False)
  <|>
  do
    Token _ (Int n) <- intToken
    return (VInt n)
  <|>
  do
    Token _ (Float f) <- floatToken
    return (VFloat f)
  <|>
  do
    Token _ (String s) <- stringToken
    return (VString s)

-- ExprPrim' → '[' Expr ']' | '(' ListExpr ')' ExprPrim' | '.' Id ExprPrim' | ε
exprPrim' :: SourcePos -> String -> P IMValue
exprPrim' pos name = 
  do
    a <- punc_lbracketToken
    -- b <- expr
    -- c <- punc_rbracketToken
    -- return ([a]++b++[c])
    throwError pos ("Kkkkkkkk ainda n implementamos ExpPrim' -> [Expr]")
  <|>
  do
    a <- punc_lparenToken
    -- b <- listExpr
    -- c <- punc_rparenToken
    -- d <- exprPrim'
    -- return ([a]++b++[c]++d)
    throwError pos ("Kkkkkkkk ainda n implementamos ExpPrim' -> (ListExpr) ExprPrim'")
  <|>
  do
    a <- punc_dotToken
    -- b <- idToken
    -- c <- exprPrim'
    -- return ([a]++[b]++c)
    throwError pos ("Kkkkkkkk ainda n implementamos ExpPrim' -> .Id ExprPrim'")
  <|>
  do
    (typ, val) <- symbtable_lookup pos name
    return val

-- ListExpr → Expr ListExpr' | ε
listExpr :: P [IMValue]
listExpr = 
  do
    a <- expr
    b <- listExpr'
    return ([a]++b)
  <|>
    return []

-- ListExpr' → ',' ListExpr | ε
listExpr' :: P [IMValue]
listExpr' = 
  do
    a <- punc_commaToken
    b <- listExpr
    return b
  <|>
    return []

-- ListTail → Id '->' Expr '|' Expr ']' | ListExpr ']'
listTail :: P IMValue
listTail = 
  try
    (do
      pos <- getPosition
      a <- idToken
      b <- op_iteratorToken
      throwError pos ("ainda n implementamos ListTail -> Id '->' Expr")
      -- c <- expr
      -- d <- op_comprehensionToken
      -- e <- expr
      -- f <- punc_rbracketToken
      -- return ([a,b]++c++[d]++e++[f])
    )
  <|>
  do
    a <- listExpr
    b <- punc_rbracketToken
    return (VList a)

-- Op → '<=' | '>=' | '<' | '>' | '==' | '!='
op :: P Token
op = op_leqToken
  <|> op_geqToken
  <|> op_lesserToken
  <|> op_greaterToken
  <|> op_eqToken
  <|> op_neqToken
  

-----------------------------------------------------------------------------
-- FUNÇÕES AUXILIARES E AFINS
-----------------------------------------------------------------------------

idName :: Token -> String
idName (Token _ (Id s)) = s
idName t = show t

typeName :: IMType -> String
typeName tn = case tn of
  TInt -> "int"
  TBool -> "bool"
  TFloat -> "float"
  TString -> "string"
  TU8 -> "u8"
  TComplex -> "complex"
  TKernel -> "kernel"
  TImage -> "image"
  TSignal -> "signal"
  TList t -> "list<" ++ typeName t ++ ">"
  TMatrix t -> "matrix<" ++ typeName t ++ ">"

showValue :: IMValue -> String
showValue (VInt n) = show n 
showValue (VFloat f) = show f
showValue (VBool b) = if b then "true" else "false"
showValue (VString s) = s
showValue (VList vals) = "[" ++ intercalate ", " (map showValue vals) ++ "]"

throwError :: SourcePos -> String -> P e
throwError pos msg = liftIO (die ("[ERRO SEMÂNTICO (?)] na linha " ++ show (sourceLine pos) ++ ", coluna " ++ show (sourceColumn pos) ++ ": " ++ msg))


-- TABELA DE SÍMBOLOS
symbtable_insert :: SourcePos -> String -> IMType -> IMValue -> P () -- n retorna valor
symbtable_insert pos var typ val = do -- na posição pos...
  st <- getState
  case scopes st of
    (head:tail)
      | Map.member var head ->  throwError pos ("'" ++ var ++ "' ja declarada neste escopo!!!" ) -- n deu certo...
      | otherwise -> putState st {scopes = Map.insert var (typ, val) head:tail} -- ... insira var com tipo typ e valor val
    [] -> throwError pos ("pilha de escopos vazia") -- 

symbtable_update :: SourcePos -> String -> IMValue -> P () -- n retorna valor
symbtable_update pos var val = do -- na posição pos...
  st <- getState
  newScopes <- update (scopes st)
  putState st {scopes = newScopes}
  where
    update [] = throwError pos ("'" ++ var ++ "' n declarada" ) -- n deu certo...
    update (sc : tail) =
      case Map.lookup var sc of
        Just (typ, _) -> return (Map.insert var (typ, val) sc:tail) -- ... atualize o valor de var para val, o tipo continua
        Nothing -> do
          tail' <- update tail
          return (sc : tail')
          
symbtable_remove :: SourcePos -> String -> P () -- n retorna valor
symbtable_remove pos var = do -- na posição pos...
  st <- getState
  newScopes <- remove (scopes st)
  putState st {scopes = newScopes}
  where
    remove [] = throwError pos ("'" ++ var ++ "' n declarada" ) -- n deu certo...
    remove (sc : tail) =
      case Map.lookup var sc of
        Just _ -> return (Map.delete var sc:tail) --  -- ... apague var !
        Nothing -> do
          tail' <- remove tail
          return (sc : tail')

symbtable_lookup :: SourcePos -> String -> P (IMType, IMValue) -- retorno typ val
symbtable_lookup pos var = do -- na posição pos...
  st <- getState
  search (scopes st)
  where 
    search [] = throwError pos ("'" ++ var ++ "' n declarada" ) -- n deu certo...
    search (sc : tail) =
      case Map.lookup var sc of
        Just (typ, val) -> return (typ, val) -- ...me dê o tipo typ e o valor val de var
        Nothing -> search tail

-- AVALIAÇÃO E VERIFICAÇÃO

get_default_value :: SourcePos -> IMType -> P IMValue
get_default_value _ TInt = return (VInt 0)
get_default_value _ TU8 = return (VInt 0)
get_default_value _ TFloat = return (VFloat 0.0)
get_default_value _ TBool = return (VBool False)
get_default_value _ TString = return (VString "")
get_default_value _ (TList _) = return (VList [])
get_default_value pos t = throwError pos ("tipo " ++ typeName t ++ " n implementado (ainda!)")
         
compatible :: IMType -> IMValue -> Bool
compatible TInt (VInt _) = True
compatible TFloat (VFloat _) = True

compatible TInt (VFloat _) = True -- float p int trunca
compatible TFloat (VInt _) = True -- int p float transforma em FLOAT
compatible TU8 (VInt _) = True 
compatible TU8 (VFloat _) = True -- float p u8 trunca
-- teremos compatibilidade entre inteiros e booleanos?
-- compatible TInt (VBool _) = True
-- compatible TBool (VInt _) = True
compatible TBool (VBool _) = True
compatible TString (VString _) = True
compatible _ _ = False

convert :: IMType -> IMValue -> IMValue
convert TInt (VFloat f) = VInt (truncate f)
convert TU8 (VFloat f) = VInt (truncate f)
convert TFloat (VInt n) = VFloat (fromIntegral n)
convert _ v = v

-- isso aq eh mais pra U8 
checkRange :: SourcePos -> IMType -> IMValue -> P IMValue
checkRange pos TU8 (VInt n)
  | n < 0 || n > 255 = throwError pos ("valor n=" ++ show n ++ " fora da faixa [0,255]")
checkRange _ _ v = return v

coerce :: SourcePos -> IMType -> IMValue -> P IMValue
coerce pos typ val
  | not (compatible typ val) = throwError pos ("valor " ++ showValue val ++ " incompativel com tipo " ++ typeName typ)
  | otherwise = checkRange pos typ (convert typ val)

-- OPERAÇÕES !
-- adiçao subtraçao e multiplicaçao
-- código fonte -> string do operador -> inteiros -> float caso haja -> operando esquerdo -> operando direito -> resposta !
-- queria aproveitar o convert no retorno dessa funçao mas nao entendo mto de assinatura das funçoes 
arithmetic :: SourcePos -> String -> (Int -> Int -> Int) -> (Double -> Double -> Double) -> IMValue -> IMValue -> P IMValue
arithmetic _ _ opI _ (VInt a) (VInt b) = return (VInt (opI a b))
arithmetic _ _ _ opF (VFloat a) (VFloat b) = return (VFloat (opF a b))
arithmetic _ _ _ opF (VInt a) (VFloat b) = return (VFloat (opF (fromIntegral a) b))
arithmetic _ _ _ opF (VFloat a) (VInt b) = return (VFloat (opF a (fromIntegral b)))
arithmetic pos op _ _ a b = throwError pos ("operandos incompativeis na operacao " ++ op ++ " ---> "  ++ showValue a ++ " e " ++ showValue b)

--contudo temos a divisao por zero 
divide :: SourcePos -> IMValue -> IMValue -> P IMValue
divide pos _ (VInt 0) = throwError pos ("divisao por zero")
divide pos _ (VFloat 0.0) = throwError pos ("divisao por zero")
-- return pos, char da divisao, caso inteiro a operaçao eh div, caso float eh /, operando a e operando b
divide pos a b = arithmetic pos "/" div (/) a b

-- modulo !
modulo :: SourcePos -> IMValue -> IMValue -> P IMValue
modulo pos _ (VInt 0) = throwError pos ("erro !! modulo zero")
modulo _ (VInt a) (VInt b) = return (VInt (a `mod` b))
modulo pos a b = throwError pos ("% espera inteiros, recebeu " ++ showValue a ++ " e " ++ showValue b)

-- op unaria de valor negativo
unaryNegative :: SourcePos -> IMValue -> P IMValue
unaryNegative _ (VInt n) = return (VInt (negate n))
unaryNegative _ (VFloat f) = return (VFloat (negate f))
unaryNegative pos val = throwError pos ("operaçao unaria '-' espera int/float, recebeu " ++ showValue val)

-- ops booleanas
logical :: SourcePos -> String -> (Bool -> Bool -> Bool) -> IMValue -> IMValue -> P IMValue
logical _ _ opL (VBool a) (VBool b) = return (VBool (opL a b))
logical pos op _ a b = throwError pos (op ++ " espera bool, recebeu " ++ showValue a ++ " e " ++ showValue b)

-- COMPARAÇÕES !
compareVals :: SourcePos -> Token -> IMValue -> IMValue -> P IMValue
compareVals pos (Token _ opTkn) a b = do
  order <- case (a, b) of -- comparando os valores 
    (VInt x, VInt y) -> return (compare x y)
    (VFloat x, VFloat y) -> return (compare x y)
    (VInt x, VFloat y) -> return (compare (fromIntegral x) y)
    (VFloat x, VInt y) -> return (compare x (fromIntegral y))
    (VString x, VString y) -> return (compare x y)
    (VBool x, VBool y) 
      | opTkn == OP_EQ || opTkn == OP_NEQ -> return (compare x y)
    _ -> throwError pos ("comparacao entr valores incompativeis: " ++ showValue a ++ " e " ++ showValue b)
  
  return (VBool (case opTkn of
    OP_LESSER -> order == LT
    OP_GREATER -> order == GT
    OP_LEQ -> order /= GT
    OP_GEQ -> order /= LT
    OP_EQ -> order == EQ
    OP_NEQ -> order /= EQ
    ))


-- EXECUÇÕES E AFINS
-- p saber se a flag de execuçao esta V ou F
isExec :: P Bool
isExec = do
  st <- getState
  return (executing st)

-- rodando trecho independente da flag
withExec :: Bool -> P algo -> P algo
withExec run action = do
  st <- getState    
  let currExecVal = executing st          -- estado atual do executing
  putState st {executing = run}           -- estado que recebi em withExec
  r <- action                             -- trecho rodando
  st' <- getState                         -- releitura do estado
  putState st' {executing = currExecVal}  -- restauraçao da flag 
  return r

-- o que precisa ser feito caso executing == True
ifExec :: algo -> P algo -> P algo
ifExec defaultVal action = do
  exec <- isExec
  if exec then action else return defaultVal -- se executando faz a acao senao retorna defaultval
  
isBool :: SourcePos -> IMValue -> P Bool
isBool _ (VBool b) = return b 
isBool pos val = throwError pos ("condicao deve ser booleana, recebeu" ++ showValue val)

-- bloco de stmts ( Stmts )
runBlock :: Bool -> P ()
runBlock run = do
  a <- punc_lparenToken
  withExec run (do
    enterScope
    stmts
    exitScope
    )
  b <- punc_rparenToken

  return ()

-- AÇÕES DE ESCOPOS
enterScope :: P ()
enterScope = do
  st <- getState
  putState st {scopes = Map.empty : scopes st} -- empilhar escopo !

exitScope :: P ()
exitScope = do
  st <- getState 
  case scopes st of
    (_:tail) -> putState st {scopes = tail} -- desempilhar escopo descartando o começo (_)
    []  -> return () -- pilha de escopos vazia

-----------------------------------------------------------------------------

parser :: String -> [Token] -> IO (Either ParseError ())
parser fn tokens = runParserT defProgram (IMState [Map.empty] True) fn tokens

main :: IO ()
main = do
        mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr] -- só pra printar na codificação 
        args <- getArgs
        case args of 
          [fn] -> do
            tokens <- getTokens fn
            result <- parser fn tokens
            case result of
            { Left err -> do {hPutStrLn stderr "🚨 Erro sintático !!!"; print err};
              Right () -> return () 
            }
          _ -> putStrLn "Please inform the input filename. Closing application..." 