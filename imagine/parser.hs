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

-- DefSub → 'procedure' DefProc DefSub | 'function' DefFunc DefSub | ε
defSub :: P [Token]
defSub = return []

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

-- Stmts → St ';' Stmts | ε
stmts :: P [Token]
stmts = return []

-- continuar...



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