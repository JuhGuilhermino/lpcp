module Main (main) where

import Scanner
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment
import Text.Parsec (token, eof)

-- (mari) pelo q entendi a assinatura de todas as regras de produçao eh assim
-- REGRA :: ParsecT [Token] [(Token,Token)] IO ([Token])
-- i.e. ParsecT <ENTRADA> <ESTADO_USUARIO> <IO> <TIPO_RETORNO> 
-- entao conseguimos fazer um apelido p diminuir retrabalho ! 
-- (caso a gnt mude alguma coisa na tabela de simbolos)

type P res = ParsecT [Token] [(Token,Token)] IO res

kw_programToken = tokenPrim show update_pos get_token where
  -- get_token (Token p Program) = Just (Token p Program)
  get_token token@(Token p KW_PROGRAM) = Just token
  get_token _ = Nothing

-- Tokens para variáveis
kw_intToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_INT) = Just token
  get_token _ = Nothing

kw_boolToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_BOOL) = Just token
  get_token _ = Nothing

kw_floatToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_FLOAT) = Just token
  get_token _ = Nothing

-- TODO: STRING

kw_u8Token = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_U8) = Just token
  get_token _ = Nothing

kw_complexToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_COMPLEX) = Just token
  get_token _ = Nothing

kw_kernelToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_KERNEL) = Just token
  get_token _ = Nothing

kw_imageToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_IMAGE) = Just token
  get_token _ = Nothing

kw_signalToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_SIGNAL) = Just token
  get_token _ = Nothing

kw_typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_TYPE) = Just token
  get_token _ = Nothing
  
kw_enumToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_ENUM) = Just token
  get_token _ = Nothing

kw_structToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_STRUCT) = Just token
  get_token _ = Nothing

kw_listToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_LIST) = Just token
  get_token _ = Nothing

kw_trueToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_TRUE) = Just token
  get_token _ = Nothing

kw_falseToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_FALSE) = Just token
  get_token _ = Nothing


-- subprogramas

kw_procedureToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_PROCEDURE) = Just token
  get_token _ = Nothing

kw_functionToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_FUNCTION) = Just token
  get_token _ = Nothing

kw_mainToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_MAIN) = Just token
  get_token _ = Nothing

kw_returnToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_RETURN) = Just token
  get_token _ = Nothing


-- controle & comandos

kw_ifToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_TOKEN) = Just token
  get_token _ = Nothing

kw_endifToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_ENDIF) = Just token
  get_token _ = Nothing

kw_elseToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_ELSE) = Just token
  get_token _ = Nothing

kw_untilToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_UNTIL) = Just token
  get_token _ = Nothing

kw_performToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_PERFORM) = Just token
  get_token _ = Nothing

kw_forToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_FOR) = Just token
  get_token _ = Nothing

kw_shoutToken = tokenPrim show update_pos get_token where
  get_token token@(Token p KW_SHOUT) = Just token
  get_token _ = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token token@(Token p (Id x)) = Just token
  get_token _  = Nothing


-- operadores 

op_assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_ASSIGN) = Just token
  get_token _ = Nothing

op_iteratorToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_ITERATOR) = Just token
  get_token _ = Nothing

op_comprehensionToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_COMPREHENSION) = Just token
  get_token _ = Nothing

op_plusToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_PLUS) = Just token
  get_token _ = Nothing

op_minusToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_MINUS) = Just token
  get_token _ = Nothing

op_asteriskToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_ASTERISK) = Just token
  get_token _ = Nothing

op_slashToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_SLASH) = Just token
  get_token _ = Nothing

op_eqToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_EQ) = Just token
  get_token _ = Nothing

op_neqToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_NEQ) = Just token
  get_token _ = Nothing

op_greaterToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_GREATER) = Just token
  get_token _ = Nothing

op_lesserToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_LESSER) = Just token
  get_token _ = Nothing

op_geqToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_GEQ) = Just token
  get_token _ = Nothing

op_leqToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_LEQ) = Just token
  get_token _ = Nothing

op_andToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_AND) = Just token
  get_token _ = Nothing

op_orToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_OR) = Just token
  get_token _ = Nothing

op_notToken = tokenPrim show update_pos get_token where
  get_token token@(Token p OP_NOT) = Just token
  get_token _ = Nothing


-- pontuacao

punc_lparenToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_LPAREN) = Just token
  get_token _ = Nothing

punc_rparenToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_RPAREN) = Just token
  get_token _ = Nothing

punc_lbraceToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_LBRACE) = Just token
  get_token _ = Nothing

punc_rbraceToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_RBRACE) = Just token
  get_token _ = Nothing

punc_lbracketToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_LBRACKET) = Just token
  get_token _ = Nothing

punc_rbracketToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_RBRACKET) = Just token
  get_token _ = Nothing

punc_semicolonToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_SEMICOLON) = Just token
  get_token _ = Nothing

punc_colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_COLON) = Just token
  get_token _ = Nothing

punc_commaToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_COMMA) = Just token
  get_token _ = Nothing

punc_dotToken = tokenPrim show update_pos get_token where
  get_token token@(Token p PUNC_DOT) = Just token
  get_token _ = Nothing
  
  

-- posicoes dos tokens (linha, coluna)
update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos


-- parsers p tokens
progToken :: P Token
progToken = tokenPrim show update_pos get_token where
    get_token token@(Token p KW_PROGRAM) = Just token
    get_token _ = Nothing

-- continuar...

--- parsers p nao terminais
program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- kw_programToken
            b <- kw_mainToken
            c <-  kw_functionToken
            d <- kw_procedureToken
            e <- kw_returnToken
            eof
            return (a:[b]++[c])
-- program = do
--     a <- progToken
--     -- outras regras q nao definimos ainda...
--     eof
--     return algo

-- continuar...

Stmts :: ParsecT [Token] [(Token,Token)] IO ([Token])
Stmts = do
        first <- St 
        sc <- 
        next <-

-- invocação do parser p/ simbolo de partida 
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "🚨 Erro sintático !!!" tokens


-----------------------------------------------------------------------------
-- FUNÇÕES PARA VERIFICAÇÃO DE TIPOS
-----------------------------------------------------------------------------
---

---







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