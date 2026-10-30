module Main (main) where

import Scanner
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  -- get_token (Token p Program) = Just (Token p Program)
  get_token token@(Token p Program) = Just token
  get_token _ = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token token@(Token p (Id x)) = Just token
  get_token _  = Nothing

varToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Var) = Just token
  get_token _ = Nothing  

beginToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Begin) = Just token
  get_token _ = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token token@(Token p End) = Just token
  get_token _ = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token token@(Token p SemiColon) = Just token
  get_token _ = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Colon) = Just token
  get_token _ = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Assign) = Just token
  get_token _ = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Int x)) = Just token
  get_token _ = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Type x)) = Just token
  get_token _ = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- varToken
            d <- varDecl
            e <- beginToken 
            f <- stmts
            g <- endToken
            eof
            return (a:b:[c] ++ d++ [e] ++ f ++ [g])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- idToken
            b <- colonToken
            c <- typeToken
            updateState(symtable_insert (a, get_default_value c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return (a:b:[c])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- semiColonToken
                      b <- assign
                      c <- remaining_stmts
                      return (a:b ++ c)) <|> (return [])

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Token p (Type "int")) = Token p (Int 0)          

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (token1@(Token p1 c1), v1) ((token2@(Token p2 c2), v2):t) = 
                               if c1 == c2 then (token1, v1) : t
                               else (token2, v2) : symtable_update (token1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (token1@(Token p1 c1), v1) ((token2@(Token p2 c2), v2):t) = 
                               if c1 == c2 then t
                               else (token2, v2) : symtable_remove (token1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Parsing error!" tokens

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