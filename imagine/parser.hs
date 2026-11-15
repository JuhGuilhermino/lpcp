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
program = undefined
-- program = do
--     a <- progToken
--     -- outras regras q nao definimos ainda...
--     eof
--     return algo

-- continuar...


-- invocação do parser p/ simbolo de partida 
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "🚨 Erro sintático !!!" tokens

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