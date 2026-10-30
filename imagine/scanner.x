-- 
-- $idchar     = [$alphanum \_ \']

-- @string     = \" ($graphic # \")* \"
-- @id         = $alpha $idchar*
-- @smac       = '$' id
-- @rmac       = '@' id
-- @char       = ($graphic # $special) | @escape
-- @escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
-- @code       = -- curly braces surrounding a Haskell code fragment
-- https://haskell-alex.readthedocs.io/en/latest/syntax.html#lexical-syntax

{
module Scanner where

import System.IO
}

%wrapper "posn"

-- $digit = 0-9      -- numeros
-- $alpha = [a-zA-Z] -- alfabeto
-- $sym = [\! \@ \# \$ \% \^ \& \( \) \[ \] \{ \} \- \_ \' \" \= \+ \* \; \: \, \. \< \> \\ \~ \`] -- simbolos
-- $space = \32 -- espaco
-- $newspace = \n



$digit      = [0-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white

$lower      = a-z
$upper      = A-Z
$alpha      = [$upper $lower]
$alphanum   = [$alpha $digit]
$underscore = \_

-- int | float | bool | u8 | complex . . . { \p s -> Token p (Type s)}

tokens :-
    -- palavras reservadas e afins
    $white+                                ;
    "--".*                                 ;
    
    int                                    { \p s -> Token p KW_INT}
    bool                                   { \p s -> Token p KW_BOOL}
    float                                  { \p s -> Token p KW_FLOAT}
    string                                 { \p s -> Token p KW_STRING}
    u8                                     { \p s -> Token p KW_U8}
    complex                                { \p s -> Token p KW_COMPLEX}
    struct ?                               { \p s -> Token p KW_STRUCT}
    kernel                                 { \p s -> Token p KW_KERNEL}
    image                                  { \p s -> Token p KW_IMAGE}
    signal                                 { \p s -> Token p KW_SIGNAL}
    procedure                              { \p s -> Token p KW_PROCEDURE}
    function                               { \p s -> Token p KW_FUNCTION}
    main                                   { \p s -> Token p KW_MAIN}
    return                                 { \p s -> Token p KW_RETURN}
    if                                     { \p s -> Token p KW_IF}
    endif                                  { \p s -> Token p KW_ENDIF}
    else                                   { \p s -> Token p KW_ELSE}
    until                                  { \p s -> Token p KW_UNTIL}
    perform                                { \p s -> Token p KW_PERFORM}    
    for                                    { \p s -> Token p KW_FOR}
    type                                   { \p s -> Token p KW_TYPE}    
    enum                                   { \p s -> Token p KW_ENUM}
      

    -- operadores  
    "<-"                                      { \p s -> Token p OP_ASSIGN} 
    "->"                                      { \p s -> Token p OP_ITERATOR} 
    "|"                                       { \p s -> Token p OP_COMPREHENSION} 
    "+"                                       {\p s -> Token p OP_PLUS}
    "-"                                       {\p s -> Token p OP_MINUS}
    "*"                                       {\p s -> Token p OP_ASTERISK}
    "/"                                       {\p s -> Token p OP_SLASH}
    "=="                                      {\p s -> Token p OP_EQ}
    "!="                                      {\p s -> Token p OP_NEQ}
    ">"                                       {\p s -> Token p OP_GREATER}
    "<"                                       {\p s -> Token p OP_LESSER}
    ">="                                      {\p s -> Token p OP_GEQ}
    "<="                                      {\p s -> Token p OP_LEQ}
    "&&"                                      {\p s -> Token p OP_AND}
    "||"                                      {\p s -> Token p OP_OR}
    "!"                                       {\p s -> Token p OP_NOT}
    
    -- pontuacao
    -- "(" 
    -- ")"
    -- "{"
    -- "}"
    -- <<
    -- >>
    -- ;
    -- :
    -- .





    $alpha [alphanum $underscore]*          {\s -> Id s }
    \" [$printable]* \"                     {\s -> String s }


{
data Token =
    Token AlexPosn TokenClass
    deriving (Eq,Show)

data TokenClass =
  OP_ASSIGN |
  OP_PLUS |
  OP_MINUS |
  OP_ASTERISK |
  OP_SLASH |
  OP_BACKSLASH |
  OP_EQ |
  OP_NEQ |
  OP_GREATER |
  OP_LESSER |
  OP_GEQ |
  OP_LEQ |
  OP_AND |
  OP_OR |
  OP_NOT |
  KW_INT |
  KW_BOOL |
  KW_FLOAT |
  KW_STRING |
  KW_U8 |
  KW_COMPLEX |
  KW_STRUCT |
  KW_KERNEL |
  KW_IMAGE |
  KW_SIGNAL |
  KW_PROCEDURE |
  KW_FUNCTION |
  KW_MAIN |
  KW_RETURN |
  KW_IF |
  KW_ENDIF |
  KW_ELSE |
  KW_UNTIL |
  KW_PERFORM |    
  KW_FOR |
  KW_TYPE |    
  KW_ENUM |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

  
getTokens fn = do
                  fh <- openFile fn ReadMode;
                  s <- hGetContents fh;
                  return (alexScanTokens s)
}