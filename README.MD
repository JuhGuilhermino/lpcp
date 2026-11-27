### Este repositório refere-se à implementação da linguagem _Imagine_, desenvolvida ao longo da matéria de _Linguagens de Programação: Conceitos e Paradigmas_.

_*Autores:* Alexandre Ribeiro, Carolina Nunes, Julia Guilhermino, Mariana Emerenciano_

# Dependências
- Linguagem de programação: Haskell
- Analisador léxico: Alex
- Analisador sintático: Parsec
- Packages do haskell: Data, Containers

# Compilação

1 - Execute o comando abaixo para chamar o Alex sobre o _"scanner.x"_
```
alex scanner.x
```
2 - Isso gerará um arquivo chamado _"scanner.hs"_. Renomeie-o para _"Scanner.hs"_
3 - Agora, compile a _file_ com o ghc
```
ghc Scanner.hs
```

4 - Agora, compile o arquivo _"parser.hs"_ com o ghc
``` 
ghc parser.hs
```

# Execução

5 - Execute o arquivo criado na etapa anterior, acompanhado do _path_ para o programa que deve ser interpretado
```
./parserV1 programaV1.pe
```



