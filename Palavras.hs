module Palavras where

data Afixo = Prefixo | Sufixo deriving Show

tipoDoAfixo :: String -> Afixo
tipoDoAfixo ('P':'F':'X':resto) = Prefixo
tipoDoAfixo ('S':'F':'X':resto) = Sufixo
