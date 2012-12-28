module Palavras where

data Afixo = Prefixo | Sufixo deriving Show

afixo :: String -> (Maybe Afixo, String)
afixo ('P':'F':'X':' ':resto) = (Just Prefixo, resto)
afixo ('S':'F':'X':' ':resto) = (Just Sufixo, resto)
afixo string                  = (Nothing, string)
