module Palavras where

import qualified Data.Map as M

data Tipo = Prefixo | Sufixo deriving Show

data Afixo = Afixo {
        tipo :: Tipo,
        permiteCruzamentos :: Bool,
        quantidade :: Int,
        regras :: [Regra]
    }

data Regra = Regra {
        símbolosARemover :: Int,
        afixo :: String,
        condição :: String -> Bool
    }

extrairTipo :: [String] -> (Maybe Tipo, [String])
extrairTipo ("PFX":resto) = (Just Prefixo, resto)
extrairTipo ("SFX":resto) = (Just Sufixo, resto)
extrairTipo string        = (Nothing, string)

especificação :: [String] -> (Char, Bool, Int)
especificação (símbolo:cruzamentos:quantidade)

criarAfixo :: [String] -> Afixo
criarAfixo (regra:regras) =
    let (tipo, resto) = extrairTipo regra
        símbolo = head resto
        cruzamentos = head $ tail resto
        quantidade = last resto
