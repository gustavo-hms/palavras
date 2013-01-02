module Palavras where

import

data Tipo = Prefixo | Sufixo deriving Show

data Afixo = Afixo {
        tipo :: Tipo,
        símbolo :: Char,
        permiteCruzamentos :: Bool,
        quantidade :: Int,
        regras :: [Regra]
    } deriving Show

data Regra = Regra {
        símbolosARemover :: Int,
        texto :: String,
        condição :: String -> Bool
    } deriving Show

criarAfixo :: [String] -> Maybe Afixo
criarAfixo ("PFX":símb:cruzamentos:qtd:[]) =
    Just $ Prefixo (head símb) (podeCruzar cruzamentos) (read qtd) []
criarAfixo ("SFX":símb:cruzamentos:qtd:[]) =
    Just $ Sufixo (head símb) (podeCruzar cruzamentos) (read qtd) []
criarAfixo _ = Nothing

podeCruzar :: String -> Bool
podeCruzar "Y" = True
podeCruzar _   = False

inserirRegra :: [String] -> Afixo -> Maybe Afixo
inserirRegra _             Nothing = Nothing
inserirRegra especificação afixo   = case regra of
    Just r  -> Afixo t s c qtd (r:rs)
    Nothing -> Nothing
    where regra = criarRegra especificação afixo
          t = tipo afixo
          s = símbolo afixo
          c = permiteCruzamentos afixo
          qtd = quantidade afixo
          rs = regras afixo

criarRegra :: [String] -> Maybe Regra
criarRegra ("PFX":símb:remover:prefixo:contexto:[]) =
    Just $ Regra Prefixo (head símb) (read remover) prefixo (criarCondição contexto)
criarRegra ("SFX":símb:remover:sufixo:contexto:[]) =
    Just $ Regra Sufixo (head símb) (read remover) sufixo (criarCondição contexto)
criarRegra _ = Nothing
