module Palavras where

import Afixos
import qualified Data.Map.Lazy as M
import Data.List (partition)

data Palavra = Palavra {
        raiz :: String,
        prefixos :: [Afixo],
        sufixos :: [Afixo]
    }

criarPalavra :: String -> M.Map Char Afixo -> Palavra
criarPalavra p m
    | null símbs = Palavra txt [] []
    | otherwise  = Palavra txt pref suf
    where (txt, símbs) = break (== '/') p
          (pref, suf)  = partition prefixo (map (m M.!) (tail símbs))

expandir :: Palavra -> [String]
expandir p = p:comPrefixos ++ comCruzamentos ++ comSufixos
    where comPrefixos = expandirPrefixos (raiz p) pfxSemCruzamentos
          comCruzamentos = expandirCruzamentos (raiz p) pfxComCruzamentos sfxComCruzamentos
          comSufixos = expandirSufixos (raiz p) sfxSemCruzamentos
          (pfxSemCruzamentos, pfxComCruzamentos) = partition permiteCruzamentos (prefixos p)
          (sfxSemCruzamentos, sfxComCruzamentos) = partition permiteCruzamentos (sufixos p)

expandirPrefixos :: String -> [Afixo] -> [String]
