module Palavras where

import Afixos
import qualified Data.Map.Lazy as M
import Data.List (partition)

data Palavra = Palavra {
        raiz     :: String,
        prefixos :: [Afixo],
        sufixos  :: [Afixo]
    }

criarPalavra :: String -> M.Map Char Afixo -> Palavra
criarPalavra p m
    | null símbs = Palavra txt [] []
    | otherwise  = Palavra txt pref suf
    where (txt, símbs) = break (== '/') p
          (pref, suf)  = partition prefixo (map (m M.!) (tail símbs))

expandir :: Palavra -> [String]
expandir p = p:comPrefixos ++ comCruzamentos ++ comSufixos
    where comPrefixos = expandirPrefixos pfxSemCruzamentos (raiz p)
          comCruzamentos = expandirCruzamentos pfxComCruzamentos sfxComCruzamentos (raiz p)
          comSufixos = expandirSufixos sfxSemCruzamentos (raiz p)
          (pfxSemCruzamentos, pfxComCruzamentos) = partition permiteCruzamentos (prefixos p)
          (sfxSemCruzamentos, sfxComCruzamentos) = partition permiteCruzamentos (sufixos p)

expandirPrefixos :: [Afixo] -> String -> [String]
expandirPrefixos as p = map ($ p) (concat . map expandirPrefixo as)

expandirPrefixo :: Afixo -> String -> [String]
expandirPrefixo a p = map (aplicar p) (filter (`condição` p) (regras a))
    where aplicar p regra = textoAInserir regra ++ remover (textoARemover regra) p
          remover t = drop (length t)
