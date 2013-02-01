module Palavras where

import Afixos
import qualified Data.Map.Lazy as M
import Data.List (partition)

data Palavra = Palavra {
        radical  :: String,
        prefixos :: [Afixo],
        sufixos  :: [Afixo]
    }

criarPalavra :: String -> M.Map Char Afixo -> Palavra
criarPalavra p m
    | null símbs = Palavra txt [] []
    | otherwise  = Palavra txt pref suf
    where (txt, símbs) = break (== '/') p
          (pref, suf)  = partition prefixo (map (m M.!) (tail símbs))

obterAfixos :: M.Map Char Afixo -> [Char] -> (Afixo, Afixo)
obterAfixos m símbs = partition prefixo (map (extrairAfixo m) símbs)

extrairAfixo :: M.Map Char Afixo -> Char -> Afixo
extrairAfixo m símb = preencherContinuação m (m M.! símb)

preencherContinuação :: M.Map Char Afixo -> Afixo -> Afixo
preencherContinuação a@(Afixo t s p q rs) =
    case t of
         Prefixo -> a
         Sufixo  -> Afixo t s p q (map (inserirContinuações m) rs)

expandir :: Palavra -> [String]
expandir p = raiz p : comSfxSemCruz ++ comSfxCruz ++ comCruz ++ comPfx
    where comSfxSemCruz = concat [aplicar sfx (raiz p) | sfx <- sufixos p, not (podeCruzar sfx)]
          comSfxCruz    = concat [aplicar sfx (raiz p) | sfx <- sufixos p, podeCruzar sfx]
          comCruz       = concat [aplicar pfx x | pfx <- prefixos p, podeCruzar pfx, x <- comSfxCruz]
          comPfx        = concat [aplicar pfx (raiz p) | pfx <- prefixos p]
