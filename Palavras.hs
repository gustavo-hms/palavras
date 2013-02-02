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
          (pref, suf)  = obterAfixos m (if not (null símbs) then tail símbs else [])

obterAfixos :: M.Map Símbolo Afixo -> [Símbolo] -> ([Afixo], [Afixo])
obterAfixos m símbs = partition prefixo (map (extrairAfixo m) símbs)

extrairAfixo :: M.Map Símbolo Afixo -> Símbolo -> Afixo
extrairAfixo m s = preencherContinuação m (m M.! s)

expandir :: Palavra -> [String]
expandir p = radical p : comSfxSemCruz ++ comSfxCruz ++ comCruz ++ comPfx
    where comSfxSemCruz = concat [aplicar sfx (radical p) | sfx <- sufixos p, not (podeCruzar sfx)]
          comSfxCruz    = concat [aplicar sfx (radical p) | sfx <- sufixos p, podeCruzar sfx]
          comCruz       = concat [aplicar pfx x | pfx <- prefixos p, podeCruzar pfx, x <- comSfxCruz]
          comPfx        = concat [aplicar pfx (radical p) | pfx <- prefixos p]
