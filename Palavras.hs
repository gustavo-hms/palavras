module Palavras where

import Afixos
import Data.List (partition)
import qualified Data.Map.Lazy as M
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T

data Palavra = Palavra {
        radical  :: T.Text,
        prefixos :: [Afixo],
        sufixos  :: [Afixo]
    }

criarPalavra :: T.Text -> M.Map Símbolo Afixo -> Palavra
criarPalavra p m
    | T.null símbs = Palavra txt [] []
    | otherwise  = Palavra txt pref suf
    where (txt, símbs) = T.breakOn (T.pack "/") p
          (pref, suf)  = obterAfixos m (if not (T.null símbs)
                                           then (T.unpack . T.tail) símbs
                                           else [])

obterAfixos :: M.Map Símbolo Afixo -> [Símbolo] -> ([Afixo], [Afixo])
obterAfixos m símbs = partition prefixo (mapMaybe (extrairAfixo m) símbs)

extrairAfixo :: M.Map Símbolo Afixo -> Símbolo -> Maybe Afixo
extrairAfixo m s =
    case M.lookup s m of
         Just a  -> Just $ preencherContinuação m a
         Nothing -> Nothing

expandir :: Palavra -> [T.Text]
expandir p = radical p : comSfxSemCruz ++ comSfxCruz ++ comCruz ++ comPfx
    where comSfxSemCruz = concat [aplicar sfx (radical p) | sfx <- sufixos p, not (podeCruzar sfx)]
          comSfxCruz    = concat [aplicar sfx (radical p) | sfx <- sufixos p, podeCruzar sfx]
          comCruz       = concat [aplicar pfx x | pfx <- prefixos p, podeCruzar pfx, x <- comSfxCruz]
          comPfx        = concat [aplicar pfx (radical p) | pfx <- prefixos p]
