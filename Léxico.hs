module Main where

import Afixos
--import Palavras
import qualified Data.Map.Lazy as M

main :: IO ()
main = undefined

inserirAfixo :: Afixo -> M.Map Símbolo Afixo -> M.Map Símbolo Afixo
inserirAfixo a = M.insert (símbolo a) a

montarAfixo :: [String] -> (Maybe Afixo, [String])
montarAfixo []       = (Nothing, [])
montarAfixo (l:linhas) =
    let a  = criarAfixo (words l)
        nº = qtd a
        rs = map (criarRegra . words) (take nº linhas)
        a' = foldr mInserirRegra a rs
    in (a', drop nº linhas)
    where qtd Nothing   = 0
          qtd (Just a') = quantidade a'

mInserirRegra :: Maybe Regra -> Maybe Afixo -> Maybe Afixo
mInserirRegra mr ma = do
    r <- mr
    a <- ma
    inserirRegra r a
