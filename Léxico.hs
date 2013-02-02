module Main where

import Afixos
import Palavras
import qualified Data.Map.Lazy as M
import Data.List (isPrefixOf)

main :: IO ()
main = undefined

inserirAfixo :: Afixo -> M.Map Símbolo Afixo -> M.Map Símbolo Afixo
inserirAfixo a = M.insert (símbolo a) a

filtrarLinhas :: [String] -> [String]
filtrarLinhas ls = [l | l <- ls, pfx <- ["PFX", "SFX"], pfx `isPrefixOf` l]

gerarTabelaDeAfixos :: [String] -> M.Map Símbolo Afixo -> M.Map Símbolo Afixo
gerarTabelaDeAfixos []     m = m
gerarTabelaDeAfixos linhas m =
    let (ma, linhas') = montarAfixo linhas
        m' = case ma of Nothing -> m
                        Just a  -> M.insert (símbolo a) a m
    in gerarTabelaDeAfixos linhas' m'

montarAfixo :: [String] -> (Maybe Afixo, [String])
montarAfixo []         = (Nothing, [])
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

gerarPalavras :: [String] -> M.Map Char Afixo -> [String]
gerarPalavras linhas m =
    foldr (\p ps -> expandir (criarPalavra p m) ++ ps) [] linhas
