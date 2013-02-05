module Main where

import Afixos
import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Palavras
import System.Environment (getArgs, getProgName)
import System.IO 

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
       then do
            nome <- getProgName
            hPutStrLn stderr $ "Uso: " ++ nome ++ " <arquivo dic> <arquivo aff>"
       else do
            aff <- openFile (args !! 1) ReadMode  
            hSetEncoding aff latin1
            linhasAff <- T.hGetContents aff  
            let m = gerarTabelaDeAfixos (T.lines linhasAff) M.empty

            dic <- openFile (head args) ReadMode  
            hSetEncoding dic latin1
            linhasDic <- T.hGetContents dic  

            T.putStr $ T.unlines (gerarPalavras (tail $ T.lines linhasDic) m)
            hClose aff
            hClose dic

inserirAfixo :: Afixo -> M.Map Símbolo Afixo -> M.Map Símbolo Afixo
inserirAfixo a = M.insert (símbolo a) a

filtrarLinhas :: [T.Text] -> [T.Text]
filtrarLinhas ls =
    [l | l <- ls, pfx <- ["PFX", "SFX"], T.pack pfx `T.isPrefixOf` l]

gerarTabelaDeAfixos :: [T.Text] -> M.Map Símbolo Afixo -> M.Map Símbolo Afixo
gerarTabelaDeAfixos []     m = m
gerarTabelaDeAfixos linhas m =
    let (ma, linhas') = montarAfixo linhas
        m' = case ma of Nothing -> m
                        Just a  -> M.insert (símbolo a) a m
    in gerarTabelaDeAfixos linhas' m'

montarAfixo :: [T.Text] -> (Maybe Afixo, [T.Text])
montarAfixo []         = (Nothing, [])
montarAfixo (l:linhas) =
    let a  = criarAfixo (T.words l)
        nº = qtd a
        rs = map (criarRegra . T.words) (take nº linhas)
        a' = foldr mInserirRegra a rs
    in (a', drop nº linhas)
    where qtd Nothing   = 0
          qtd (Just a') = quantidade a'

mInserirRegra :: Maybe Regra -> Maybe Afixo -> Maybe Afixo
mInserirRegra mr ma = do
    r <- mr
    a <- ma
    inserirRegra r a

gerarPalavras :: [T.Text] -> M.Map Símbolo Afixo -> [T.Text]
gerarPalavras linhas m =
    foldr (\p ps -> expandir (criarPalavra p m) ++ ps) [] linhas
