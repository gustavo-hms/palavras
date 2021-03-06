module Main where

import Afixo
import Data.List (isPrefixOf)
import qualified Data.Map.Lazy as M
import Palavra
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Regex.TDFA

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
       then do
           nome <- getProgName
           hPutStrLn stderr $ "Uso: " ++ nome ++ " <expressão>"
       else do
           let (arquivoAff, arquivoDic) = dicionário

           aff <- openFile arquivoAff ReadMode
           dic <- openFile arquivoDic ReadMode
           codificação <- lerCodificação aff
           hSetEncoding aff codificação
           hSetEncoding dic codificação

           linhasAff <- hGetContents aff
           let la = filtrarLinhas $ lines linhasAff
               m  = gerarTabelaDeAfixos la M.empty

           linhasDic <- hGetContents dic
           let exprFinal = formatarExpressão $ head args
           mapM_ putStr $ map (unlines . (map head) . (=~ exprFinal)) $
               gerarPalavras (tail $ lines linhasDic) m
           hClose aff
           hClose dic

dicionário :: (String, String)
dicionário = (base ++ ".aff", base ++ ".dic")
    where caminho = "/usr/share/hunspell/"
          língua  = "pt_BR"
          base    = caminho ++ língua

lerCodificação :: Handle -> IO TextEncoding
lerCodificação _ = return latin1

filtrarLinhas :: [String] -> [String]
filtrarLinhas ls = [l | l <- ls, pfx <- ["PFX", "SFX"], pfx `isPrefixOf` l]

formatarExpressão :: String -> String
formatarExpressão e = início ++ exprFinal ++ "$"
    where início    = if head e /= '^' then "^.*" else ""
          exprFinal = inserirConsoantes . inserirVogais $ e

inserirConsoantes :: String -> String
inserirConsoantes "" = ""
inserirConsoantes s  = começo ++ consoantes ++ inserirConsoantes final
    where (começo, resto) = break (== '#') s
          final           = if length resto > 1 then tail resto else ""
          consoantes      = if length resto > 1 
                               then "[^" ++ vogais ++ "]"
                               else ""

inserirVogais :: String -> String
inserirVogais "" = ""
inserirVogais s  = começo ++ vogais' ++ inserirVogais final
    where (começo, resto) = break (== '@') s
          final           = if length resto > 1 then tail resto else ""
          vogais'         = if length resto > 1 
                               then "[" ++ vogais ++ "]"
                               else ""

vogais :: String
vogais = "aáàãâeéèẽêiíìĩîoóòõôuúùũûy"

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
    map (unlines . expandir . (`criarPalavra` m)) linhas
