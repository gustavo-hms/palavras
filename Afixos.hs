-- TODO CIMFUMFIX
-- TODO estudar de habilitar a "FLAG long" e a "FLAG num"
-- TODO implementar FORBIDDENWORD
module Afixos (
        Tipo,
        Afixo(..),
        Regra(..),
        Símbolo,
        prefixo,
        sufixo,
        criarRegra,
        inserirRegra,
        criarAfixo,
        aplicar,
        preencherContinuação
    ) where

import qualified Data.Map.Lazy as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import qualified Data.Text.Lazy as T

data Tipo = Prefixo | Sufixo deriving (Eq, Show)

type Símbolo = Char

data Afixo = Afixo {
        tipo       :: Tipo,
        símbolo    :: Símbolo,
        podeCruzar :: Bool,
        quantidade :: Int,
        regras     :: [Regra]
    }

data Regra = Regra {
        tipoDoAfixo     :: Tipo,
        símboloDoAfixo  :: Símbolo,
        remover         :: Int,
        inserir         :: T.Text,
        condição        :: T.Text -> Bool,
        símbContinuação :: [Símbolo],
        continuação     :: [Afixo]
    }

gerarTipo :: T.Text -> Maybe Tipo
gerarTipo t
    | t == T.pack "PFX" = Just Prefixo
    | t == T.pack "SFX" = Just Sufixo
    | otherwise         = Nothing

prefixo :: Afixo -> Bool
prefixo a = tipo a == Prefixo

sufixo :: Afixo -> Bool
sufixo a = tipo a == Sufixo

criarAfixo :: [T.Text] -> Maybe Afixo
criarAfixo (t:símb:cruzamentos:qtd:[]) = do
    t' <- gerarTipo t
    return $ Afixo t' (T.head símb) (aceitaCruzamentos cruzamentos)
        (read $ T.unpack qtd) []
criarAfixo _ = Nothing

aceitaCruzamentos :: T.Text -> Bool
aceitaCruzamentos y
    | y == T.pack "Y"  = True
    | otherwise        = False

inserirRegra :: Regra -> Afixo -> Maybe Afixo
inserirRegra r a
    | compatíveis r a = Just $ Afixo t s c qtd (r:rs)
    | otherwise       = Nothing
    where compatíveis regra afixo = tipoDoAfixo regra == tipo afixo
              && símboloDoAfixo regra == símbolo afixo 
          t   = tipo a
          s   = símbolo a
          c   = podeCruzar a
          qtd = quantidade a
          rs  = regras a 

criarRegra :: [T.Text] -> Maybe Regra
criarRegra (t:s:aRemover:aInserir:contexto:_) = do
    t' <- gerarTipo t
    return $ Regra t' (T.head s) (fromIntegral . T.length $ uniformizar aRemover)
        (uniformizar aInserir) (criarCondição t' cond) cont []
    where uniformizar p
              | p == T.pack "0" = T.pack ""
              | otherwise       = p
          (cond, resto) = T.breakOn (T.pack "/") contexto
          cont          = if T.null resto then "" else T.unpack $ T.tail resto
criarRegra _ = Nothing

criarCondição :: Tipo -> T.Text -> T.Text -> Bool
criarCondição t símbolos = condiçãoAPartirDeGrupos t $
    snd (T.foldr agrupar (False, []) símbolos)

-- Agrupa entradas entre colchetes.
-- Ex: snd $ foldr agrupar (False, []) "[^a]xy[bcdef]" ==
--   ["^a", "x", "y","bcdef"]
agrupar :: Char -> (Bool, [T.Text]) -> (Bool, [T.Text])
agrupar c (grupoAberto, acumulado)
    | c == ']'    = (True, T.empty:acumulado)
    | c == '['    = (False, acumulado)
    | grupoAberto = (True, (c `T.cons` a):as)
    | otherwise   = (False, T.singleton c:acumulado)
    where a  = head acumulado
          as = tail acumulado

condiçãoAPartirDeGrupos :: Tipo -> [T.Text] -> T.Text -> Bool
condiçãoAPartirDeGrupos t grupos palavra
    | tamanhoDaPalavra < nºDeGrupos = False
    | t == Prefixo = fst $ T.foldl validar (True, predicados) inícioDaPalavra
    | otherwise    = fst $ T.foldl validar (True, predicados) finalDaPalavra
    where tamanhoDaPalavra = T.length palavra
          nºDeGrupos       = fromIntegral $ length grupos
          finalDaPalavra   = T.take (tamanhoDaPalavra - nºDeGrupos) palavra
          inícioDaPalavra  = T.drop nºDeGrupos palavra
          predicados       = map criarPredicado grupos 

validar :: (Bool, [Char -> Bool]) -> Char -> (Bool, [Char -> Bool])
validar (b,     []) _ = (b, [])
validar (False, ps) _ = (False, ps)
validar (_,     ps) c = (head ps c, tail ps)

criarPredicado :: T.Text -> Char -> Bool
criarPredicado p c
    | p == T.pack "." = True
    | T.length p == 1 = p == T.singleton c
    | T.head p == '^' = isNothing $ T.find (== c) p
    | otherwise       = isJust $ T.find (== c) p

aplicar :: Afixo -> T.Text -> [T.Text]
aplicar afx termo =
    map (executarRegra termo) (filter (`condição` termo) (regras afx))

executarRegra :: T.Text -> Regra -> T.Text
executarRegra termo r =
    case tipoDoAfixo r of
         Prefixo -> inserir r `T.append` T.drop (fromIntegral (remover r)) termo
         Sufixo  -> T.take (T.length termo - fromIntegral (remover r)) termo `T.append` inserir r

preencherContinuação :: M.Map Símbolo Afixo -> Afixo -> Afixo
preencherContinuação m a@(Afixo t s p q rs) =
    case t of
         Prefixo -> a
         Sufixo  -> Afixo t s p q (map (inserirContinuações m) rs)

inserirContinuações :: M.Map Símbolo Afixo -> Regra -> Regra
inserirContinuações m (Regra ta sa re i c ss _) = Regra ta sa re i c ss cs
    where cs = mapMaybe (`M.lookup` m) ss

