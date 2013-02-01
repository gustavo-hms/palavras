-- TODO CIMFUMFIX
-- TODO estudar de habilitar a "FLAG long" e a "FLAG num"
-- TODO implementar FORBIDDENWORD
module Afixos where

import qualified Data.Map.Lazy as M

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
        inserir         :: String,
        condição        :: String -> Bool,
        símbContinuação :: [Símbolo],
        continuação     :: [Afixo]
    }

gerarTipo :: String -> Maybe Tipo
gerarTipo "PFX" = Just Prefixo
gerarTipo "SFX" = Just Sufixo
gerarTipo _     = Nothing

prefixo :: Afixo -> Bool
prefixo a = tipo a == Prefixo

sufixo :: Afixo -> Bool
sufixo a = tipo a == Sufixo

criarAfixo :: [String] -> Maybe Afixo
criarAfixo (t:símb:cruzamentos:qtd:[]) = do
    t' <- gerarTipo t
    return $ Afixo t' (head símb) (aceitaCruzamentos cruzamentos) (read qtd) []
criarAfixo _ = Nothing

aceitaCruzamentos :: String -> Bool
aceitaCruzamentos "Y" = True
aceitaCruzamentos _   = False

inserirRegra :: Regra -> Afixo -> Maybe Afixo
inserirRegra r a
    | compatíveis r a = Just $ Afixo t s c qtd (r:rs)
    | otherwise       = Nothing
    where compatíveis regra afixo = tipoDoAfixo regra == tipo afixo
              && símboloDoAfixo regra == símbolo afixo 
          t = tipo a
          s = símbolo a
          c = podeCruzar a
          qtd = quantidade a
          rs = regras a 

criarRegra :: [String] -> Maybe Regra
criarRegra (t:s:aRemover:aInserir:contexto:_) = do
    t' <- gerarTipo t
    return $ Regra t' (head s) (length $ uniformizar aRemover) (uniformizar aInserir) (criarCondição t' cond) cont []
    where uniformizar "0" = ""
          uniformizar p   = p

          (cond, resto) = break (== '/') contexto
          cont = if null resto then "" else tail resto
criarRegra _ = Nothing

criarCondição :: Tipo -> String -> String -> Bool
criarCondição t símbolos = condiçãoAPartirDeGrupos t $ snd (foldr agrupar (False, []) símbolos)

-- Agrupa entradas entre colchetes.
-- Ex: snd $ foldr agrupar (False, []) "[^a]xy[bcdef]" ==
--   ["^a", "x", "y","bcdef"]
agrupar :: Char -> (Bool, [String]) -> (Bool, [String])
agrupar c (grupoAberto, acumulado)
    | c == ']'    = (True, []:acumulado)
    | c == '['    = (False, acumulado)
    | grupoAberto = (True, (c:a):as)
    | otherwise   = (False, [c]:acumulado)
    where a = head acumulado
          as = tail acumulado

condiçãoAPartirDeGrupos :: Tipo -> [String] -> String -> Bool
condiçãoAPartirDeGrupos t grupos palavra
    | tamanhoDaPalavra < númeroDeGrupos = False
    | t == Prefixo                      = and $ zipWith ($) predicados palavra
    | otherwise                         = and $ zipWith ($) predicados finalDaPalavra 
    where tamanhoDaPalavra = length palavra
          númeroDeGrupos   = length grupos
          finalDaPalavra   = drop (tamanhoDaPalavra - númeroDeGrupos) palavra
          predicados       = map criarPredicado grupos 

criarPredicado :: String -> Char -> Bool
criarPredicado "." _                 = True
criarPredicado (c:[]) letra          = c == letra
criarPredicado ('^':elementos) letra = letra `notElem` elementos
criarPredicado elementos letra       = letra `elem` elementos

aplicar :: Afixo -> String -> [String]
aplicar afx termo = map (executarRegra termo) (filter (`condição` termo) (regras afx))

executarRegra :: String -> Regra -> String
executarRegra termo r =
    case tipoDoAfixo r of
         Prefixo -> inserir r ++ drop (remover r) termo
         Sufixo  -> take (length termo - remover r) termo ++ inserir r

preencherContinuação :: M.Map Símbolo Afixo -> Afixo -> Afixo
preencherContinuação m a@(Afixo t s p q rs) =
    case t of
         Prefixo -> a
         Sufixo  -> Afixo t s p q (map (inserirContinuações m) rs)

inserirContinuações :: M.Map Char Afixo -> Regra -> Regra
inserirContinuações m (Regra ta sa re i c ss _) = Regra ta sa re i c ss cs
    where cs = map (m M.!) ss

