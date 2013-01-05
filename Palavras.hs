module Palavras where

data Tipo = Prefixo | Sufixo deriving Show

data Afixo = Afixo {
        tipo :: Tipo,
        símbolo :: Char,
        permiteCruzamentos :: Bool,
        quantidade :: Int,
        regras :: [Regra]
    }

data Regra = Regra {
        símbolosARemover :: Int,
        texto :: String,
        condição :: String -> Bool
    }

criarAfixo :: [String] -> Maybe Afixo
criarAfixo ("PFX":símb:cruzamentos:qtd:[]) =
    Just $ Afixo Prefixo (head símb) (podeCruzar cruzamentos) (read qtd) []
criarAfixo ("SFX":símb:cruzamentos:qtd:[]) =
    Just $ Afixo Sufixo (head símb) (podeCruzar cruzamentos) (read qtd) []
criarAfixo _ = Nothing

podeCruzar :: String -> Bool
podeCruzar "Y" = True
podeCruzar _   = False

inserirRegra :: Regra -> Afixo -> Afixo
inserirRegra r afixo = Afixo t s c qtd (r:rs)
    where t = tipo afixo
          s = símbolo afixo
          c = permiteCruzamentos afixo
          qtd = quantidade afixo
          rs = regras afixo

criarRegra :: [String] -> Regra
criarRegra (aRemover:trechoARemover:contexto:[]) =
    Regra (read aRemover) trechoARemover (criarCondição contexto)

criarCondição :: String -> String -> Bool
criarCondição símbolos = condiçãoAPartirDeGrupos $ snd (foldr agrupar (False, []) símbolos)

-- Agrupa entradas entre colchetes.
-- Ex: snd $ foldr agrupar (False, []) "[^a]xy[bcdef]" == ["^a", "x", 
-- "y","bcdef"]
agrupar :: Char -> (Bool, [String]) -> (Bool, [String])
agrupar c (grupoAberto, acumulado)
    | c == ']'  = (True, []:acumulado)
    | c == '['  = (False, acumulado)
    | otherwise = if grupoAberto
                     then (True, (c:a):as)
                     else (False, [c]:acumulado)
                  where a = head acumulado
                        as = tail acumulado

condiçãoAPartirDeGrupos :: [String] -> String -> Bool
condiçãoAPartirDeGrupos grupos palavra
    | tamanhoDaPalavra < númeroDeGrupos = False
    | otherwise                         = and $ zipWith ($) predicados finalDaPalavra 
    where tamanhoDaPalavra = length palavra
          númeroDeGrupos   = length grupos
          finalDaPalavra   = drop (tamanhoDaPalavra - númeroDeGrupos) palavra
          predicados       = map predicado grupos 

predicado :: String -> Char -> Bool
predicado "." letra             = True
predicado (c:[]) letra          = c == letra
predicado ('^':elementos) letra = letra `notElem` elementos
predicado elementos letra       = letra `elem` elementos
