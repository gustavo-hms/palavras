module Palavras where

data Tipo = Prefixo | Sufixo deriving (Eq, Show)

data Afixo = Afixo {
        tipo :: Tipo,
        símbolo :: Char,
        permiteCruzamentos :: Bool,
        quantidade :: Int,
        regras :: [Regra]
    }

data Regra = Regra {
        tipoDoAfixo :: Tipo,
        símboloDoAfixo :: Char,
        letrasARemover :: Int,
        textoARemover :: String,
        condição :: String -> Bool
    }

gerarTipo :: String -> Maybe Tipo
gerarTipo "PFX" = Just Prefixo
gerarTipo "SFX" = Just Sufixo
gerarTipo _     = Nothing

criarAfixo :: [String] -> Maybe Afixo
criarAfixo (t:símb:cruzamentos:qtd:[]) = do
    t' <- gerarTipo t
    return $ Afixo t' (head símb) (podeCruzar cruzamentos) (read qtd) []
criarAfixo _ = Nothing

podeCruzar :: String -> Bool
podeCruzar "Y" = True
podeCruzar _   = False

inserirRegra :: Regra -> Afixo -> Maybe Afixo
inserirRegra r a
    | compatíveis r a = Just $ Afixo t s c qtd (r:rs)
    | otherwise       = Nothing
    where compatíveis regra afixo = tipoDoAfixo regra == tipo afixo && símboloDoAfixo regra == símbolo afixo 
          t = tipo a
          s = símbolo a
          c = permiteCruzamentos a
          qtd = quantidade a
          rs = regras a 

criarRegra :: [String] -> Maybe Regra
criarRegra (t:símb:aRemover:trechoARemover:contexto:[]) = do
    t' <- gerarTipo t
    return $ Regra t' (head símb) (read aRemover) trechoARemover (criarCondição t' contexto)
criarRegra _ = Nothing

criarCondição :: Tipo -> String -> String -> Bool
criarCondição t símbolos = condiçãoAPartirDeGrupos t $ snd (foldr agrupar (False, []) símbolos)

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
