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
    return $ Regra t' (head símb) (read aRemover) trechoARemover (criarCondição contexto)
criarRegra _ = Nothing

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
-- TODO a condição só é aplicada ao final da palavra para sufixos; para 
-- prefixos, ela é aplicada ao início da palavra; corrigir isso!

predicado :: String -> Char -> Bool
predicado "." letra             = True
predicado (c:[]) letra          = c == letra
predicado ('^':elementos) letra = letra `notElem` elementos
predicado elementos letra       = letra `elem` elementos
