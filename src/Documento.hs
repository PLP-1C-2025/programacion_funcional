module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

-- msj = Doc (Texto "hola" Linea 0 Texto "mi nombre es" Linea 0 Texto "Don Pollo" Vacio)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio


foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc casoVacio casoTexto casoLinea doc = case doc of
    Vacio -> casoVacio
    Texto t documentoRestante -> casoTexto t (rec documentoRestante)
    Linea cantEspacios documentoRestante -> casoLinea cantEspacios (rec documentoRestante)
  where rec = foldDoc casoVacio casoTexto casoLinea

-- Este estaba para debuggear foldDoc
-- contarLineas :: Doc -> Int
-- contarLineas Vacio = 0
-- contarLineas d = 1 + foldDoc 0 (\_ rec -> rec) (\_ rec -> 1 + rec) d

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

-- s no debe ser el string vac´ıo.
-- s no debe contener saltos de l´ınea
-- d debe ser Vacio o Linea i d’
-- Esta funcion recorre el d1, si se encuentra un texto se asegura de unir correctamente el t1 de la recursion con el texto de d1 encontrado

-- Va hasta el final, encuentra Vacio y la reemplaza por d2, no hay vacio entre los textos por esto. Si d1 y d2 cumplen a su vez el inv entonces estaría bien
(<+>) :: Doc -> Doc -> Doc
doc1 <+> doc2 = foldDoc doc2 (\t rec -> case rec of
                                      -- En caso de que el final del documento sea Texto y el inicio del siguiente tambien, sus textos se concatenan en un solo Texto
                                      Texto textoRestante documentoRestante -> Texto (t ++ textoRestante) documentoRestante
                                      _ -> Texto t rec) Linea doc1


-- Por la especificacion de indentar, i >= 0. Por lo tanto el invariante se cumple para toda entrada valida.
indentar :: Int -> Doc -> Doc
-- Sobre texto no operamos, por lo que no es posible que se rompa el invariante.
indentar i = foldDoc Vacio Texto (\espacios rec -> Linea (espacios+i) rec)

mostrar :: Doc -> String
mostrar = foldDoc "" (\t rec -> t ++ rec) (\espacios rec -> "\n" ++ take espacios (repeat ' ') ++ rec)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
