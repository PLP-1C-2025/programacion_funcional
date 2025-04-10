module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
    contarLineas -- SE PUEDE AGREGAR ESTA FUNCION ACA PARA PROBAR EL FOLDDOC EN UN TEST?
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
    Texto texto documentoRestante -> casoTexto texto (rec documentoRestante)
    Linea cantEspacios documentoRestante -> casoLinea cantEspacios (rec documentoRestante)
  where rec = foldDoc casoVacio casoTexto casoLinea

contarLineas :: Doc -> Int
contarLineas Vacio = 0
contarLineas d = 1 + foldDoc 0 (\_ rec -> rec) (\_ rec -> 1 + rec) d

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>


-- Sea Texto s d, s no debe ser el string vacio. s no debe contener saltos de linea. d debe ser Vacio o Linea i d’
-- Sea Linea i d, i >= 0
-- Va hasta el final, encuentra Vacio y lo reemplaza por d2, no hay vacio entre los textos por esto. Si d1 y d2 cumplen a su vez el inv entonces esta bien
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 (\texto rec -> case rec of 
                                      Vacio -> Texto texto rec
                                      Linea _ _ -> Texto texto rec
                                      -- En caso de que el final del documento sea Texto y el inicio del siguiente tambien, sus textos se concatenan en un solo Texto
                                      Texto t1 d1 -> Texto (texto ++ t1) d1) (\espacios rec -> Linea espacios rec) d1


-- Por la especificacion de indentar, i >= 0. Por lo tanto el invariante se cumple para toda entrada valida.
indentar :: Int -> Doc -> Doc
-- Sobre texto no operamos, por lo que no es posible que se rompa el invariante.
indentar i = foldDoc Vacio (\texto rec -> Texto texto rec) (\espacios rec -> Linea (espacios+i) rec)

mostrar :: Doc -> String
mostrar = foldDoc "" (\texto rec -> texto ++ rec) (\espacios rec -> "\n" ++ take espacios (repeat ' ') ++ rec)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
