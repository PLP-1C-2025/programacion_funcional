module PPON where

import Documento


data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico pp = case pp of
  TextoPP _ -> True
  IntPP _ -> True
  ObjetoPP _ -> False

-- Interpretamos que si le pasas un pponAtomico es ademas un ObjetoSimple
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple pp = case pp of
  ObjetoPP xs -> all (\(_, ppon) -> pponAtomico ppon) xs 
  _ -> False
  --ObjetoPP xs -> foldr (\(_, ppon) -> (&&) (pponAtomico ppon)) True xs

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar docIntercalado xs = foldr1 (\doc rec -> doc <+> docIntercalado <+> rec) xs

--intercalar :: Doc -> [Doc] -> Doc
--intercalar docIntercalado = foldr (\doc rec -> if rec /= vacio then doc <+> docIntercalado <+> rec else doc) vacio

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\t1 rec -> texto t1 <+> rec) (\_ rec -> texto " " <+> rec)

{-
pponAdoc cumple con el esquema global, ya que no posee contadores ni acumuladores (esquema iterativo),
y además es primitivo y estructural, ya que operamos sobre una lista de tipo (String, PPON) y no sobre un solo elemento, por lo que.

No es un esquema iterativo porque no posee contadores ni acumuladores
No alcanza un esquema estructural porque operamos sobre el resto del PPON en: pponADoc (snd tupla)
No alcanza un esquema primitivo porque usamos todas las llamadas recursivas anteriores al hacer 
map (\(elemTexto, elemPPON) -> texto (show elemTexto) <+> texto ": " <+> pponADoc elemPPON) pps


Recordemos la definición de recursión primitiva. 
Utiliza un caso base fijo y un caso recursivo. En el caso recursivo, puede utilizar xs, x y (g xs).

Utilizamos nuestra recursión en la lambda del map sobre todos los objetos "hijos" del objeto pps. Esta recursión se aplica una sola vez sobre
estos objetos. Esta recursión en sí es estructural ya que no estamos utilizando al objeto padre en sí (ObjetoPP pps), sino que utilizaríamos pps y 
sus hijos.
Luego, en la guarda del if, notamos que rompemos esta recursión estructural haciendola primitiva, ya que pasamos a usar (ObjetoPP pps) por afuera de 
alguna recursión.


El tipo de recursion es primitiva ya que la recursion en rec es estructural en pps. Pero en la guarda del if estoy manipulando 
el pps más que para solo la recursion, por lo que estaría rompiendo la estructural, pasando a la recursion primitiva
-} 

pponADoc :: PPON -> Doc
pponADoc pp = case pp of
  TextoPP   tex   ->  texto (show tex)
  IntPP     num   ->  texto (show num)
  ObjetoPP  pps   ->  if pponObjetoSimple (ObjetoPP pps) then
                        aplanar rec
                      else rec
    where rec = entreLlaves (map (\(elemTexto, elemPPON) -> texto (show elemTexto) <+> texto ": " <+> pponADoc elemPPON) pps)