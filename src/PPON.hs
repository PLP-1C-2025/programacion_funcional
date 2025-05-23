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
y además no es primitivo ni estructural, ya que operamos sobre una lista de tipo (String, PPON) y no sobre un solo elemento.
-} 

pponADoc :: PPON -> Doc
pponADoc pp = case pp of
  TextoPP tex  -> texto (show tex)
  IntPP   num   -> texto (show num)
  ObjetoPP pps  -> if pponObjetoSimple (ObjetoPP pps) then
                          aplanar rec
                    else rec
    where rec = entreLlaves (map (\tupla -> texto (show (fst tupla)) <+> texto ": " <+> pponADoc (snd tupla)) pps)