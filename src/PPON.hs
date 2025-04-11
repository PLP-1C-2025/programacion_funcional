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
  TextoPP _ -> True -- Preguntar
  IntPP _ -> True -- Preguntar 
  ObjetoPP xs -> foldr (\(_, ppon) -> (&&) (pponAtomico ppon)) True xs

intercalar :: Doc -> [Doc] -> Doc
intercalar docIntercalado = foldr (\doc rec -> if rec /= vacio then doc <+> docIntercalado <+> rec else doc <+> rec ) vacio

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

-- pponAdoc cumple con el esquema Estructural, pues: su caso base no es recursivo y no tiene acceso a la "cola" (en este caso, el segundo elemento de la tupla)  
-- del Doc de ninguna forma que no sea mediante la llamada recursiva pponADoc
pponADoc :: PPON -> Doc
pponADoc pp = case pp of
  TextoPP tex  -> texto (show tex)
  IntPP   num   -> texto (show num)
  ObjetoPP pps  -> if pponObjetoSimple (ObjetoPP pps) then
                          aplanar rec
                    else rec
    where rec = entreLlaves (map (\tupla -> texto (show (fst tupla)) <+> texto ": " <+> pponADoc (snd tupla)) pps)