module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)
{- 
foldPPON :: (String -> b) -> (Int -> b) -> ([(String, PPON)] -> b -> b) -> PPON -> b
foldPPON casoTexto casoInt casoObjeto pp = case pp of
  TextoPP texto -> casoTexto texto
  IntPP numero -> casoInt numero
  ObjetoPP (tupla:listaTuplas) -> (casoObjeto tupla)
 where rec = foldPPON casoTexto casoInt casoObjeto
 -}
-- longitudPPON :: PPON -> Int
-- longitudPPON pp = foldPPON (const 1) (const 1) (\) pp 

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
intercalar = error "PENDIENTE: Ejercicio 7"

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
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
