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

-- {"clave_1": valor_1, "clave_2": valor_2, ...}
pponADoc :: PPON -> Doc
pponADoc pp = case pp of
  TextoPP a   -> texto (show a)
  IntPP   a   -> texto (show a)
  ObjetoPP pps  -> if pponObjetoSimple (ObjetoPP pps) then
                          aplanar (entreLlaves (map (\x -> texto (fst x) <+> texto ": " <+> pponADoc (snd x)) pps))
                    else texto ""
{-
 pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
 merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24), ("alturo", IntPP 2)]
 addams = ObjetoPP [("0", pericles), ("1", merlina)]
 
 {
 "0": { "nombre": "Pericles", "edad": 30, "altura": 2 },
 "1": { "nombre": "Merlina", "edad": 24 }
 } 
-}