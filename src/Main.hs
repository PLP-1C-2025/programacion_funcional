module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [
      "Ejercicio 1" ~: testsEj1,
      "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 5" ~: testsEj5,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj1 :: Test
testsEj1 =
  let
    doc1 = texto "Testing" 
    doc2 = linea <+> texto "x"
    doc3 = texto "Testing 1" <+> texto "Testing 2" <+> linea <+> texto "Testing 3"
    docVacio = vacio

    contarLineas d = foldDoc 0 (\_ rec -> rec) (\_ rec -> 1 + rec) d
    concatenarTextos d = foldDoc "" (\t rec -> t ++ rec) (\_ rec -> rec) d
    contarEspaciosLineas d = foldDoc 0 (\_ rec -> rec) (\n rec -> n + rec) d
    longitudTextos d = foldDoc 0 (\t rec -> length t + rec) (\_ rec -> rec) d

  in
    test
      [ contarLineas doc1 ~?= 0,
        contarLineas doc2 ~?= 1,
        concatenarTextos doc3 ~?= "Testing 1Testing 2Testing 3",
        contarEspaciosLineas doc2 ~?= 0,
        longitudTextos doc3 ~?= 27,
        longitudTextos docVacio ~?= 0,
        contarLineas docVacio ~?= 0
      ]


testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      -- Los siguientes tests son nuestros: 
      texto "a" <+> texto "b" <+> linea <+> texto "c" ~?= texto "ab" <+> linea <+> texto "c" ,
      linea <+> linea <+> linea ~?= linea <+> linea <+> linea
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),

      -- Los siguientes tests son nuestros: 
      indentar 2 (linea <+> linea <+> linea) ~?= indentar 2 linea <+> indentar 2 linea <+> indentar 2 linea,
      indentar 1 (texto "a" <+> linea) ~?= texto "a" <+> indentar 1 linea
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      -- Estos son nuestros:
      mostrar (texto "a" <+> indentar 2 (linea <+> texto "b" <+> linea <+> texto "c")) ~?= "a\n  b\n  c",
      mostrar (indentar 3 (linea <+> linea <+> linea)) ~?= "\n   \n   \n   "
    ]
-- 
pericles, merlina, addams, familias, ian, rama, thiago, santy :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

ian = ObjetoPP [("Nombre", TextoPP "Ian"), ("Apellido", TextoPP "Pipo"), ("Flia", pericles)]
rama = TextoPP "hola"
thiago = IntPP 14
santy = ObjetoPP [("amalgama 1", ian), ("amalgama 2", rama), ("amalgama 3", thiago)]

testsEj5 :: Test
testsEj5 =
  test
    [
      pponAtomico pericles ~?= False,
      pponAtomico addams ~?= False,
      -- Estos son nuestros:
      pponAtomico familias ~?= False,
      pponAtomico merlina ~?= False,
      pponAtomico ian ~?= False,
      pponAtomico rama ~?= True,
      pponAtomico thiago ~?= True,
      pponAtomico santy ~?= False
    ]


testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      -- Estos son nuestros:
      pponObjetoSimple familias ~?= False,
      pponObjetoSimple merlina ~?= True,
      pponObjetoSimple ian ~?= False,
      pponObjetoSimple rama ~?= False,  -- corregido, dado que tomabamos que un atomico era un simple
      pponObjetoSimple thiago ~?= False,
      pponObjetoSimple santy ~?= False

    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"


testsEj7 :: Test
testsEj7 =
  test
    [
      mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      -- Estos son nuestros:
      mostrar (intercalar (texto "?") [texto "hola", texto "como"]) ~?= "hola?como",
      mostrar (intercalar (texto " ") [texto "hola", texto "como"]) ~?= "hola como",
      mostrar (intercalar (texto "") [texto "hola", texto "como"]) ~?= "holacomo",
      mostrar (intercalar (texto "?-._.{}") [texto "hola", texto "como"]) ~?= "hola?-._.{}como"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar vacio) ~?= "",
      -- Nuestros:
      mostrar (aplanar linea <+> linea <+> a) ~?= "  a"
    ]


testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      -- Nuestros:
      mostrar (pponADoc ian) ~?= "{\n  \"Nombre\": \"Ian\",\n  \"Apellido\": \"Pipo\",\n  \"Flia\": { \"nombre\": \"Pericles\", \"edad\": 30 }\n}"
    ]
