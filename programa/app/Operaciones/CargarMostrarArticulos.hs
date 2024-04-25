module Operaciones.CargarMostrarArticulos(
    cargarArticulos,
    mostrarArticulos,
    splitComa,
    guardarArticulosJSON,
    cargarArticulosDesdeJSON,
    agregarArticulos,
    findArticulo
) where

import System.IO
import Text.Printf
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Datas.Data
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

{-
Entradas: Una direccion del archivo.
Salida: Los articulos cargados desde el archivo
-}
cargarArticulos :: FilePath -> IO [Articulo]
cargarArticulos nombre = do
    contenido <- readFile ("app\\Operaciones\\Archivos\\" ++ nombre)
    let lineas = lines contenido
    return $ map parseArticulo lineas

{-
Entradas: La linea en forma de csv.
Salidas: El objeto articulo.
-}
parseArticulo :: String -> Articulo
parseArticulo linea =
    let [cod, nom, cst, tip, tiv] = splitComa linea
        costo' = read cst :: Double
        tipo' = read tip :: Tipo
        tipoIVA' = read tiv :: TipoIVA
    in Articulo cod nom costo' tipo' tipoIVA'

splitComa :: String -> [String]
splitComa "" = [""]
splitComa (',':xs) = "" : splitComa xs
splitComa (x:xs) = let (y:ys) = splitComa xs in (x:y) : ys

mostrarArticulo :: Articulo -> IO ()
mostrarArticulo articulo = do
    putStrLn "==============================="
    printf "Código: %s\n" (codigoArticulo articulo)
    printf "Nombre: %s\n" (nombreArticulo articulo)
    printf "Costo: %.2f\n" (costoArticulo articulo)
    printf "Tipo: %s\n" (show $ tipoArticulo articulo)
    printf "Tipo de IVA: %s\n" (show $ tipoIVAArticulo articulo)
    putStrLn "==============================="

{-
Entradas: La lista de articulos.
Salidas: La impresion de todos los articulos.
-}
mostrarArticulos :: [Articulo] -> IO ()
mostrarArticulos = mapM_ mostrarArticulo

{-
Entradas: La lista de articulos a guardar en el JSON.
Salidas: La impresiond el mensaje de exito.
-}
guardarArticulosJSON :: [Articulo] -> IO ()
guardarArticulosJSON articulos = do
    let json = encode articulos
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Articulos.json"
    B.writeFile direccion json
    putStrLn "\nSe ha guardado los articulos"

{-
Entradas: Nada
Salidas: La lista de articulos cargados desde el JSON
-}
cargarArticulosDesdeJSON :: IO [Articulo]
cargarArticulosDesdeJSON  = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Articulos.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right articulos -> return articulos

{-
Entradas: La lista de articulos a agregar al JSON.
Salidas: La impresion del mensaje de exito.
-}
agregarArticulos :: [Articulo] -> IO ()
agregarArticulos nuevosArticulos = do
    articulosExistentes <- cargarArticulosDesdeJSON
    let articulosActualizados = articulosExistentes ++ nuevosArticulos
    guardarArticulosJSON articulosActualizados

{-
Entradas: El codigo del articulo. La lista de articulos.
Salidas: El articulo que se encontro.
-}
findArticulo :: String -> [Articulo] -> Maybe Articulo
findArticulo codigo [] = Nothing
findArticulo codigo (articulo:resto)
    | codigoArticulo articulo == codigo = Just articulo
    | otherwise = findArticulo codigo resto