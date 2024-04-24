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
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Datas.Data
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

cargarArticulos :: FilePath -> IO [Articulo]
cargarArticulos nombre = do
    contenido <- readFile ("app\\Operaciones\\Archivos\\" ++ nombre)
    let lineas = lines contenido
    return $ map parseArticulo lineas

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

mostrarArticulos :: [Articulo] -> IO ()
mostrarArticulos articulos = mapM_ print articulos

guardarArticulosJSON :: [Articulo] -> IO ()
guardarArticulosJSON articulos = do
    let json = encode articulos
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Articulos.json"
    B.writeFile direccion json
    putStrLn "\nSe ha guardado los articulos"

cargarArticulosDesdeJSON :: IO [Articulo]
cargarArticulosDesdeJSON  = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Articulos.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right articulos -> return articulos

agregarArticulos :: [Articulo] -> IO ()
agregarArticulos nuevosArticulos = do
    articulosExistentes <- cargarArticulosDesdeJSON
    let articulosActualizados = articulosExistentes ++ nuevosArticulos
    guardarArticulosJSON articulosActualizados

findArticulo :: String -> [Articulo] -> Maybe Articulo
findArticulo codigo [] = Nothing
findArticulo codigo (articulo:resto)
    | codigoArticulo articulo == codigo = Just articulo
    | otherwise = findArticulo codigo resto