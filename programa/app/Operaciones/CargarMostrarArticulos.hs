module Operaciones.CargarMostrarArticulos(
    cargarArticulos,
    mostrarArticulos,
    splitComa
) where
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Datas.Data

cargarArticulos :: FilePath -> IO [Articulo]
cargarArticulos nombre = do
    contenido <- readFile ("./Archivos/" ++ nombre)
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

guardarArticulosJSON :: FilePath -> [Articulo] -> IO ()
guardarArticulosJSON fileName articulos = do
    let json = encode articulos
    B.appendFile fileName (B.pack "\n")
    B.appendFile fileName json

cargarArticulosDesdeJSON :: FilePath -> IO [Articulo]
cargarArticulosDesdeJSON fileName = do
    json <- B.readFile fileName
    case eitherDecode json of
        Left err -> error $ "Error al decodificar el JSON: " ++ err
        Right articulos -> return articulos