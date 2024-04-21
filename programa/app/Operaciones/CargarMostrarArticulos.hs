module Operaciones.CargarMostrarArticulos(
    cargarArticulos,
    mostrarArticulos,
    splitComa
) where
import System.IO
import Datas.Data

rutaBase :: FilePath
rutaBase = "./Archivos/"

cargarArticulos :: FilePath -> IO [Articulo]
cargarArticulos nombre = do
    contenido <- readFile (rutaBase ++ nombre)
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
