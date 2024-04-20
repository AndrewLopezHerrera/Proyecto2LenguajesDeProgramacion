module Operaciones.CargarMostrarIngresos(
    cargarIngreso,
    mostrarIngreso,
    actualizarStock
) where

import Operaciones.CargarMostrarArticulos (splitComa)
import System.IO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Map as Map

data Ingreso = Ingreso { codigoIngreso :: String
                       , idUsuario :: String
                       , fecha :: String
                       , lineas :: [LineaIngreso]
                       } deriving (Show)

data LineaIngreso = LineaIngreso { codigoArticulo :: String
                                 , identificadorBodega :: String
                                 , cantidad :: Int
                                 } deriving (Show)

type Stock = Map.Map (String, String) Int

rutaBase :: FilePath
rutaBase = "./Archivos/"

cargarIngreso :: String -> String -> [String] -> [String] -> IO Ingreso
cargarIngreso idUsuario fileName articulosExistentes bodegasExistentes = do
    tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
    contenido <- readFile (rutaBase ++ fileName)
    let lineasIngreso = map (parseLineaIngreso articulosExistentes bodegasExistentes) (lines contenido)
    return $ Ingreso (idUsuario ++ "_" ++ tiempo) idUsuario tiempo lineasIngreso

parseLineaIngreso :: [String] -> [String] -> String -> LineaIngreso
parseLineaIngreso articulosExistentes bodegasExistentes linea =
    case splitComa linea of
        [cod, id, cant] ->
            if cantidadValida (read cant :: Int)
                then if cod `elem` articulosExistentes
                         then if id `elem` bodegasExistentes
                                  then LineaIngreso cod id (read cant :: Int)
                                  else error "Identificador de bodega no existe"
                         else error "Codigo de articulo no existe"
                else error "Cantidad no valida"
        _ -> error "Formato de linea de ingreso incorrecto."

cantidadValida :: Int -> Bool
cantidadValida cant = cant > 0

mostrarIngreso :: Ingreso -> IO ()
mostrarIngreso ingreso = do
    putStrLn $ "Codigo de ingreso: " ++ codigoIngreso ingreso
    putStrLn $ "ID de usuario: " ++ idUsuario ingreso
    putStrLn $ "Fecha: " ++ fecha ingreso
    putStrLn "Lineas de ingreso:"
    mapM_ print (lineas ingreso)

mostrarLineasPorCodigo :: String -> [Ingreso] -> IO ()
mostrarLineasPorCodigo codigo ingresos = do
    case filter (\ingreso -> codigoIngreso ingreso == codigo) ingresos of
        [] -> putStrLn "No se encontro un ingreso con ese codigo."
        [ingreso] -> mostrarIngreso ingreso
        _ -> putStrLn "Error: Codigo de ingreso duplicado."

actualizarStock :: Ingreso -> Stock -> Stock
actualizarStock ingreso stockInicial =
    foldl (\stock (LineaIngreso cod idBodega cant) -> Map.insertWith (+) (cod, idBodega) cant stock) stockInicial (lineas ingreso)

