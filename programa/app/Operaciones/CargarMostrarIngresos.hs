module Operaciones.CargarMostrarIngresos(
    cargarIngreso,
    mostrarIngreso
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

cargarIngreso :: String -> String -> IO Ingreso
cargarIngreso idUsuario ruta = do
    tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
    contenido <- readFile ruta
    let lineasIngreso = map parseLineaIngreso (lines contenido)
    return $ Ingreso (idUsuario ++ "_" ++ tiempo) idUsuario tiempo lineasIngreso

parseLineaIngreso :: String -> LineaIngreso
parseLineaIngreso linea =
    case splitComa linea of
        [cod, id, cant] -> LineaIngreso cod id (read cant :: Int)
        _ -> error "Formato de línea de ingreso incorrecto."

mostrarIngreso :: Ingreso -> IO ()
mostrarIngreso ingreso = do
    putStrLn $ "Código de ingreso: " ++ codigoIngreso ingreso
    putStrLn $ "ID de usuario: " ++ idUsuario ingreso
    putStrLn $ "Fecha: " ++ fecha ingreso
    putStrLn "Líneas de ingreso:"
    mapM_ print (lineas ingreso)

mostrarLineasPorCodigo :: String -> [Ingreso] -> IO ()
mostrarLineasPorCodigo codigo ingresos = do
    case filter (\ingreso -> codigoIngreso ingreso == codigo) ingresos of
        [] -> putStrLn "No se encontró un ingreso con ese código."
        [ingreso] -> mostrarIngreso ingreso
        _ -> putStrLn "Error: Código de ingreso duplicado."

actualizarStock :: Ingreso -> Stock -> Stock
actualizarStock ingreso stockInicial =
    foldl (\stock (LineaIngreso cod idBodega cant) -> Map.insertWith (+) (cod, idBodega) cant stock) stockInicial (lineas ingreso)

main :: IO ()
main = do
    putStrLn "Ingrese la ruta del archivo de ingresos de inventario:"
    ruta <- getLine
    putStrLn "Ingrese el ID de usuario:"
    idUsuario <- getLine
    ingreso <- cargarIngreso idUsuario ruta
    putStrLn "Ingreso de inventario cargado:"
    mostrarIngreso ingreso
