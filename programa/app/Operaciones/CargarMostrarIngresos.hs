module Operaciones.CargarMostrarIngresos(
    cargarIngreso,
    mostrarIngreso,
    actualizarBodegas,
    guardarIngreso,
    consultarIngresoPorCodigo
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe, mapMaybe)
import Datas.Data
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO

cargarIngreso :: String -> String -> [Articulo] -> [Bodega] -> IO Ingreso
cargarIngreso idUsuario fileName articulosExistentes bodegasExistentes = do
    tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
    contenido <- readFile fileName
    let lineasIngreso = map (parseLineaIngreso articulosExistentes bodegasExistentes) (lines contenido)
    return $ Ingreso (idUsuario ++ "_" ++ tiempo) idUsuario tiempo lineasIngreso

parseLineaIngreso :: [Articulo] -> [Bodega] -> String -> LineaIngreso
parseLineaIngreso articulosExistentes bodegasExistentes linea =
    case splitComa linea of
        [cod, id, cant] ->
            if cantidadValida (read cant :: String)
                then case findArticulo cod articulosExistentes of
                         Just articulo -> case findBodega (read id :: Int) bodegasExistentes of
                                               Just bodega -> LineaIngreso (codigoArticulo articulo) (idBodega bodega) (read cant :: Int)
                                               Nothing -> error "Identificador de bodega no existe"
                         Nothing -> error "Codigo de articulo no existe"
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

guardarIngreso :: Ingreso -> IO ()
guardarIngreso ingreso = do
    ingresosExistentes <- cargarIngresosDesdeJSON
    let ingresosActualizados = ingreso : ingresosExistentes
    let json = encode ingresosActualizados
    B.writeFile "app\\BasesDeDatos\\Ingresos.json" json

cargarIngresosDesdeJSON :: IO [Ingreso]
cargarIngresosDesdeJSON = do
    contenido <- readFile "app\\BasesDeDatos\\Ingresos.json"
    let lineasIngresos = lines contenido
    return $ mapMaybe decodeIngreso lineasIngresos

decodeIngreso :: String -> Maybe Ingreso
decodeIngreso str = decode (B.pack $ fromMaybe "" str)

consultarIngresoPorCodigo :: String -> IO ()
consultarIngresoPorCodigo codigo = do
    ingresos <- cargarIngresosDesdeJSON "./Archivos/Ingresos.json"
    mostrarLineasPorCodigo codigo ingresos