module Operaciones.CargarMostrarIngresos(
    cargarIngreso,
    mostrarIngreso,
    guardarIngreso,
    consultarIngresoPorCodigo,
    cargarIngresosDesdeJSON,
    mostrarLineasPorCodigo
) where

import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe, mapMaybe)
import Datas.Data
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO
import Operaciones.CargarMostrarArticulos
import System.Directory (getCurrentDirectory)
import System.IO.Error (catchIOError)
import System.FilePath ((</>))

cargarIngreso :: String -> String -> [Articulo] -> [Bodega] -> [Usuario] -> IO (Maybe Ingreso)
cargarIngreso idUsuario fileName articulosExistentes bodegasExistentes usuarios = do
    let maybeUsuario = find (\usuario -> (show (getCedula usuario)) == idUsuario) usuarios
    case maybeUsuario of
        Just _ -> do
            tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
            contenido <- readFile ("app\\Operaciones\\Archivos\\" ++ fileName)
            let lineasIngreso = map (parseLineaIngreso articulosExistentes bodegasExistentes) (lines contenido)
            putStrLn $ "ID de Ingreso Generado: " ++ (idUsuario ++ "_" ++ tiempo)
            return $ Just $ Ingreso (idUsuario ++ "_" ++ tiempo) idUsuario tiempo lineasIngreso
        Nothing -> do
            putStrLn "El idUsuario especificado no existe."
            return Nothing

parseLineaIngreso :: [Articulo] -> [Bodega] -> String -> LineaIngreso
parseLineaIngreso articulosExistentes bodegasExistentes linea =
    case splitComa linea of
        [cod, id, cant] ->
            if cantidadValida (read cant :: Int)
                then case findArticulo cod articulosExistentes of
                         Just articulo -> case findBodega (read id :: Int) bodegasExistentes of
                                               Just bodega -> if cantidadDisponibleValida (read cant :: Int) bodega
                                                                  then LineaIngreso (codigoArticulo articulo) (show (idBodega bodega)) (read cant :: Int)
                                                                  else error "La cantidad ingresada sobrepasa la capacidad de la bodega"
                                               Nothing -> error "Identificador de bodega no existe"
                         Nothing -> error "Código de artículo no existe"
                else error "Cantidad no válida"
        _ -> error "Formato de línea de ingreso incorrecto."

findBodega :: Int -> [Bodega] -> Maybe Bodega
findBodega _ [] = Nothing
findBodega idBodega (b:bodegas)
    | idBodega == getID b = Just b
    | otherwise = findBodega idBodega bodegas

cantidadValida :: Int -> Bool
cantidadValida cant = cant > 0

cantidadDisponibleValida :: Int -> Bodega -> Bool
cantidadDisponibleValida cant bodega = (cant + sum (map getCantidadLineaIngreso (stock bodega))) <= round (getCapacidad bodega)

mostrarIngreso :: Ingreso -> IO ()
mostrarIngreso ingreso = do
    putStrLn $ "Codigo de ingreso: " ++ codigoIngreso ingreso
    putStrLn $ "ID de usuario: " ++ idUsuario ingreso
    putStrLn $ "Fecha: " ++ fecha ingreso
    putStrLn "Lineas de ingreso:"
    mapM_ mostrarLineaIngreso (lineasIngreso ingreso)
    putStrLn ""

mostrarLineaIngreso :: LineaIngreso -> IO ()
mostrarLineaIngreso linea =
    putStrLn $ "> Código: " ++ codigoLineaIngreso linea ++ "\t" ++ "Cantidad: " ++ show (cantidad linea)

mostrarLineasPorCodigo :: String -> [Ingreso] -> IO ()
mostrarLineasPorCodigo codigo ingresos = do
    case filter (\ingreso -> codigoIngreso ingreso == codigo) ingresos of
        [] -> putStrLn "No se encontro un ingreso con ese codigo."
        [ingreso] -> mostrarIngreso ingreso
        _ -> putStrLn "Error: Codigo de ingreso duplicado."

guardarIngreso :: Ingreso -> IO ()
guardarIngreso ingreso = do
    ingresos <- cargarIngresosDesdeJSON
    let nuevosIngresos = ingreso : ingresos
    B.writeFile "app\\BasesDeDatos\\Ingresos.json" (encode nuevosIngresos)
    putStrLn "\nSe ha guardado los ingresos."

cargarIngresosDesdeJSON :: IO [Ingreso]
cargarIngresosDesdeJSON = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Ingresos.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right ingresos -> return ingresos

consultarIngresoPorCodigo :: String -> IO ()
consultarIngresoPorCodigo codigo = do
    ingresos <- cargarIngresosDesdeJSON
    mostrarLineasPorCodigo codigo ingresos
