{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Operaciones.CrearOrdenCompra (
    crearOrdenCompra,
    guardarOrdenCompraJSON,
    cargarOrdenesDesdeJSON,
    eliminarOrdenPorId,
    mostrarOrdenCompra
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromMaybe, mapMaybe)
import Datas.Data
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO
import Data.List
import Data.Text (Text, pack)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Operaciones.Facturar (textTostring)

{-
Entradas: La lista de usuarios existentes.
Salidas: Una orden de compra.
-}
crearOrdenCompra :: [Usuario] -> IO (Maybe OrdenCompra)
crearOrdenCompra usuarios= do
    putStrLn "Ingrese la cedula del cliente:"
    cedula <- getLine
    let maybeUsuario = find (\usuario -> (show (getCedula usuario)) == cedula) usuarios
    case maybeUsuario of
        Just user -> do
            tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
            lineas <- ingresarLineasOrdenCompra []
            let idOrdenGenerado = cedula ++ "_" ++ tiempo
            putStrLn $ "ID de Orden Generada: " ++ show idOrdenGenerado
            return $ Just $ OrdenCompra idOrdenGenerado cedula (textTostring (getNombre user)) tiempo lineas
        Nothing -> do
            putStrLn "El idUsuario especificado no existe."
            return Nothing

{-
Entradas: Una lista con articulos.
Salidas: Una lista con artículos escogidos.
-}
ingresarLineasOrdenCompra :: [LineaOrdenCompra] -> IO [LineaOrdenCompra]
ingresarLineasOrdenCompra lineasPrevias = do
    putStrLn "Ingrese el codigo del articulo (o 'fin' para finalizar):"
    codigo <- getLine
    if codigo == "fin"
        then return lineasPrevias
        else do
            putStrLn "Ingrese la cantidad:"
            cantidad <- readLn :: IO Int
            ingresarLineasOrdenCompra (LineaOrdenCompra codigo cantidad : lineasPrevias)

{-
Entradas: Guarda una orden de compra a un archivo JSON.
Salidas: El mensaje de éxito.
-}
guardarOrdenCompraJSON :: OrdenCompra -> IO ()
guardarOrdenCompraJSON nuevaOrdenCompra = do
    ordenesAnteriores <- cargarOrdenesDesdeJSON
    let ordenesActualizadas = nuevaOrdenCompra : ordenesAnteriores
    B.writeFile "app\\BasesDeDatos\\OrdenesCompra.json" (encode ordenesActualizadas)
    putStrLn "\nSe ha guardado la orden de compra."

{-
Entradas: Nada
Salidas: Una lista de ordenes de compra cargados desde JSON.
-}
cargarOrdenesDesdeJSON :: IO [OrdenCompra]
cargarOrdenesDesdeJSON = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\OrdenesCompra.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right ordenes -> return ordenes

{-
Entradas: El ID de la ordena a eliminar.
Salidas: Nada.
-}
eliminarOrdenPorId :: String -> IO ()
eliminarOrdenPorId idOrdenEliminar = do
    ordenes <- cargarOrdenesDesdeJSON
    let ordenesActualizadas = filter (\orden -> idOrden orden /= idOrdenEliminar) ordenes
    B.writeFile "app\\BasesDeDatos\\OrdenesCompra.json" (encode ordenesActualizadas)

{-
Entradas: Una orden de compra.
Salidas: La información de la orden de compra.
-}
mostrarOrdenCompra :: OrdenCompra -> IO ()
mostrarOrdenCompra orden = do
    putStrLn $ "ID de Orden: " ++ getIdOrdenCompra orden
    putStrLn $ "Cédula del Cliente: " ++ getCedulaClienteOrdenCompra orden
    putStrLn $ "Nombre del Cliente: " ++ getNombreClienteOrdenCompra orden
    putStrLn $ "Fecha de la Orden: " ++ getFechaOrdenCompra orden
    putStrLn "Líneas de Compra:"
    mapM_ mostrarLineaOrdenCompra (getLineasOrdenCompra orden)

{-
Entradas: Una línea de la orden de compra
Salidas: La información de la línea.
-}
mostrarLineaOrdenCompra :: LineaOrdenCompra -> IO ()
mostrarLineaOrdenCompra linea = do
    putStrLn $ "Código de Artículo: " ++ getCodigoArticuloOrdenCompra linea
    putStrLn $ "Cantidad: " ++ show (getCantidadArticuloOrdenCompra linea)