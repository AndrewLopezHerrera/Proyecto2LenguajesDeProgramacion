{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Operaciones.CrearOrdenCompra (
    crearOrdenCompra,
    guardarOrdenCompraJSON,
    cargarOrdenesDesdeJSON,
    eliminarOrdenPorId
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B
import Data.List (delete)
import Datas.Data
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

crearOrdenCompra :: IO OrdenCompra
crearOrdenCompra = do
    putStrLn "Ingrese la cedula del cliente:"
    cedula <- getLine
    putStrLn "Ingrese el nombre del cliente:"
    nombre <- getLine
    tiempo <- fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime
    lineas <- ingresarLineasOrdenCompra []
    let idOrdenGenerado = cedula ++ "_" ++ tiempo
    return $ OrdenCompra idOrdenGenerado cedula nombre tiempo lineas

ingresarLineasOrdenCompra :: [LineaOrdenCompra] -> IO [LineaOrdenCompra]
ingresarLineasOrdenCompra lineasPrevias = do
    putStrLn "Ingrese el codigo del articulo (o 'fin' para finalizar):"
    codigo <- getLine
    if codigo == "fin"
        then return lineasPrevias
        else do
            putStrLn "Ingrese la cantidad:"
            cantidad <- readLn :: IO Int
            lineas <- ingresarLineasOrdenCompra (LineaOrdenCompra codigo cantidad : lineasPrevias)
            return lineas

<<<<<<< HEAD
guardarOrdenCompraJSON :: FilePath -> OrdenCompra -> IO ()
guardarOrdenCompraJSON fileName ordenCompra = do
    let json = encode ordenCompra
    B.appendFile fileName json
=======
guardarOrdenCompraJSON :: OrdenCompra -> IO ()
guardarOrdenCompraJSON nuevaOrdenCompra = do
    ordenesAnteriores <- cargarOrdenesDesdeJSON
    let ordenesActualizadas = nuevaOrdenCompra : ordenesAnteriores
    B.writeFile "app\\BasesDeDatos\\OrdenesCompra.json" (encode ordenesActualizadas)
>>>>>>> 4663f9a020c7d219ebe210ae6e701d85486858dd

cargarOrdenesDesdeJSON :: IO [OrdenCompra]
cargarOrdenesDesdeJSON = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\OrdenesCompra.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right ordenes -> return ordenes

eliminarOrdenPorId :: String -> IO ()
eliminarOrdenPorId idOrdenEliminar = do
    ordenes <- cargarOrdenesDesdeJSON
    let ordenesActualizadas = filter (\orden -> idOrden orden /= idOrdenEliminar) ordenes
    B.writeFile "app\\BasesDeDatos\\OrdenesCompra.json" (encode ordenesActualizadas)
