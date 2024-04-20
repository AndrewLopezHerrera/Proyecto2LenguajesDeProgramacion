module Operaciones.CrearOrdenCompra (
    crearOrdenCompra
) where

import System.IO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data OrdenCompra = OrdenCompra { idOrden :: String
                               , cedulaCliente :: String
                               , nombreCliente :: String
                               , fecha :: String
                               , lineas :: [LineaOrdenCompra]
                               } deriving (Show)

data LineaOrdenCompra = LineaOrdenCompra { codigoArticulo :: String
                                         , cantidad :: Int
                                         } deriving (Show)

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
