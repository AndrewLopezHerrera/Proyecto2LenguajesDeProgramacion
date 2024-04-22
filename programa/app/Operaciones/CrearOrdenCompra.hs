module Operaciones.CrearOrdenCompra (
    crearOrdenCompra,
    guardarOrdenCompraJSON,
    cargarOrdenesDesdeJSON
) where

import Data.Aeson
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

guardarOrdenCompraJSON :: FilePath -> OrdenCompra -> IO ()
guardarOrdenCompraJSON fileName ordenCompra = do
    let json = encode ordenCompra
    B.appendFile fileName json

cargarOrdenesDesdeJSON :: IO [OrdenCompra]
cargarOrdenesDesdeJSON = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app/BasesDeDatos/OrdenesCompra.json"
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right ordenes -> return ordenes

eliminarOrdenPorId :: FilePath -> String -> IO ()
eliminarOrdenPorId fileName idOrdenEliminar = do
    ordenes <- cargarOrdenesDesdeJSON
    let ordenesActualizadas = filter (\orden -> idOrden orden /= idOrdenEliminar) ordenes
    B.writeFile fileName (encode ordenesActualizadas)