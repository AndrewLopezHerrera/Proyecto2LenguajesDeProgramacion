module Inicio.InformacionBodegas(
    cargarDatosBodega,
    findBodega,
    guardarBodegas,
    anadirBodegas,
    crearBodegas
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Datas.Data
import Data.Text (Text, pack)
import GHC.Generics
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.IO
import Text.Read (readMaybe)

cargarDatosBodega :: IO [Bodega]
cargarDatosBodega =
    do
        cwd <- getCurrentDirectory
        let direccion = cwd </> "app\\BasesDeDatos\\Bodegas.json"
        bodegas <- readJSONFileBusiness direccion
        if null bodegas then do
            bodegasNuevas <- crearBodegas 1
            guardarBodegas bodegasNuevas
            return bodegasNuevas
        else do
            return bodegas

crearBodegas :: Int -> IO [Bodega]
crearBodegas idActual = do
    putStrLn $ "\nBodega con Id " ++ show idActual
    putStr "Ingrese la capacidad en metros cubicos de la bodega: "
    hFlush stdout
    ioCapacidad <- getLine
    let maybeCapacidad = readMaybe ioCapacidad :: Maybe Double
    capacidad <- case maybeCapacidad of
        Just numero -> return numero
        Nothing -> do
            putStrLn "Entrada invalida. Por favor, ingresa un numero."
            return (-1.0)
    if capacidad == (-1.0) then
        crearBodegas idActual
    else do
        putStr "Ingrese la ubicacion de la bodega: "
        hFlush stdout
        ioUbicacion <- getLine
        let ubicacion :: Text
            ubicacion = pack ioUbicacion
        let bodegaNueva = Bodega idActual capacidad ubicacion []
        if idActual >= 5 then do
            putStr "Desea crear una bodega más? Si(S) No(Cualquier otra letra): "
            hFlush stdout
            respuesta <- getLine
            case respuesta of
                "S" -> do
                    restante <- crearBodegas (idActual + 1)
                    return $ bodegaNueva : restante
                _ -> return [bodegaNueva]
        else do
            restante <- crearBodegas (idActual + 1)
            return $ bodegaNueva : restante

guardarBodegas :: [Bodega] -> IO ()
guardarBodegas bodegas = do
    let json = encode bodegas
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app/BasesDeDatos/Bodegas.json"
    B.writeFile direccion json
    putStrLn "\nSe ha guardado la información"

anadirBodegas :: [Bodega] -> IO ()
anadirBodegas nuevasBodegas = do
    bodegasExistentes <- cargarDatosBodega
    putStrLn "Añadiendo nuevas bodegas..."
    guardarBodegas (bodegasExistentes ++ nuevasBodegas)

readJSONFileBusiness :: FilePath -> IO [Bodega]
readJSONFileBusiness direccion = do
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right bodegas -> return bodegas

findBodega :: Int -> [Bodega] -> Maybe Bodega
findBodega objetivo [] = Nothing
findBodega objetivo (bodega:resto)
    | idBodega bodega == objetivo = Just bodega
    | otherwise = findBodega objetivo resto