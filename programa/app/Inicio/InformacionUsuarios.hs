module Inicio.InformacionUsuarios(
    cargarDatosUsuarios,
    obtenerUsuarioPorCedula
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
import Data.Maybe (listToMaybe)
import Data.List (find)

{-
Entradas: Nada
Salidas: Los usuarios.
-}
cargarDatosUsuarios :: IO [Usuario]
cargarDatosUsuarios = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Usuarios.json"
    usuarios <- readJSONFileUsers direccion
    if length usuarios == 0 then do
        usuariosNuevos <- crearUsuarios [] 1
        guardarUsuarios usuariosNuevos
        return usuariosNuevos
    else do
        return usuarios

{-
Entradas: Los usuarios creados. El indice.
Salidas: Los usuarios creados.
-}
crearUsuarios :: [Usuario] -> Int -> IO [Usuario]
crearUsuarios usuarios cantidad = do
    putStrLn ("Usuario " ++ show cantidad)
    putStr "Ingrese la cédula del nuevo usuario: "
    hFlush stdout
    ioCedula <- getLine
    let maybeCedula = readMaybe ioCedula :: Maybe Int
    cedula <- case maybeCedula of
        Just numero -> return numero
        Nothing -> do
            putStrLn "Entrada invalida. Por favor, ingresa un número."
            return (-1)
    if cedula == (-1) then
        crearUsuarios usuarios cantidad
    else do
        let respuesta = verificarDuplicidadCedula usuarios 0 cedula
        if respuesta == 0 then do
            putStrLn "\nEsta cédula ya se ingresó"
            crearUsuarios usuarios cantidad
        else do
            putStr "Ingrese el nombre: "
            hFlush stdout
            ioNombre <- getLine
            let nombre :: Text
                nombre = pack ioNombre
            putStr "Ingrese el primer apellido: "
            hFlush stdout
            ioPrimerApellido <- getLine
            let primerApellido :: Text
                primerApellido = pack ioPrimerApellido
            putStr "Ingrese el segundo apellido: "
            hFlush stdout
            ioSegundoApellido <- getLine
            let segundoApellido :: Text
                segundoApellido = pack ioSegundoApellido
            putStr "Ingrese el puesto: "
            hFlush stdout
            ioPuesto <- getLine
            let puesto :: Text
                puesto = pack ioPuesto
            let usuario = Usuario cedula nombre primerApellido segundoApellido puesto
            let usuarios' = usuario : usuarios
            if cantidad >= 5
                then do
                    putStr "Desea crear un usuario más? Si(S) No(Cualquier otra letra): "
                    hFlush stdout
                    respuesta <- getLine
                    case respuesta of
                        "S" -> crearUsuarios usuarios' (cantidad + 1)
                        _ -> do
                            putStrLn "\nSe han creado los usuarios."
                            return usuarios'
                else
                    crearUsuarios usuarios' (cantidad + 1)

{-
Entradas: Los usuarios creados. El indice. La cedula a evaluar.
Salidas: 1 si no hay coincidencias. 0 si hay coincidencias.
-}
verificarDuplicidadCedula :: [Usuario] -> Int -> Int -> Int
verificarDuplicidadCedula usuarios indice cedula =
    if length usuarios == 0 then
        1
    else
        let 
            usuario = usuarios !! indice
            cedulaGuardada = getCedula usuario
        in
            if cedula == cedulaGuardada then
                0
            else
                if length usuarios == (indice + 1) then
                    1
                else
                    verificarDuplicidadCedula usuarios (indice + 1) cedula

{-
Entradas: Los usuarios a guardar.
Salidas: El mensaje de éxito.
-}
guardarUsuarios :: [Usuario] -> IO ()
guardarUsuarios bodegas = do
    let json = encode bodegas
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app/BasesDeDatos/Usuarios.json"
    B.writeFile direccion json
    putStrLn "\nSe ha guardado la información"

{-
Entradas: La dirección del archivo.
Salidas: Los usuarios cargados desde el archivo JSON.
-}
readJSONFileUsers :: FilePath -> IO [Usuario]
readJSONFileUsers direccion = do
    json <- B.readFile direccion
    case eitherDecode json of
        Left err -> error err
        Right bodegas -> return bodegas

{-
Entradas: El numero de cedula. Los usuarios existentes.
Salidas: El usuario que coincide con la cédula.
-}
obtenerUsuarioPorCedula :: Int -> [Usuario] -> Maybe Usuario
obtenerUsuarioPorCedula cedula = find (\ usuario -> getCedula usuario == cedula)
