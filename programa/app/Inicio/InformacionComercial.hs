module Inicio.InformacionComercial(
    cargarDatosEmpresa
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Datas.Data
import Data.Text (Text, pack)
import GHC.Generics
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import System.IO

{-
Entradas: Nada
Salidas: La empresa creada.
-}
cargarDatosEmpresa :: IO Empresa
cargarDatosEmpresa =
    do
        cwd <- getCurrentDirectory
        let direccion= cwd </> "app\\BasesDeDatos\\Empresa.json"
        empresa <- readJSONFileBusiness direccion
        if length empresa == 0 then
            do
                nuevaEmpresa <- crearEmpresa
                return nuevaEmpresa
        else
            do
                let empresaGuardada = empresa !! 0
                return empresaGuardada

{-
Entradas: Nada
Salidas: La empresa creada.
-}
crearEmpresa :: IO Empresa
crearEmpresa =
    do
        putStr "\nIngrese el nombre de la empresa: "
        hFlush stdout
        ioNombreEmpresa <- getLine
        let nombreEmpresa = pack ioNombreEmpresa
        putStr "Ingrese el sitio web de la empresa: "
        hFlush stdout
        ioSitioWeb <- getLine
        let sitioWeb = pack ioSitioWeb
        putStr "Ingrese el contacto de la empresa: "
        hFlush stdout
        ioContacto <- getLine
        let contacto = pack ioContacto
        let empresa = Empresa nombreEmpresa sitioWeb contacto
        guardarEmpresa empresa
        return (Empresa nombreEmpresa sitioWeb contacto)

{-
Entradas: La empresa a guardar.
Salidas: El mensaje de éxito.
-}
guardarEmpresa :: Empresa -> IO()
guardarEmpresa empresa =
    do
        let json = encode empresa
        cwd <- getCurrentDirectory
        let direccion = cwd </> "app\\BasesDeDatos\\Empresa.json"
        B.writeFile direccion json
        putStrLn "\nSe ha guardado la información"

{-
Entradas: La dirección de la base de datos de la empresa.
Salidas: La empresa guardada.
-}
readJSONFileBusiness :: FilePath -> IO [Empresa]
readJSONFileBusiness filePath = do
  fileContent <- B.readFile filePath
  case eitherDecode fileContent of
    Left err -> do
      return []
    Right empresa -> return [empresa]