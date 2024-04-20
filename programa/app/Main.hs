module Main where

import Datas.Data
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
import Operaciones.CargarMostrarArticulos

main :: IO ()
main = 
    do
        putStrLn "Bienvenido al Sistema de Inventarios"
        empresa <- cargarDatosEmpresa
        print empresa
        bodegas <- cargarDatosBodega
        print bodegas
        usuarios <- cargarDatosUsuarios
        print usuarios
        putStrLn "Ingrese la ruta del archivo de artículos:"
        ruta <- getLine
        articulos <- cargarArticulos ruta
        putStrLn "Artículos cargados:"
        mostrarArticulos articulos
