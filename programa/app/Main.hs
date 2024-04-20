module Main where

import Datas.Data
import Inicio.InformacionComercial
<<<<<<< HEAD
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
=======
import Operaciones.CargarMostrarArticulos
>>>>>>> 698e1a20f01d3c29c4bc73d0e93508f41b4586e8

main :: IO ()
main = 
    do
        putStrLn "Bienvenido al Sistema de Inventarios"
        empresa <- cargarDatosEmpresa
        print empresa
<<<<<<< HEAD
        bodegas <- cargarDatosBodega
        print bodegas
        usuarios <- cargarDatosUsuarios
        print usuarios
=======
        putStrLn "Ingrese la ruta del archivo de artículos:"
        ruta <- getLine
        articulos <- cargarArticulos ruta
        putStrLn "Artículos cargados:"
        mostrarArticulos articulos
>>>>>>> 698e1a20f01d3c29c4bc73d0e93508f41b4586e8
