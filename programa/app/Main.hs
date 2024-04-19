module Main where

import Datas.Data
import Inicio.InformacionComercial
import Operaciones.CargarMostrarArticulos

main :: IO ()
main = 
    do
        putStrLn "Bienvenido al Sistema de Inventarios"
        empresa <- cargarDatosEmpresa
        print empresa
        putStrLn "Ingrese la ruta del archivo de artículos:"
        ruta <- getLine
        articulos <- cargarArticulos ruta
        putStrLn "Artículos cargados:"
        mostrarArticulos articulos
