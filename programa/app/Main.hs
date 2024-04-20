module Main where

import Datas.Data
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios

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
