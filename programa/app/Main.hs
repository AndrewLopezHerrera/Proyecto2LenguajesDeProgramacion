module Main where

import Datas.Data
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
import Operaciones.CargarMostrarArticulos
import Operaciones.CrearOrdenCompra
import Operaciones.Facturar
import Vista.VistaPrograma

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
        ejecutarMenuPrincipal empresa bodegas usuarios articulos ordenesCompra facturas