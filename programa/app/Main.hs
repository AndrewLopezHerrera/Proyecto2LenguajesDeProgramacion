module Main where

import Datas.Data
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
import Operaciones.CargarMostrarArticulos
import Operaciones.CrearOrdenCompra
import Vista.VistaPrograma

{-
Entradas: Nada
Salidas: Nada
-}
main :: IO ()
main = 
    do
        putStrLn "Bienvenido al Sistema de Inventarios"
        ejecutarMenuPrincipal