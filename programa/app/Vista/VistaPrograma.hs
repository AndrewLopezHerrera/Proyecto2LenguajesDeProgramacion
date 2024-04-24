module Vista.VistaPrograma (
    ejecutarMenuPrincipal
) where

import System.IO
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
import Operaciones.CrearOrdenCompra
import Operaciones.CargarMostrarIngresos
import Operaciones.CargarMostrarArticulos
import Operaciones.OpcionesGenerales
import Operaciones.Facturar
import Operaciones.StockBodegas
import Datas.Data
import Data.Maybe

ejecutarMenuPrincipal :: IO ()
ejecutarMenuPrincipal =
    do
        empresa <- cargarDatosEmpresa
        bodegas <- cargarDatosBodega
        usuarios <- cargarDatosUsuarios
        articulos <- cargarArticulosDesdeJSON
        ordenesCompra <- cargarOrdenesDesdeJSON
        facturas <- cargarFacturas
        imprimirMenuPrincipal
        opcion <- getLine
        case opcion of
            "1" -> do putStrLn "Ingrese la ruta del archivo de articulos:"
                      ruta <- getLine
                      articulos <- cargarArticulos ruta
                      putStrLn "Articulos cargados:"
                      mostrarArticulos articulos
            "2" -> do putStrLn "Ingrese la ruta del archivo de ingreso:"
                      ruta <- getLine
                      putStrLn "Ingrese la usuario que realiza ingreso:"
                      user <- getLine
                      ingreso <- cargarIngreso user ruta articulos bodegas usuarios
                      putStrLn "Ingreso cargado:"
                      mostrarIngreso (fromMaybe (error "El valor Maybe es Nothing") ingreso)
            "3" -> do orden <- crearOrdenCompra
                      guardarOrdenCompraJSON orden
            --"4" -> facturarOrdenCompra ordenesCompra bodegas empresa
            "5" -> verStockBodegas bodegas
            "6" -> ejecutarMenuOpcionesGenerales
            "7" -> putStrLn "\n\t***Hasta luego***"
            _   -> putStrLn "Opcion no invalida"

imprimirMenuPrincipal :: IO ()
imprimirMenuPrincipal = do
    putStr "Menu principal\n\t1. Cargar y mostrar articulos"
    hFlush stdout
    putStr "\t2. Cargar y mostrar ingresos de inventario\n\t3. Crear orden de compra"
    hFlush stdout
    putStr "\t4. Facturar\n\t5. Ver stock de bodegas\n\t6. Opciones generales\n\t7. Salir"
    hFlush stdout
    putStr "\nSeleccione una opcion: "
    hFlush stdout
