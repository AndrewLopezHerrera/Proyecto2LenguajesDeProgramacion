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

{-
Entradas: Nada
Salidas: Nada
-}
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
                      agregarArticulos articulos
                      ejecutarMenuPrincipal
            "2" -> do putStrLn "Ingrese la ruta del archivo de ingreso:"
                      ruta <- getLine
                      putStrLn "Ingrese la indentificacion que realiza el ingreso:"
                      user <- getLine
                      maybeIngreso <- cargarIngreso user ruta articulos bodegas usuarios
                      case maybeIngreso of
                            Just ingreso -> do
                                putStrLn "Ingreso cargado:"
                                mostrarIngreso ingreso
                                guardarIngreso ingreso
                                let nuevasBodegas = actualizarBodegas (getLineasIngreso ingreso) bodegas
                                guardarBodegas nuevasBodegas
                            Nothing -> putStrLn "Fallo al cargar el ingreso. No se ejecutarán los programas."
                      ejecutarMenuPrincipal
            "3" -> do maybeOrden <- crearOrdenCompra usuarios
                      case maybeOrden of
                            Just orden -> do
                                guardarOrdenCompraJSON orden
                            Nothing -> putStrLn "Fallo al crear la orden. No se ejecutarán los programas."
                      ejecutarMenuPrincipal
            "4" -> do facturacion ordenesCompra bodegas usuarios empresa articulos
                      ejecutarMenuPrincipal
            "5" -> do verStockBodegas bodegas
                      ejecutarMenuPrincipal
            "6" -> do ejecutarMenuOpcionesGenerales
                      ejecutarMenuPrincipal
            "7" -> do ejecutarMenuExtra
                      ejecutarMenuPrincipal
            "8" -> putStrLn "\n\t***Hasta luego***"
            _   -> do putStrLn "Opcion no invalida"
                      ejecutarMenuPrincipal

{-
Entradas: Nada
Salidas: Nada
-}
imprimirMenuPrincipal :: IO ()
imprimirMenuPrincipal = do
    putStr "Menu principal\n1. Cargar y mostrar articulos"
    hFlush stdout
    putStr "\t2. Cargar y mostrar ingresos de inventario\n3. Crear orden de compra"
    hFlush stdout
    putStr "\t4. Facturar\n5. Ver stock de bodegas\t\t6. Opciones generales\n7. Opciones extras\t\t8. Salir"
    hFlush stdout
    putStr "\nSeleccione una opcion: "
    hFlush stdout

{-
Entradas: Nada
Salidas: Nada
-}
ejecutarMenuExtra :: IO ()
ejecutarMenuExtra =
    do
        bodegas <- cargarDatosBodega
        imprimirMenuExtra
        opcion <- getLine
        case opcion of
            "1" -> do nuevos <- crearBodegas (length bodegas)
                      anadirBodegas nuevos
                      ejecutarMenuExtra
            "2" -> do putStrLn "Ingrese la indentificacion del ingreso:"
                      codigo <- getLine
                      ingresos <- cargarIngresosDesdeJSON
                      mostrarLineasPorCodigo codigo ingresos
                      ejecutarMenuExtra
            "3" -> putStrLn "\nVolviendo..."
            _   -> do putStrLn "Opcion no invalida"
                      ejecutarMenuExtra

{-
Entradas: Nada
Salidas: Nada
-}
imprimirMenuExtra :: IO ()
imprimirMenuExtra = do
    putStr "Menu Extra\n1. Crear Bodegas"
    hFlush stdout
    putStr "\t2. Consultar Ingreso\n3. Volver"
    hFlush stdout
    putStr "\nSeleccione una opcion: "
    hFlush stdout
