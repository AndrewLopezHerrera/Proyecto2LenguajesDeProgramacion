module Vista.VistaPrograma (
    ejecutarMenuPrincipal
) where

import System.IO
import Operaciones.CargarMostrarArticulos
import Operaciones.Facturar
import Operaciones.CrearOrdenCompra
import Datas.Data

ejecutarMenuPrincipal :: IO ()
ejecutarMenuPrincipal empresa bodegas usuarios =
    do
        imprimirMenuPrincipal
        opcion <- getLine
        case opcion of
            "1" -> do putStrLn "Ingrese la ruta del archivo de articulos:"
                      ruta <- getLine
                      articulos <- cargarArticulos ruta
                      putStrLn "Articulos cargados:"
                      mostrarArticulos articulos
            "2" -> ejecutarMenuPrincipal empresa bodegas usuarios articulos ordenesCompra facturas
            "3" -> ejecutarMenuPrincipal empresa bodegas usuarios articulos ordenesCompra facturas 
            "4" -> crearFactura 
            "5" -> ejecutarMenuPrincipal empresa bodegas usuarios articulos ordenesCompra facturas 
            "6" -> ejecutarMenuPrincipal empresa bodegas usuarios articulos ordenesCompra facturas 
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
