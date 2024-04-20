module Vista.VistaPrograma (
    ejecutarMenuPrincipal
) where

import System.IO
import Operaciones.CargarMostrarArticulos

ejecutarMenuPrincipal :: IO ()
ejecutarMenuPrincipal =
    do
        imprimirMenuPrincipal
        opcion <- getLine
        case opcion of
            "1" -> do putStrLn "Ingrese la ruta del archivo de artículos:"
                      ruta <- getLine
                      articulos <- cargarArticulos ruta
                      putStrLn "Artículos cargados:"
                      mostrarArticulos articulos
            "2" -> ejecutarMenuPrincipal
            "3" -> ejecutarMenuPrincipal
            "4" -> ejecutarMenuPrincipal
            "5" -> ejecutarMenuPrincipal
            "6" -> ejecutarMenuPrincipal
            "7" -> putStrLn "\n\t***Hasta luego***"
            _   -> putStrLn "Opción no invalida"

imprimirMenuPrincipal :: IO ()
imprimirMenuPrincipal = do
    putStr "Menu principal\n\t1. Cargar y mostrar artículos"
    hFlush stdout
    putStr "\t2. Cargar y mostrar ingresos de inventario\n\t3. Crear orden de compra"
    hFlush stdout
    putStr "\t4. Facturar\n\t5. Ver stock de bodegas\n\t6. Opciones generales\n\t7. Salir"
    hFlush stdout
    putStr "\nSeleccione una opción: "
    hFlush stdout
