module Vista.VistaPrograma where

EjecutarMenuPrincipal :: OI
EjecutarMenuPrincipal opcion =
    do
        ImprimirMenuPrincipal;
        let opcion <- getLine;
        case opcion of
            "1" -> ImprimirMenuPrincipal opcion
            "2" -> ImprimirMenuPrincipal opcion
            "3" -> ImprimirMenuPrincipal opcion
            "4" -> ImprimirMenuPrincipal opcion
            "5" -> ImprimirMenuPrincipal opcion
            "6" -> strPutLn "\n\t***Hasta luego***"

ImprimirMenuPrincipal :: IO -> IO()
ImprimirMenuPrincipal = putStrLn "Menu principal\n\t1. Cargar y mostrar artículos"
putStrLn "\t2. Cargar y mostrar ingresos de inventario\n\t3. Crear orden de compra"
putStrLn "\t4. Facturar\n\t5. Ver stock de bodegas\n\t6. Salir\nSeleccione una opción: "