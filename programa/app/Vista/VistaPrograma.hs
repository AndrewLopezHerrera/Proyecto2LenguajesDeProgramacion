module Vista.VistaPrograma (
    ejecutarMenuPrincipal
) where

ejecutarMenuPrincipal :: OI ()
ejecutarMenuPrincipal opcion =
    do
        imprimirMenuPrincipal;
        let opcion <- getLine;
        case opcion of
            "1" -> ejecutarMenuPrincipal opcion
            "2" -> ejecutarMenuPrincipal opcion
            "3" -> ejecutarMenuPrincipal opcion
            "4" -> ejecutarMenuPrincipal opcion
            "5" -> ejecutarMenuPrincipal opcion
            "6" -> ejecutarMenuPrincipal opcion
            "7" -> do strPutLn "\n\t***Hasta luego***"

imprimirMenuPrincipal :: IO -> IO()
imprimirMenuPrincipal = do
    putStr "Menu principal\n\t1. Cargar y mostrar artículos"
    hFlush stdout
    putStr "\t2. Cargar y mostrar ingresos de inventario\n\t3. Crear orden de compra"
    hFlush stdout
    putStr "\t4. Facturar\n\t5. Ver stock de bodegas\n\t6. Opciones generales\n\t7. Salir"
    hFlush stdout
    putStr "\nSeleccione una opción: "
    hFlush stdout