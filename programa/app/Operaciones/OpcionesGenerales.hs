import Operaciones.Facturar
import Operaciones.CargarMostrarArticulos
import Operaciones.CargarMostrarIngresos
import Operaciones.CrearOrdenCompra
import Datas.Data
import Data.Text (Text, pack, unpack)

ejecutarMenuOpcionesGenerales :: [Bodega] ->  IO ()
ejecutarMenuOpcionesGenerales bodegas = do
    facturas <- cargarFacturas
    articulos <- cargarArticulosDesdeJSON
    ordenesCompra <- cargarOrdenesDesdeJSON
    imprimirMenuOpcionesGenerales
    opcion <- getLine
    case opcion of
        "1" -> consultarOrdenCompra ordenesCompra articulos
        "2" -> consultarFactura facturas
        "3" -> 
        "4" -> putStrLn "\nVolviendo..."
        _ -> putStrLn "\nOpción incorrecta.\n"

imprimirMenuOpcionesGenerales :: IO()
imprimirMenuOpcionesGenerales = do
    putStr "\t\tOpciones Generales\n\t1. Consultar orden compra\n\t2. Consultar factura"
    hFlush stdout
    putStr "\n\t3. Retornar mercadería\n\t4. Volver\nSeleccione un opción"
    hFlush stdout

consultarFactura :: [Factura] -> IO()
consultarFactura facturas = do
    putStr "Ingrese el ID factura que desea consultar: "
    hFlush stdout
    ioID <- getLine
    let 
        id = pack ioID :: Text
        facturaArray = buscarFactura id facturas 0
    in
        if length facturaArray == 0 then
            putStrLn "\nNo se ha encontrado la factura\n"
        else do
            let
                factura = head facturaArray
                idFactura = getIdFactura factura
                nombreEmpresa = getNombreEmpresaFactura factura
                sitioWebEmpresa = getSitioWebEmpresaFactura factura
                contacto = getContactoEmpresaFactura factura
                cedulaCliente = getCedulaClienteFactura factura
                nombreCliente = getNombreClienteFactura factura
                estadoFactura = getEstadoFactura factura
                fechaFactura = getFechaFactura factura
                articulosFactura = getArticulosFactura
            in do
                putStrLn ("\nId: " ++ unpack idFactura)
                putStrLn ("Empresa: " ++ unpack nombreEmpresa)
                putStrLn ("Sitio web: " ++ unpack sitioWebEmpresa)
                putStrLn ("Contacto: " ++ unpack contacto)
                putStrLn ("Cedula del cliente: " ++ show cedulaCliente)
                putStrLn ("Nombre del cliente: " ++ unpack nombreCliente)
                putStrLn ("Estado de la factura: " ++ unpack estadoFactura)
                putStrLn ("Fecha de la factura: " ++ (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" fechaFactura))
                putStrLn "Artículos :"
                mostrarArticuloFactura articulosFactura

mostrarArticuloFactura :: [ArticuloFactura] -> Int -> IO()
mostrarArticuloFactura articulos indice =
    if length articulos == 0 then
        putStrLn "--------------------------------------------\n"
    else
        let
            articulo = articulos !! indice
            codigoArticulo = getCodigoArticuloFactura articulo
            nombreArticulo = getNombreArticuloFactura articulo
            costoArticulo = getCostoArticuloFactura articulo
            tipoArticulo = getTipoArticuloFactura articulo
            tipoIVAArticulo = getTipoIVAArticuloFactura articulo
            cantidadArticulo = getCantidadArticulosFactura articulo
            subTotal = getSubTotalArticuloFactura articulo
            total = getTotalArticuloFactura articulo
        in do
            putStrLn ("Código: " ++ unpack codigoArticulo)
            putStrLn ("Nombre: " ++ unpack nombreArticulo)
            putStrLn ("Costo: " ++ show costoArticulo)
            putStrLn ("Tipo: " ++ show tipoArticulo)
            putStrLn ("Tipo IVA: " ++ show tipoIVAArticulo)
            putStrLn ("Cantidad: " ++ show cantidadArticulo)
            putStrLn ("Subtotal: " ++ show subTotal)
            putStrLn ("Total: " ++ show total)
            putStrLn("=====================================")
            mostrarArticuloFactura articulos (indice + 1)
            

buscarFactura :: Text -> [Factura] -> Int -> [Factura]
buscarFactura idFactura facturas indice =
    if length factura == indice then
        []
    else
        let
            factura = facturas !! indice
            idFacturaGuardada = getIdFactura(factura)
        in
            if idFacturaGuardada == idFactura then
                [factura]
            else
                buscarFactura idFactura facturas (indice + 1)

consultarOrdenCompra :: [OrdenCompra] -> [Artículo] -> IO()
consultarOrdenCompra ordenesCompra articulos = do
    putStr "Ingrese el ID de la orden de compra a buscar: "
    hFlush stdout
    ioID <- getLine
    let
        id = pack id :: Text
        ordenCompraArray = buscarOrdenCompra id ordenesCompra
    in
        if length ordenCompraArray == 0 then
            putStrLn "\nNo se ha encontrado la orden de compra\n"
        else
            let
                ordenCompra = head ordenCompraArray
                idOrden = getIdOrdenCompra ordenCompra
                cedulaCliente = getCedulaClienteOrdenCompra ordenCompra
                nombreCliente = getNombreClienteOrdenCompra ordenCompra
                fechaOrden = getFechaOrdenCompra ordenCompra
                lineas = getLineasOrdenCompra ordenCompra
            in do
                putStrLn ("ID: " ++ idOrden)
                putStrLn ("Cedula del cliente: " ++ cedulaCliente)
                putStrLn ("Nombre del cliente: " ++ nombreCliente)
                putStrLn ("Fecha: " ++ fechaOrden)
                putStrLn ("Artículos: ")
                mostrarArticulosOrdenCompra lineas articulos 0

mostrarArticulosOrdenCompra :: [LineaOrdenCompra] -> [Artículo] -> IO()
mostrarArticulosOrdenCompra lineas articulos indice =
    if length lineas == 0 then
        putStrLn "\tNo hay artículos en la orden de compra"
    else if length lineas == indice then
        putStr "============================================"
    else
        let
            linea = lineas !! indice
            codigoArticulo = getCodigoArticuloLineaIngreso linea
            articuloArray = buscarArticulo articulos codigoArticulo 0
        in
            if length articulo == 0 then
                mostrarArticulosOrdenCompra lineas articulos (indice + 1)
            else
                let
                    articulo = head articuloArray
                    nombre = getNombreArticulo articulo
                    costo = getCostoArticulo articulo
                    tipo = getTipoArticulo articulo
                    tipoIVA = getTipoIVAArticulo articulo
                in
                    putStrLn ("\tCodigo: " ++ codigoArticulo)
                    putStrLn ("\tNombre: " ++ nombre)
                    putStrLn ("\tCosto: " ++ show costo)
                    putStrLn ("\tTipo: " ++ show tipo)
                    putStrLn ("Tipo IVA: " ++ show tipoIVA)
                    putStrLn ("---------------------------------")
                    mostrarArticulosOrdenCompra lineas articulos (indice + 1)

buscarArticulo :: [Artículo] -> String -> Int -> [Articulo]
buscarArticulo articulos codigoArticulo indice =
    if length articulos == 0 then
        []
    else
        let
            articulo <- articulos indice
            codigoArticuloGuardado = getCodigoArticulo articulo
        in
            if codigoArticuloGuardado == codigoArticulo then
                [articulo]
            else
                buscarArticulo articulos codigoArticulo (indice + 1)
            
        

buscarOrdenCompra :: Text -> [OrdenCompra] -> Int -> IO()
buscarOrdenCompra idOrdenCompra ordenesCompra indice =
    if length ordenesCompra == indice then
        []
    else
        let
            ordenCompra = ordenesCompra !! indice
            idOrdenCompraGuardado = getIdOrdenCompra ordenCompra
        in
            if idOrdenCompraGuardado == ordenCompra then
                [ordenCompra]
            else
                buscarOrdenCompra idOrdenCompra ordenesCompra (indice + 1)