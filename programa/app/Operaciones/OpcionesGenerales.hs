module Operaciones.OpcionesGenerales
  ()
where

import Inicio.InformacionBodegas

ejecutarMenuOpcionesGenerales :: [Bodega] -> [Factura] -> [OrdenCompra] -> [Articulos] -> IO ()
ejecutarMenuOpcionesGenerales bodegas facturas ordenesCompra articulos = do
    imprimirMenuOpcionesGenerales
    opcion <- getLine
    case opcion of
        "1" ->
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
                articulos = getLineasOrdenCompra ordenCompra
            in do
                putStrLn ("ID: " ++ idOrden)
                putStrLn ("Cedula del cliente: " ++ cedulaCliente)
                putStrLn ("Nombre del cliente: " ++ nombreCliente)
                putStrLn ("Fecha: " ++ fechaOrden)
                putStrLn ("Arículos: ")

mostrarArticulosOrdenCompra :: [LineaOrdenCompra] -> [Artículo] -> IO()
mostrarArticulosOrdenCompra lineas articulos =

buscarArticulo :: [Artículo] -> String -> Int -> Articulo
buscarArticulo articulos codigoArticulo indice =
    let
        articulo <- articulos indice
        codigoArticuloGuardado = getCodigoArticuloOrdenCompra articulo
        

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

retornarMercaderia :: Text -> [Factura] -> [Bodega] -> Maybe ([Factura], [Bodega])
retornarMercaderia cFactura facturas bodegas = do
    factura <- find (\f -> cFactura == idFactura f && estadoFactura f == "Activa") facturas
    let facturaAnulada = factura { estadoFactura = "Anulada" }
    let bodegasActualizadas = actualizarStock (articulosFactura facturaAnulada) bodegas
    return (facturaAnulada : filter (\f -> idFactura f /= idFactura factura) facturas, bodegasActualizadas)

actualizarStock :: [ArticuloFactura] -> [Bodega] -> [Bodega]
actualizarStock [] bodegas = bodegas
actualizarStock (articulo:articulos) bodegas =
    case findBodegaDeArticulo (codigoArticuloFactura articulo) bodegas of
        Just bodega -> let stockActualizado = sumarStock (codigoArticuloFactura articulo) (cantidadArticuloFactura articulo) (stock bodega)
                       in bodega { stock = stockActualizado } : actualizarStock articulos bodegas
        Nothing -> actualizarStock articulos bodegas

findBodegaDeArticulo :: Text -> [Bodega] -> Maybe Bodega
findBodegaDeArticulo codigoArticulo [] = Nothing
findBodegaDeArticulo codigoArticulo (bodega:bodegas) =
    if any (\linea -> codigoArticulo == codigoLineaIngreso linea) (stock bodega)
        then Just bodega
        else findBodegaDeArticulo codigoArticulo bodegas

sumarStock :: Text -> Int -> [LineaIngreso] -> [LineaIngreso]
sumarStock codigoArticulo cantidad [] = [LineaIngreso codigoArticulo "" cantidad]
sumarStock codigoArticulo cantidad (linea:lineas) =
    if codigoArticulo == codigoArticuloLineaIngreso linea
        then linea { cantidad = cantidad + cantidadLineaIngreso linea } : lineas
        else linea : sumarStock codigoArticulo cantidad lineas