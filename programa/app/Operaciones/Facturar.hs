

cargarFacturas :: IO [Factura]
cargarFacturas = do
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Empresa.json"
    fileContent <- B.readFile direccion
    case eitherDecode fileContent of
        Left err -> do
            return []
        Right factura -> return [factura]

guardarFacturas :: [Factura] -> IO ()
guardarFacturas facturas = do
    let json = encode facturas
    cwd <- getCurrentDirectory
    let direccion = cwd </> "app\\BasesDeDatos\\Facturas.json"
    B.writeFile direccion json
    putStrLn "\nSe ha guardado la factura"

crearFactura :: [OrdenCompra] -> [Factura] -> IO ()
crearFactura ordenesCompra facturas = do
    putStrLn "\t\tFacturación de Ordenes de Compra\n"
    putStr "Escriba el código de la orden de compra a facturar: "
    hFlush stdout
    ioIdOrdenCompra <- getLine
    let idOrdenCompra = pack ioIdOrdenCompra :: Text
    let existeFactura = verificarExistenciaFactura facturas idOrdenCompra 0
    if existeFactura then do
        putStrLn "\nLa factura ya existe\n"
    else
        let
            ordenCompra = verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra 0
        in
            if length ordenCompra == 0 then
                putStrLn ("\nNo existe la orden de compra con el código " ++ idOrdenCompra)
            else
                print("Factura creada")

verificarExistenciaFactura :: [Factura] -> Text -> Integer -> Bool
verificarExistenciaFactura facturas idFactura indice =
    if length facturas == 0 || length facturas == indice then
        false
    else
        let
            factura = facturas !! indice
            idFacturaGuardada = getIdFactura(factura)
        in
            if idFacturaGuardada == idFactura then
                true
            else
                verificarExistenciaFactura facturas idFactura (indice + 1)

verificarExistenciaOrdenCompra :: [OrdenCompra] -> Text -> Integer -> [OrdenCompra]
verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra indice =
    if length ordenesCompra == 0 || length ordenesCompra == indice then
        []
    else
        let
            ordenCompra = ordenesCompra !! indice
            idOrdenCompraString = getIdOrdenCompra(ordenCompra)
            idOrdenCompraGuardado = pack idOrdenCompraString
        in
            if idOrdenCompraGuardado == idOrdenCompra then
                [ordenCompra]
            else
                verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra (indice + 1)

