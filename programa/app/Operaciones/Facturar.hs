module Operaciones.Facturar (
    facturacion,
    anadirFactura,
    cargarFacturas,
    mostrarFactura,
    textTostring
) where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Time
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Datas.Data
import GHC.Generics
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO
import Text.Read (readMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.ByteString (unpack)
import Inicio.InformacionUsuarios
import Inicio.InformacionBodegas

anadirFactura :: Factura -> IO ()
anadirFactura nuevaFactura = do
    facturasExistente <- cargarFacturas
    let facturasActualizadas = nuevaFactura : facturasExistente
    guardarFacturas facturasActualizadas
    putStrLn "\nSe ha añadido la factura correctamente."

cargarFacturas :: IO [Factura]
cargarFacturas = do
  cwd <- getCurrentDirectory
  let direccion = cwd </> "app\\BasesDeDatos\\Facturas.json"
  fileContent <- B.readFile direccion
  case eitherDecode fileContent of
    Left err -> error err
    Right facturas -> return facturas

guardarFacturas :: [Factura] -> IO ()
guardarFacturas facturas = do
  let json = encode facturas
  cwd <- getCurrentDirectory
  let direccion = cwd </> "app\\BasesDeDatos\\Facturas.json"
  B.writeFile direccion json
  putStrLn "\nSe ha guardado la factura"

verificarStock :: [(Bodega, Double)] -> [(String, Int)] -> Bool
verificarStock _ [] = True
verificarStock [] _ = False
verificarStock ((bodega, capacidadBodega):restoBodegas) ((codigoArticulo, cantidad):restoArticulos)
    | cantidad <= stockArticulo = verificarStock restoBodegas restoArticulos
    | otherwise = False
    where
        stockArticulo = sum [getCantidadLineaIngreso ingreso | ingreso <- stock bodega, getCodigoArticuloLineaIngreso ingreso == codigoArticulo]

descontarStock :: [(Bodega, Double)] -> [(String, Int)] -> [(Bodega, Double)]
descontarStock [] _ = []
descontarStock _ [] = []
descontarStock ((bodega, capacidadBodega):restoBodegas) ((codigoArticulo, cantidad):restoArticulos) =
    (bodegaActualizada, capacidadBodega) : descontarStock restoBodegas restoArticulos
    where
        stockActualizado = descontarArticuloStock (stock bodega) codigoArticulo cantidad
        bodegaActualizada = bodega { stock = stockActualizado }

descontarArticuloStock :: [LineaIngreso] -> String -> Int -> [LineaIngreso]
descontarArticuloStock [] _ _ = []
descontarArticuloStock (ingreso:restoStock) codigoArticulo cantidad
    | cantidad <= cantidadStock = nuevaLineaIngreso : restoStock
    | otherwise = descontarArticuloStock restoStock codigoArticulo (cantidad - cantidadStock)
    where
        cantidadStock = getCantidadLineaIngreso ingreso
        nuevaCantidadStock = cantidadStock - cantidad
        nuevaLineaIngreso = ingreso { cantidad = nuevaCantidadStock }

buscarArticuloPorCodigo :: String -> [Articulo] -> Maybe Articulo
buscarArticuloPorCodigo _ [] = Nothing
buscarArticuloPorCodigo codigo (articulo:restoArticulos)
    | codigoArticulo articulo == codigo = Just articulo
    | otherwise = buscarArticuloPorCodigo codigo restoArticulos

calcularSubtotal :: [ArticuloFactura] -> Double
calcularSubtotal = sum . map subTotalArticuloFactura

calcularTotal :: [ArticuloFactura] -> Double
calcularTotal = sum . map getTotalArticuloFactura

buscarOrdenPorId :: String -> [OrdenCompra] -> Maybe OrdenCompra
buscarOrdenPorId idOrden = find (\orden -> getIdOrdenCompra orden == idOrden)

obtenerBodegas :: [(Bodega, Double)] -> [Bodega]
obtenerBodegas = map fst

facturar :: OrdenCompra -> [Bodega] -> Usuario -> Empresa -> [Articulo] -> IO (Maybe Factura)
facturar ordenCompra bodegas usuario empresa guarArticulos = do
    let lineasOrdenCompra = lineasCompra ordenCompra
    let articulos = map (\linea -> (codigoLineaOrden linea, cantidadLineaOrden linea)) lineasOrdenCompra
    let bodegasConStock = filter (\bodega -> verificarStock [(bodega, capacidad bodega)] articulos) bodegas
    if null bodegasConStock
        then do
            putStrLn "No hay suficiente stock para facturar esta orden de compra."
            return Nothing
        else do
            let bodegasActualizadas = descontarStock (map (\bodega -> (bodega, capacidad bodega)) bodegasConStock) articulos
            guardarBodegas (obtenerBodegas bodegasActualizadas)
            idFactura <- (getIdOrdenCompra ordenCompra ++) <$> getCurrentTimestamp
            tiempoActual <- getCurrentTimestamp
            let lineasFactura = mapMaybe (\(codigoArticulo, cantidad) -> do
                    articulo <- buscarArticuloPorCodigo codigoArticulo guarArticulos
                    return $ ArticuloFactura
                                codigoArticulo
                                (nombreArticulo articulo)
                                cantidad
                                (costoArticulo articulo)
                                (tipoArticulo articulo)
                                (tipoIVAArticulo articulo)
                                (fromIntegral cantidad * costoArticulo articulo)
                            ) articulos
            let subtotal = calcularSubtotal lineasFactura
            let total = calcularTotal lineasFactura
            return $ Just $ Factura idFactura usuario empresa "Activa" tiempoActual lineasFactura subtotal total

getCurrentTimestamp :: IO String
getCurrentTimestamp = fmap (formatTime defaultTimeLocale "%Y%m%d%H%M%S") getCurrentTime

mostrarFactura :: Factura -> IO ()
mostrarFactura factura = do
    putStrLn "\nInformación de la factura:"
    putStrLn $ "ID: " ++ getIdFactura factura
    putStrLn $ "Cédula del cliente: " ++ show (getCedulaClienteFactura factura)
    putStrLn $ "Nombre del cliente: " ++ textTostring (getNombreClienteFactura factura)
    putStrLn $ "Estado: " ++ getEstadoFactura factura
    putStrLn $ "Fecha y hora: " ++ getFechaFactura factura
    putStrLn "Líneas de la factura:"
    mapM_ mostrarLineaFactura (getArticulosFactura factura)
    putStrLn $ "SubTotal: " ++  show (subtotalFactura factura)
    putStrLn $ "Total: " ++  show (totalFactura factura) ++ "\n"

mostrarLineaFactura :: ArticuloFactura -> IO ()
mostrarLineaFactura linea = do
    putStrLn $ "\tCódigo de artículo: " ++ getCodigoArticuloFactura linea
    putStrLn $ "\tNombre de artículo: " ++ getNombreArticuloFactura linea
    putStrLn $ "\tCantidad: " ++ show (getCantidadArticulosFactura linea)
    putStrLn $ "\tCosto unitario: " ++ show (getCostoArticuloFactura linea)
    putStrLn $ "\tSubtotal: " ++ show (getSubTotalArticuloFactura linea)

facturacion :: [OrdenCompra] -> [Bodega] -> [Usuario] -> Empresa -> [Articulo] -> IO ()
facturacion ordenes bodegas usuarios empresa guarArticulos = do
    putStrLn "Ingrese el ID de la orden de compra: "
    idOrden <- getLine
    case buscarOrdenPorId idOrden ordenes of
        Nothing -> putStrLn "No se encontró ninguna orden de compra con ese ID."
        Just orden -> do
            case obtenerUsuarioPorCedula (read (getCedulaClienteOrdenCompra orden)) usuarios of
                Nothing -> putStrLn "No se encontró el usuario asociado a la orden de compra."
                Just usuario -> do
                    resultadoFacturacion <- facturar orden bodegas usuario empresa guarArticulos
                    case resultadoFacturacion of
                        Just factura -> do
                            putStrLn "La factura se ha generado exitosamente:"
                            anadirFactura factura
                            mostrarFactura factura
                        Nothing -> putStrLn "No se pudo facturar la orden de compra debido a insuficiente stock."

textTostring :: Text -> String
textTostring = T.unpack