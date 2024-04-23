module Operaciones.Facturar
  ( cargarFacturas,
    guardarFacturas,
    crearFactura,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, pack, unpack)
import Datas.Data
import GHC.Generics
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO
import Text.Read (readMaybe)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)

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

buscarArticulo :: String -> [Articulo] -> Maybe Articulo
buscarArticulo codigo = find (\articulo -> codigoArticulo articulo == codigo)

verificarStock :: [(Bodega, Int)] -> [(Articulo, Int)] -> Bool
verificarStock _ [] = True
verificarStock bodegas ((articulo, cantidad):resto) =
    case buscarArticulo (codigoArticulo articulo) (map fst bodegas >>= stock) of
        Just (LineaIngreso _ _ stockArticulo) ->
            let stockTotal = sum $ map snd bodegas
            in ((stockTotal >= cantidad) && verificarStock bodegas resto)
        Nothing -> False

descontarStock :: [(Bodega, Int)] -> [(Articulo, Int)] -> [(Bodega, Int)]
descontarStock bodegas [] = bodegas
descontarStock bodegas ((articulo, cantidad):resto) =
    let (bodega, stockBodega) = head bodegas
        Just (LineaIngreso _ _ stockArticulo) = buscarArticulo (codigoArticulo articulo) (stock bodega)
        cantidadDescontada = min cantidad stockBodega
        nuevoStock = stockBodega - cantidadDescontada
        bodegaActualizada = bodega { stock = filter (\linea -> getCodigoArticuloLineaIngreso linea /= codigoArticulo articulo) (stock bodega) }
    in if cantidadDescontada > 0
        then descontarStock ((bodegaActualizada, stockBodega - cantidadDescontada) : tail bodegas) ((articulo, cantidad - cantidadDescontada) : resto)
        else descontarStock (tail bodegas) ((articulo, cantidad) : resto)

calcularSubtotal :: [ArticuloFactura] -> Double
calcularSubtotal articulos = sum $ map getSubTotalArticuloFactura articulos

calcularTotal :: [ArticuloFactura] -> Double
calcularTotal articulos = sum $ map getTotalArticuloFactura articulos

facturarOrdenCompra :: [OrdenCompra] -> [Bodega] -> Empresa -> IO ()
facturarOrdenCompra ordenesCompra bodegas empresa = do
    putStr "Ingrese el código de la orden de compra: "
    hFlush stdout
    codigoOrdenCompra <- getLine
    putStrLn $ "Facturando orden de compra con código: " ++ codigoOrdenCompra
    let maybeOrden = find (\orden -> getIdOrdenCompra orden == codigoOrdenCompra) ordenesCompra
    case maybeOrden of
        Just orden -> do
            let usuario = obtenerUsuarioPorCedula (getCedulaClienteOrdenCompra orden) usuarios
            case usuario of
                Just u -> do
                    facturas <- cargarFacturas
                    let (newFactura, newBodegas) = fromMaybe (Nothing, bodegas) $ facturar codigoOrdenCompra orden bodegas u empresa
                    case newFactura of
                        Just factura -> do
                            guardarFacturas (factura : facturas)
                            putStrLn "La orden de compra ha sido facturada exitosamente."
                        Nothing -> putStrLn "No fue posible facturar la orden de compra debido a la falta de stock."
                Nothing -> putStrLn "No se encontró ningún usuario asociado a la orden de compra."
        Nothing -> putStrLn "No se encontró ninguna orden de compra con el código especificado."

facturar :: OrdenCompra -> [Bodega] -> Usuario -> Empresa -> IO (Maybe Factura)
facturar ordenCompra bodegas usuario empresa = do
    let lineasOrdenCompra = lineasCompra ordenCompra
    let articulos = map (\linea -> (fromMaybe "" (getCodigoArticuloOrdenCompra linea), getCantidadArticuloOrdenCompra linea)) lineasOrdenCompra
    let bodegasConStock = filter (\bodega -> verificarStock [(bodega, getCapacidad bodega)] articulos) bodegas
    if null bodegasConStock
        then do
            putStrLn "No hay suficiente stock para facturar esta orden de compra."
            return Nothing
        else do
            let bodegasActualizadas = descontarStock (map (\bodega -> (bodega, getCapacidad bodega)) bodegasConStock) articulos
            idFactura <- "FACT_" ++ idOrden ordenCompra
            tiempoActual <- show <$> getPOSIXTime
            let lineasFactura = map (\(articulo, cantidad) ->
                    ArticuloFactura
                        (codigoArticulo articulo)
                        (nombreArticulo articulo)
                        cantidad
                        (costoArticulo articulo)
                        (tipoArticulo articulo)
                        (tipoIVAArticulo articulo)
                        (fromIntegral cantidad * costoArticulo articulo)
                    ) articulos
            let subtotal = calcularSubtotal lineasFactura
            let total = calcularTotal lineasFactura
            return $ Just $ Factura idFactura usuario empresa "Activo" tiempoActual lineasFactura subtotal total

mostrarFactura :: Factura -> IO ()
mostrarFactura factura = do
    putStrLn "Información de la factura:"
    putStrLn $ "ID: " ++ getIdFactura factura
    putStrLn $ "Cédula del cliente: " ++ getCedulaClienteFactura factura
    putStrLn $ "Nombre del cliente: " ++ getNombreClienteFactura factura
    putStrLn $ "Estado: " ++ getEstadoFactura factura
    putStrLn $ "Fecha y hora: " ++ getFechaFactura factura
    putStrLn "Líneas de la factura:"
    mapM_ mostrarLineaFactura (getArticulosFactura factura)

mostrarLineaFactura :: ArticuloFactura -> IO ()
mostrarLineaFactura linea = do
    putStrLn $ "Código de artículo: " ++ getCodigoArticuloFactura linea
    putStrLn $ "Nombre de artículo: " ++ getNombreArticuloFactura linea
    putStrLn $ "Cantidad: " ++ show (getCantidadArticulosFactura linea)
    putStrLn $ "Costo unitario: " ++ show (getCostoArticuloFactura linea)
    putStrLn $ "Subtotal: " ++ show (getSubTotalArticuloFactura linea)