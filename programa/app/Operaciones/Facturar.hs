module Operaciones.Facturar (
    facturarOrdenCompra,
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
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ByteString (unpack)

{-
Entradas: Una factura
Salidas: Un mensaje de éxito.
-}
anadirFactura :: Factura -> IO ()
anadirFactura nuevaFactura = do
    facturasExistente <- cargarFacturas
    let facturasActualizadas = nuevaFactura : facturasExistente
    guardarFacturas facturasActualizadas
    putStrLn "\nSe ha añadido la factura correctamente."

{-
Entradas: Nada
Salidas: La lista de facturas que se encuentran en el archivo JSON.
-}
cargarFacturas :: IO [Factura]
cargarFacturas = do
  cwd <- getCurrentDirectory
  let direccion = cwd </> "app\\BasesDeDatos\\Facturas.json"
  fileContent <- B.readFile direccion
  case eitherDecode fileContent of
    Left err -> do
      return []
    Right factura -> return [factura]

{-
Entradas: La lista de facturas a guardar.
Salidas: Un mensjae de éxito.
-}
guardarFacturas :: [Factura] -> IO ()
guardarFacturas facturas = do
  let json = encode facturas
  cwd <- getCurrentDirectory
  let direccion = cwd </> "app\\BasesDeDatos\\Facturas.json"
  B.writeFile direccion json
  putStrLn "\nSe ha guardado la factura"

{-
Entradas: El código del artículo. La lista de los artículos.
Salidas: El artículo encontrado
-}
buscarArticulo :: String -> [Articulo] -> Maybe Articulo
buscarArticulo codigo = find (\articulo -> codigoArticulo articulo == codigo)

{-
Entradas: Una lista de tuplas de bodega e ID. Una lista de tuplas de artículos e ID.
Salidas: True si hay suficiente stock, False si no lo hay.
-}
verificarStock :: [(Bodega, Int)] -> [(Articulo, Int)] -> Bool
verificarStock _ [] = True
verificarStock bodegas ((articulo, cantidad):resto) =
    case buscarArticulo (codigoArticulo articulo) (map fst bodegas >>= stock) of
        Just (LineaIngreso _ _ stockArticulo) ->
            let stockTotal = sum $ map snd bodegas
            in ((stockTotal >= cantidad) && verificarStock bodegas resto)
        Nothing -> False

{-
Entradas: Una lista de tuplas de bodega e ID. Una lista de tuplas de artículos e ID.
Salidas: La lista de tuplas bodega e ID actualizado.
-}
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

{-
Entradas: La lista de artículos de la factura.
Salidas: El subtotal de el precio global.
-}
calcularSubtotal :: [ArticuloFactura] -> Double
calcularSubtotal articulos = sum $ map getSubTotalArticuloFactura articulos

{-
Entradas: La lista de artículos.
Salidas: El total de todos los artículos.
-}
calcularTotal :: [ArticuloFactura] -> Double
calcularTotal articulos = sum $ map getTotalArticuloFactura articulos

{-
Entradas: Una lista de las ordenes de compra. La lista de bodegas. La empresa.
Salidas: La información de éxito.
-}
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

{-
Entradas: La orden de compra a facturar. Las bodegas que contienen los artículos. El usuario que
realizará la facturación. La empresas que realiza la facturación.
Salidas: La factura creada.
-}
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

{-
Entradas: La factura creada.
Salidas: La información de la factura.
-}
mostrarFactura :: Factura -> IO ()
mostrarFactura factura = do
    putStrLn "Información de la factura:"
    putStrLn $ "ID: " ++ getIdFactura factura
    putStrLn $ "Cédula del cliente: " ++ show (getCedulaClienteFactura factura)
    putStrLn $ "Nombre del cliente: " ++ textTostring (getNombreClienteFactura factura)
    putStrLn $ "Estado: " ++ getEstadoFactura factura
    putStrLn $ "Fecha y hora: " ++ getFechaFactura factura
    putStrLn "Líneas de la factura:"
    mapM_ mostrarLineaFactura (getArticulosFactura factura)

{-
Entradas: La lista de los artículos de la factura.
Salidas: La información de cada línea.
-}
mostrarLineaFactura :: ArticuloFactura -> IO ()
mostrarLineaFactura linea = do
    putStrLn $ "Código de artículo: " ++ getCodigoArticuloFactura linea
    putStrLn $ "Nombre de artículo: " ++ getNombreArticuloFactura linea
    putStrLn $ "Cantidad: " ++ show (getCantidadArticulosFactura linea)
    putStrLn $ "Costo unitario: " ++ show (getCostoArticuloFactura linea)
    putStrLn $ "Subtotal: " ++ show (getSubTotalArticuloFactura linea)

{-
crearFactura :: [Bodega] -> [Articulo] -> [OrdenCompra] -> [Factura] -> IO ()
crearFactura bodegas articulos ordenesCompra facturas = do
  putStrLn "\t\tFacturación de Ordenes de Compra\n"
  putStr "Escriba el código de la orden de compra a facturar: "
  hFlush stdout
  ioIdOrdenCompra <- getLine
  let idOrdenCompra = pack ioIdOrdenCompra :: Text
  let existeFactura = verificarExistenciaFactura facturas idOrdenCompra 0
  if existeFactura
    then do
      putStrLn "\nLa factura ya existe\n"
    else
      let ordenCompra = verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra 0
       in if length ordenCompra == 0
            then
              putStrLn ("\nNo existe la orden de compra con el código " ++ unpack idOrdenCompra)
            else
              let objetoOrdenCompra = ordenCompra !! 0
               in putStrLn "Se ha creado la factura"

verificarExistenciaFactura :: [Factura] -> Text -> Int -> Bool
verificarExistenciaFactura facturas idFactura indice =
  if length facturas == 0 || length facturas == indice
    then
      False
    else
      let factura = facturas !! indice
          idFacturaGuardada = getIdFactura factura
      in if idFacturaGuardada == (unpack idFactura)
        then
          True
        else
          verificarExistenciaFactura facturas idFactura (indice + 1)

verificarExistenciaOrdenCompra :: [OrdenCompra] -> Text -> Int -> [OrdenCompra]
verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra indice =
  if length ordenesCompra == 0 || length ordenesCompra == indice
    then
      []
    else
      let ordenCompra = ordenesCompra !! indice
          idOrdenCompraString = getIdOrdenCompra (ordenCompra)
          idOrdenCompraGuardado = pack idOrdenCompraString
      in if idOrdenCompraGuardado == idOrdenCompra
        then
          [ordenCompra]
        else
          verificarExistenciaOrdenCompra ordenesCompra idOrdenCompra (indice + 1)
-}
textTostring :: Text -> String
textTostring = T.unpack