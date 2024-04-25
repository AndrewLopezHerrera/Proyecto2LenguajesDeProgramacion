module Operaciones.OpcionesGenerales
  (ejecutarMenuOpcionesGenerales)
where

import Data.Text (Text, pack)
import Text.Printf
import System.IO
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Inicio.InformacionUsuarios
import Operaciones.CrearOrdenCompra
import Operaciones.CargarMostrarArticulos
import Operaciones.Facturar
import Datas.Data
import Data.List
import Data.Maybe (Maybe(Nothing),isJust, fromJust)

{-
Entradas: Nada
Salidas: Nada
-}
ejecutarMenuOpcionesGenerales :: IO ()
ejecutarMenuOpcionesGenerales = do
    bodegas <- cargarDatosBodega
    facturas <- cargarFacturas
    articulos <- cargarArticulosDesdeJSON
    ordenesCompra <- cargarOrdenesDesdeJSON
    imprimirMenuOpcionesGenerales
    opcion <- getLine
    case opcion of
        "1" -> do consultarOrdenCompra
                  ejecutarMenuOpcionesGenerales
        "2" -> do consultarFactura
                  ejecutarMenuOpcionesGenerales
        "3" -> do nuevabodegas <- retornarMercaderia facturas bodegas
                  guardarBodegas (snd nuevabodegas)
                  ejecutarMenuOpcionesGenerales
        "4" -> putStrLn "\nVolviendo..."
        _ -> do putStrLn "\nOpción incorrecta.\n"
                ejecutarMenuOpcionesGenerales

{-
Entradas: Nada
Salidas: Nada
-}
imprimirMenuOpcionesGenerales :: IO()
imprimirMenuOpcionesGenerales = do
    putStr "\t\tOpciones Generales\n\t1. Consultar orden compra\n\t2. Consultar factura"
    hFlush stdout
    putStr "\n\t3. Retornar mercadería\n\t4. Volver\nSeleccione un opción"
    hFlush stdout

{-
Entradas: Nada
Salidas: La información de la orden de compra.
-}
consultarOrdenCompra :: IO ()
consultarOrdenCompra = do
    putStrLn "Ingrese el ID de la orden de compra a consultar:"
    idOrden <- getLine
    ordenes <- cargarOrdenesDesdeJSON
    let orden = find (\orden -> getIdOrdenCompra orden == idOrden) ordenes
    case orden of
        Just ord -> mostrarOrdenCompra ord
        Nothing -> putStrLn $ "No se encontró ninguna orden de compra con el ID '" ++ idOrden ++ "'."

{-
Entradas: Nada
Salidas: La información de la factura.
-}
consultarFactura :: IO ()
consultarFactura = do
    putStrLn "Ingrese el ID de la factura a consultar:"
    idFactura <- getLine
    facturas <- cargarFacturas
    let factura = find (\factura -> getIdFactura factura == idFactura) facturas
    case factura of
        Just fac -> mostrarFactura fac
        Nothing -> putStrLn $ "No se encontró ninguna factura con el ID '" ++ idFactura ++ "'."

{-
Entradas: La lista de facturas
Salidas: Una tupla con la lista de facturas y la bodegas actualizadas.
-}
retornarMercaderia :: [Factura] -> [Bodega] -> IO ([Factura], [Bodega])
retornarMercaderia facturas bodegas = do
    putStr "Ingrese el código de la factura: "
    hFlush stdout
    cFactura <- getLine
    let factura = find (\f -> cFactura == idFactura f && estadoFactura f == "Activa") facturas
    if isJust factura
        then do
            let facturaAnulada = facturaAnulada { estadoFactura = "Anulada" }
                bodegasActualizadas = actualizarStock (getArticulosFactura facturaAnulada) bodegas
            return (facturaAnulada : filter (\f -> idFactura f /= idFactura facturaAnulada) facturas, bodegasActualizadas)
        else do
            putStrLn "La factura ingresada no existe o no está activa."
            retornarMercaderia facturas bodegas

{-
Entradas: La lista de artículos. La lista de bodegas.
Salidas: Las bodegas actualizadas.
-}
actualizarStock :: [ArticuloFactura] -> [Bodega] -> [Bodega]
actualizarStock [] bodegas = bodegas
actualizarStock (articulo:articulos) bodegas =
    case findBodegaDeArticulo (pack (getCodigoArticuloFactura articulo)) bodegas of
        Just bodega -> let stockActualizado = sumarStock (pack (codigoArticuloFactura articulo)) (cantidadArticuloFactura articulo) (stock bodega)
                       in bodega { stock = stockActualizado } : actualizarStock articulos bodegas
        Nothing -> actualizarStock articulos bodegas

{-
Entradas: El código del artículo a buscar. La lista de bodegas.
Salidas: La bodega en la que se encuentra la bodega.
-}
findBodegaDeArticulo :: Text -> [Bodega] -> Maybe Bodega
findBodegaDeArticulo codigoArticulo [] = Nothing
findBodegaDeArticulo codigoArticulo (bodega:bodegas) =
    if any (\linea -> codigoArticulo == (pack (codigoLineaIngreso linea))) (stock bodega)
        then Just bodega
        else findBodegaDeArticulo codigoArticulo bodegas

{-
Entradas: El código del artículo a retornar. La cantidad a retornar. La lista de los artículos a
retornar.
Salidas: Los artículos actualizados.
-}
sumarStock :: Text -> Int -> [LineaIngreso] -> [LineaIngreso]
sumarStock codigoArticulo cantidad [] = [LineaIngreso (textTostring codigoArticulo) "" cantidad]
sumarStock codigoArticulo cantidad (linea:lineas) =
    if codigoArticulo == (pack (getCodigoArticuloLineaIngreso linea))
        then linea { cantidad = cantidad + getCantidadLineaIngreso linea } : lineas
        else linea : sumarStock codigoArticulo cantidad lineas