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
import Data.Time.Format
import Data.Time.Clock
import System.Locale
import Inicio.InformacionComercial
import Inicio.InformacionBodegas
import Prelude (putStrLn)

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

crearFactura :: Empresa -> [Bodega] -> IO ()
crearFactura = do
  articulos <- cargarArticulosDesdeJSON
  ordenesCompra <- cargarOrdenesDesdeJSON
  facturas <- cargarFacturas
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
              let
                ordenCompra = head ordenCompra
                id = getIdOrdenCompra ordenCompra
                nombreEmpresa = getNombreEmpresa empresa
                sitioWeb = getSitioWeb empresa
                contacto = getContacto empresa
                cedulaCliente = getCedulaClienteOrdenCompra ordenCompra
                nombreCliente = getNombreClienteOrdenCompra ordenCompra
                estado = pack "Activo"
                fecha = pack getFechaOrdenCompra ordenCompra
              in
                putStrLn "Se ha creado la factura"

contarArticulos :: [Bodega] -> String -> Int -> Int
contarArticulos bodegas codigo indice =
  if length bodegas == indice then
    0
  else
    let
      bodega = bodegas !! indice
      lineaIngreso = getStock bodega
      cantidad = contarArticulosAux lineaIngreso codigo 0
    in
      cantidad + contarArticulos bodegas codigo (indice + 1)

contarArticulosAux :: [LineaIngreso] -> String -> Int -> Int
contarArticulosAux articulos codigo indice =
  if length articulos == indice then
    0
  else
    let
      articulo = articulos !! indice
      codigoGuardado = getCodigoArticuloLineaIngreso articulo
      cantidad = getCantidadLineaIngreso articulo
    in
      if codigoGuardado == codigo then
        cantidad
      else
        contarArticulosAux articulos codigo (indice + 1)

buscarArticulo :: [Artículo] -> String -> Int -> Articulo
buscarArticulo articulos codigoArticulo indice =
  if length articulos == indice then
    []
  else
    let
      articulo = articulos indice
      codigoArticuloGuardado = getCodigoArticuloOrdenCompra articulo
    in
      if codigoArticuloGuardado == codigoArticulo then
        [articulo]
      else
        buscarArticulo articulos codigoArticulo (indice + 1)


verificarExistenciaFactura :: [Factura] -> Text -> Int -> Bool
verificarExistenciaFactura facturas idFactura indice =
  if length facturas == 0 || length facturas == indice
    then
      False
    else
      let factura = facturas !! indice
          idFacturaGuardada = getIdFactura factura
      in if idFacturaGuardada == idFactura
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
