{-# LANGUAGE DeriveGeneric #-}

module Datas.Data
  ( Empresa(..)
  , Bodega(..)
  , Usuario(..)
  , Articulo(..)
  , Ingreso(..)
  , OrdenCompra(..)
  , getNombreEmpresa
  , getSitioWeb
  , getContacto
  , getID
  , getCapacidad
  , getUbicacion
  , getCedula
  , getNombre
  , getPrimerApellido
  , getSegundoApellido
  , getPuesto
  ) where

import GHC.Generics
import Data.Aeson
import Data.Text (Text, pack)
import Data.Time

data Empresa =
    Empresa { nombreEmpresa  :: !Text,
    sitioWeb :: !Text,
    contacto :: !Text
    } deriving (Show,Generic)

instance FromJSON Empresa
instance ToJSON Empresa

getNombreEmpresa :: Empresa -> Text
getNombreEmpresa (Empresa nombreEmpresa _ _) = nombreEmpresa

getSitioWeb :: Empresa -> Text
getSitioWeb (Empresa _ sitioWeb _) = sitioWeb

getContacto :: Empresa -> Text
getContacto (Empresa _ _ contacto) = contacto

data Bodega = 
    Bodega {
        id :: Int,
        capacidad :: Double,
        ubicacion :: !Text
        inventario :: [LineaIngreso]
    } deriving (Show,Generic)

instance FromJSON Bodega
instance ToJSON Bodega

getID :: Bodega -> Int
getID (Bodega id _ _ _) = id

getCapacidad :: Bodega -> Double
getCapacidad (Bodega _ capacidad _ _) = capacidad

getUbicacion :: Bodega -> Text
getUbicacion (Bodega _ _ ubicacion _) = ubicacion

getInventario :: Bodega -> [LineaIngreso]
getInventario (Bodega _ _ _ inventario)

data Usuario = 
    Usuario{
        cedula :: Int,
        nombre :: !Text,
        primerApellido :: !Text,
        segundoApellido :: !Text,
        puesto :: !Text
    } deriving (Show,Generic)

instance FromJSON Usuario
instance ToJSON Usuario

getCedula ::  Usuario -> Int
getCedula (Usuario cedula _ _ _ _) = cedula

getNombre :: Usuario -> Text
getNombre (Usuario _ nombre _ _ _) = nombre

getPrimerApellido :: Usuario -> Text
getPrimerApellido (Usuario _ _ primerApellido _ _) = primerApellido

getSegundoApellido :: Usuario -> Text
getSegundoApellido (Usuario _ _ _ segundoApellido _) = segundoApellido

getPuesto :: Usuario -> Text
getPuesto (Usuario _ _ _ _ puesto) = puesto

data Tipo = ENL | GBA | FRU | CON | VEG deriving (Show, Read, Eq, Generic)

instance FromJSON Tipo
instance ToJSON Tipo

data TipoIVA = REG | ESP deriving (Show, Read, Eq, Generic)

instance FromJSON TipoIVA
instance ToJSON TipoIVA

data Articulo = Articulo { codigo :: String
                         , nombre :: String
                         , costo :: Double
                         , tipo :: Tipo
                         , tipoIVA :: TipoIVA
                         } deriving (Show, Eq, Generic)

instance FromJSON Articulo
instance ToJSON Articulo

data Ingreso = Ingreso { codigoIngreso :: String
                       , idUsuario :: String
                       , fecha :: String
                       , lineas :: [LineaIngreso]
                       } deriving (Show)


data LineaIngreso = LineaIngreso { codigoArticulo :: String
                                 , identificadorBodega :: String
                                 , cantidad :: Int
                                 } deriving (Show, Generic)

instance FromJSON LineaIngreso
instance ToJSON LineaIngreso

getCodigoArticuloLineaIngreso :: LineaIngreso -> String
getCodigoArticuloLineaIngreso (LineaIngreso codigoArticulo _ _) = codigoArticulo

getIdentificadorBodegaLineaIngreso :: LineaIngreso -> String
getIdentificadorBodegaLineaIngreso (LineaIngreso _ identificadorBodega _) = identificadorBodega

getCantidadLineaIngreso :: LineaIngreso -> Int
getCantidadLineaIngreso (LineaIngreso _ _ cantidad) = cantidad

data OrdenCompra = OrdenCompra { idOrden :: String
                               , cedulaCliente :: String
                               , nombreCliente :: String
                               , fecha :: String
                               , lineas :: [LineaOrdenCompra]
                               } deriving (Show)

getIdOrdenCompra :: OrdenCompra -> String
getIdOrdenCompra (OrdenCompra id _ _ _ _) = id

getCedulaClienteOrdenCompra :: OrdenCompra -> String
getCedulaClienteOrdenCompra (OrdenCompra _ cedulaCliente _ _ _) = cedulaCliente

getNombreClienteOrdenCompra :: OrdenCompra -> String
getCompra (OrdenCompra _ _ nombreCliente _ _) = nombreCliente

getFechaOrdenCompra :: OrdenCompra -> String
getFechaOrdenCompra (OrdenCompra _ _ _ fecha _) = fecha

getLineasOrdenCompra :: OrdenCompra -> [LineaOrdenCompra]
getLineasOrdenCompra (OrdenCompra _ _ _ _ lineas) = lineas

data LineaOrdenCompra = LineaOrdenCompra { codigoArticulo :: String
                                         , cantidad :: Int
                                         } deriving (Show)

getCodigoArticuloOrdenCompra :: LineaOrdenCompra -> String
getCodigoArticuloOrdenCompra (LineaOrdenCompra codigoArticulo _) = codigoArticulo

getCantidadArticuloOrdenCompra :: LineaOrdenCompra -> Int
getCantidadArticuloOrdenCompra (LineaOrdenCompra _ cantidad) = cantidad

data Factura =
    Factura{
        id :: !Text,
        nombreEmpresa :: !Text,
        sitioWebEmpresa :: !Text,
        contactoEmpresa :: !Text,
        cedulaCliente :: Int,
        nombreCliente :: !Text,
        estado :: !Text,
        fecha :: UTCTime,
        articulos :: [ArticuloFactura]
    }

instance FromJSON Factura
instance ToJSON Factura

getIdFactura :: Factura -> Text
getIdFactura(Factura id _ _ _ _ _ _ _ _) = id

getNombreEmpresaFactura :: Factura -> Text
getNombreEmpresaFactura (Factura _ nombreEmpresa _ _ _ _ _ _ _) = nombreEmpresa

getSitioWebEmpresaFactura :: Factura -> Text
getSitioWebEmpresaFactura (Factura _ _ sitioWebEmpresa _ _ _ _ _ _) = sitioWebEmpresa

getContactoEmpresaFactura :: Factura -> Text
getContactoEmpresaFactura (Factura _ _ _ contactoEmpresa _ _ _ _ _) = contactoEmpresa

getCedulaClienteFactura :: Factura -> Int
getCedulaClienteFactura (Factura _ _ _ _ cedulaCliente _ _ _ _) = cedulaCliente

getNombreClienteFactura :: Factura -> Text
getNombreClienteFactura (Factura _ _ _ _ _ nombreCliente _ _ _) = nombreCliente

getEstadoFactura :: Factura -> Text
getEstadoFactura (Factura _ _ _ _ _ _ estado _ _) = estado

getFechaFactura :: Factura -> UTCTime
getFechaFactura  (Factura _ _ _ _ _ _ _ fecha _) = fecha

getArticulosFactura :: Factura -> [ArticuloFactura]
getArticulosFactura (Factura _ _ _ _ _ _ _ _ articulos) = articulos

data ArticuloFactura =
    ArticuloFactura{
        codigo :: !Text,
        nombre :: !Text,
        costo :: Double,
        tipo :: Tipo,
        tipoIVA :: TipoIVA,
        cantidad :: Int,
        subTotal :: Double,
        total :: Double
    } deriving (Show, Generic)

instance FromJSON ArticuloFactura
instance ToJSON ArticuloFactura

getCodigoArticuloFactura :: ArticuloFactura -> Text
getCodigoArticuloFactura (ArticuloFactura codigoArticulo _ _ _ _ _ _ _) = codigoArticulo

getNombreArticuloFactura :: ArticuloFactura -> Text
getNombreArticuloFactura (ArticuloFactura _ nombre _ _ _ _ _ _) = nombre

getCostoArticuloFactura :: ArticuloFactura -> Double
getCostoArticuloFactura (Articulo _ _ costo _ _ _ _ _) = costo

getTipoArticuloFactura :: ArticuloFactura -> Tipo
getTipoArticuloFactura (ArticuloFactura _ _ _ tipo _ _ _) = Tipo

getTipoIVAArticuloFactura :: ArticuloFactura -> TipoIVA
getTipoIVAArticuloFactura (ArticuloFactura _ _ _ _ tipoIVA _ _ _) = tipoIVA

getCantidadArticulosFactura ::  ArticuloFactura -> Int
getCantidadArticulosFactura (ArticuloFactura _ _ _ _ _ cantidad _ _) = cantidad

getSubTotalArticuloFactura :: ArticuloFactura -> Double
getSubTotalArticuloFactura (ArticuloFactura _ _ _ _ _ _ subTotal _) = subTotal

getTotalArticuloFactura :: ArticuloFactura -> Double
getTotalArticuloFactura (ArticuloFactura _ _ _ _ _ _ _ total) = total