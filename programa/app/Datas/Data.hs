{-# LANGUAGE DeriveGeneric #-}

module Datas.Data (
    Empresa(..),
    getNombreEmpresa,
    getSitioWeb,
    getContacto,
    Bodega(..),
    getID,
    getCapacidad,
    getUbicacion,
    getStock,
    Usuario(..),
    getCedula,
    getNombre,
    getPrimerApellido,
    getSegundoApellido,
    getPuesto,
    Tipo(..),
    TipoIVA(..),
    Articulo(..),
    Ingreso(..),
    LineaIngreso(..),
    OrdenCompra(..),
    LineaOrdenCompra(..),
    Factura(..),
    ArticuloFactura(..),
    getCodigoArticuloLineaIngreso,
    getIdentificadorBodegaLineaIngreso,
    getCantidadLineaIngreso,
    getCodigoArticuloOrdenCompra,
    getCantidadArticuloOrdenCompra,
    getIdFactura,
    getNombreEmpresaFactura,
    getSitioWebEmpresaFactura,
    getContactoEmpresaFactura,
    getIdOrdenCompra,
    getCedulaClienteFactura,
    getNombreClienteFactura,
    getEstadoFactura,
    getFechaFactura,
    getArticulosFactura,
    getCodigoArticuloFactura,
    getNombreArticuloFactura,
    getCostoArticuloFactura,
    getTipoArticuloFactura,
    getTipoIVAArticuloFactura,
    getCantidadArticulosFactura,
    getSubTotalArticuloFactura,
    getTotalArticuloFactura
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
        idBodega :: Int,
        capacidad :: Double,
        ubicacion :: !Text,
        stock :: [LineaIngreso]
    } deriving (Show,Generic)

instance FromJSON Bodega
instance ToJSON Bodega

getID :: Bodega -> Int
getID (Bodega id _ _ _) = id

getCapacidad :: Bodega -> Double
getCapacidad (Bodega _ capacidad _ _) = capacidad

getUbicacion :: Bodega -> Text
getUbicacion (Bodega _ _ ubicacion _) = ubicacion

getStock :: Bodega -> [LineaIngreso]
getStock (Bodega _ _ _ stock) = stock

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

data Articulo = Articulo { codigoArticulo :: String
                         , nombreArticulo :: String
                         , costoArticulo :: Double
                         , tipoArticulo :: Tipo
                         , tipoIVAArticulo :: TipoIVA
                         } deriving (Show, Eq, Generic)

instance FromJSON Articulo
instance ToJSON Articulo

data Ingreso = Ingreso { codigoIngreso :: String
                       , idUsuario :: String
                       , fecha :: String
                       , lineasIngreso :: [LineaIngreso]
                       } deriving (Show)


data LineaIngreso = LineaIngreso { codigoLineaIngreso :: String
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
                               , cedulaClienteOrden :: String
                               , nombreClienteOrden :: String
                               , fechaOrden :: String
                               , lineasCompra :: [LineaOrdenCompra]
                               } deriving (Show, Generic)

instance ToJSON OrdenCompra
instance FromJSON OrdenCompra

getIdOrdenCompra :: OrdenCompra -> String
getIdOrdenCompra (OrdenCompra id _ _ _ _) = id

getCedulaClienteOrdenCompra :: OrdenCompra -> String
getCedulaClienteOrdenCompra (OrdenCompra _ cedulaCliente _ _ _) = cedulaCliente

getNombreClienteOrdenCompra :: OrdenCompra -> String
getNombreClienteOrdenCompra (OrdenCompra _ _ nombreCliente _ _) = nombreCliente

getFechaOrdenCompra :: OrdenCompra -> String
getFechaOrdenCompra (OrdenCompra _ _ _ fecha _) = fecha

getLineasOrdenCompra :: OrdenCompra -> [LineaOrdenCompra]
getLineasOrdenCompra (OrdenCompra _ _ _ _ lineas) = lineas

data LineaOrdenCompra = LineaOrdenCompra { codigoLineaOrden :: String
                                         , cantidadLineaOrden :: Int
                                         } deriving (Show, Generic)

instance ToJSON LineaOrdenCompra
instance FromJSON LineaOrdenCompra

getCodigoArticuloOrdenCompra :: LineaOrdenCompra -> String
getCodigoArticuloOrdenCompra (LineaOrdenCompra codigoArticulo _) = codigoArticulo

getCantidadArticuloOrdenCompra :: LineaOrdenCompra -> Int
getCantidadArticuloOrdenCompra (LineaOrdenCompra _ cantidad) = cantidad

data Factura =
    Factura{
        idFactura :: !Text,
        nombreEmpresaFactura :: !Text,
        sitioWebEmpresaFactura:: !Text,
        contactoEmpresaFactura :: !Text,
        cedulaClienteFactura :: Int,
        nombreClienteFactura :: !Text,
        estadoFactura :: !Text,
        fechaFactura :: UTCTime,
        articulosFactura :: [ArticuloFactura]
    } deriving(Generic, Show)

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
        codigoArticuloFactura :: !Text,
        nombreArticuloFactura :: !Text,
        costoArticuloFactura :: Double,
        tipoArticuloFactura :: Tipo,
        tipoIVAArticuloFactura :: TipoIVA,
        cantidadArticuloFactura :: Int,
        subTotalArticuloFactura :: Double,
        totalArticuloFactura :: Double
    } deriving (Show, Generic)

instance FromJSON ArticuloFactura
instance ToJSON ArticuloFactura

getCodigoArticuloFactura :: ArticuloFactura -> Text
getCodigoArticuloFactura (ArticuloFactura codigoArticulo _ _ _ _ _ _ _) = codigoArticulo

getNombreArticuloFactura :: ArticuloFactura -> Text
getNombreArticuloFactura (ArticuloFactura _ nombre _ _ _ _ _ _) = nombre

getCostoArticuloFactura :: ArticuloFactura -> Double
getCostoArticuloFactura (ArticuloFactura _ _ costo _ _ _ _ _) = costo

getTipoArticuloFactura :: ArticuloFactura -> Tipo
getTipoArticuloFactura (ArticuloFactura _ _ _ tipo _ _ _ _) = tipo

getTipoIVAArticuloFactura :: ArticuloFactura -> TipoIVA
getTipoIVAArticuloFactura (ArticuloFactura _ _ _ _ tipoIVA _ _ _) = tipoIVA

getCantidadArticulosFactura ::  ArticuloFactura -> Int
getCantidadArticulosFactura (ArticuloFactura _ _ _ _ _ cantidad _ _) = cantidad

getSubTotalArticuloFactura :: ArticuloFactura -> Double
getSubTotalArticuloFactura (ArticuloFactura _ _ _ _ _ _ subTotal _) = subTotal

getTotalArticuloFactura :: ArticuloFactura -> Double
getTotalArticuloFactura (ArticuloFactura _ _ _ _ _ _ _ total) = total