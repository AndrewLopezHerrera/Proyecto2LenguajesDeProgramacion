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
    getCodigoArticulo,
    getNombreArticulo,
    getCostoArticulo,
    getTipoArticulo,
    getTipoIVAArticulo,
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
    getTotalArticuloFactura,
    getLineasOrdenCompra,
    getFechaOrdenCompra,
    getNombreClienteOrdenCompra,
    getCedulaClienteOrdenCompra,
    getLineasIngreso
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

getCodigoArticulo :: Articulo -> String
getCodigoArticulo (Articulo codigoArticulo _ _ _ _) = codigoArticulo

getNombreArticulo :: Articulo -> String
getNombreArticulo (Articulo _ nombreArticulo _ _ _) = nombreArticulo

getCostoArticulo :: Articulo -> Double
getCostoArticulo (Articulo _ _ costoArticulo _ _) = costoArticulo

getTipoArticulo :: Articulo -> Tipo
getTipoArticulo (Articulo _ _ _ tipoArticulo _) = tipoArticulo

getTipoIVAArticulo :: Articulo -> TipoIVA
getTipoIVAArticulo (Articulo _ _ _ _ tipoIVAArticulo) = tipoIVAArticulo

data Ingreso = Ingreso { codigoIngreso :: String
                       , idUsuario :: String
                       , fecha :: String
                       , lineasIngreso :: [LineaIngreso]
                       } deriving (Show, Generic)

instance FromJSON Ingreso
instance ToJSON Ingreso

getLineasIngreso :: Ingreso -> [LineaIngreso]
getLineasIngreso (Ingreso _ _ _ lineas) = lineas

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
        idFactura :: String,
        clienteFactura :: Usuario,
        empresaFactura :: Empresa,
        estadoFactura :: String,
        fechaHoraFactura :: String,
        lineasFactura :: [ArticuloFactura],
        subtotalFactura :: Double,
        totalFactura :: Double
    } deriving(Show, Generic)

instance FromJSON Factura
instance ToJSON Factura

getNombreEmpresaFactura :: Factura -> Text
getNombreEmpresaFactura factura = getNombreEmpresa (empresaFactura factura)

getSitioWebEmpresaFactura :: Factura -> Text
getSitioWebEmpresaFactura factura = getSitioWeb (empresaFactura factura)

getContactoEmpresaFactura :: Factura -> Text
getContactoEmpresaFactura factura = getContacto (empresaFactura factura)

getIdFactura :: Factura -> String
getIdFactura = idFactura

getCedulaClienteFactura :: Factura -> Int
getCedulaClienteFactura = getCedula . clienteFactura

getNombreClienteFactura :: Factura -> Text
getNombreClienteFactura = getNombre . clienteFactura

getEstadoFactura :: Factura -> String
getEstadoFactura = estadoFactura

getFechaFactura :: Factura -> UTCTime
getFechaFactura  (Factura _ _ _ _ _ _ _ fecha _) = fecha

getArticulosFactura :: Factura -> [ArticuloFactura]
getArticulosFactura = lineasFactura

data ArticuloFactura =
    ArticuloFactura{
        codigoArticuloFactura :: String,
        nombreArticuloFactura :: String,
        cantidadArticuloFactura :: Int,
        costoArticuloFactura :: Double,
        tipoArticuloFactura :: Tipo,
        tipoIVAArticuloFactura :: TipoIVA,
        subTotalArticuloFactura :: Double
    } deriving (Show, Generic)

instance FromJSON ArticuloFactura
instance ToJSON ArticuloFactura

getCodigoArticuloFactura :: ArticuloFactura -> String
getCodigoArticuloFactura = codigoArticuloFactura

getNombreArticuloFactura :: ArticuloFactura -> String
getNombreArticuloFactura = nombreArticuloFactura

getCostoArticuloFactura :: ArticuloFactura -> Double
getCostoArticuloFactura = costoArticuloFactura

getTipoArticuloFactura :: ArticuloFactura -> Tipo
getTipoArticuloFactura = tipoArticuloFactura

getTipoIVAArticuloFactura :: ArticuloFactura -> TipoIVA
getTipoIVAArticuloFactura = tipoIVAArticuloFactura

getCantidadArticulosFactura :: ArticuloFactura -> Int
getCantidadArticulosFactura = cantidadArticuloFactura

getSubTotalArticuloFactura :: ArticuloFactura -> Double
getSubTotalArticuloFactura = subTotalArticuloFactura

getTotalArticuloFactura :: ArticuloFactura -> Double
getTotalArticuloFactura articulo = subTotalArticuloFactura articulo * (1 + porcentajeIVA)
    where porcentajeIVA = case tipoIVAArticuloFactura articulo of
            REG -> 0.13
            ESP -> 0.04