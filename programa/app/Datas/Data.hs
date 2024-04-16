{-# LANGUAGE DeriveGeneric #-}

module Datas.Data
  ( Empresa(..)
  , Bodega(..)
  , Usuario(..)
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
    } deriving (Show,Generic)

instance FromJSON Bodega
instance ToJSON Bodega

getID :: Bodega -> Int
getID (Bodega id _ _) = id

getCapacidad :: Bodega -> Double
getCapacidad (Bodega _ capacidad _) = capacidad

getUbicacion :: Bodega -> Text
getUbicacion (Bodega _ _ ubicacion) = ubicacion

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