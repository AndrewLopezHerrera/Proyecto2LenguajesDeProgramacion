import System.IO
import Data.List.Split (splitOn)

data Tipo = ENL | GBA | FRU | CON | VEG deriving (Show, Read, Eq)
data TipoIVA = REG | ESP deriving (Show, Read, Eq)
data Articulo = Articulo { codigo :: String
                         , nombre :: String
                         , costo :: Double
                         , tipo :: Tipo
                         , tipoIVA :: TipoIVA
                         } deriving (Show, Eq)

cargarArticulos :: FilePath -> IO [Articulo]
cargarArticulos path = do
    contenido <- readFile path
    let lineas = lines contenido
    return $ map parseArticulo lineas

parseArticulo :: String -> Articulo
parseArticulo linea =
    let [cod, nom, cst, tip, tiv] = splitOn "," linea
        costo' = read cst :: Double
        tipo' = read tip :: Tipo
        tipoIVA' = read tiv :: TipoIVA
    in Articulo cod nom costo' tipo' tipoIVA'

mostrarArticulos :: [Articulo] -> IO ()
mostrarArticulos articulos = mapM_ print articulos


main :: IO ()
main = do
    putStrLn "Ingrese la ruta del archivo de artículos:"
    ruta <- getLine
    articulos <- cargarArticulos ruta
    putStrLn "Artículos cargados:"
    mostrarArticulos articulos
