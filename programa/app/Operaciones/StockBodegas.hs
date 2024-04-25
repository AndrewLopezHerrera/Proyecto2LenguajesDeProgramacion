module Operaciones.StockBodegas(
    actualizarBodegas,
    verStockBodegas
) where

import System.IO
import Datas.Data
import Text.Printf

{-
Entradas: Los artículos que se desean actualizar. Las bodegas.
Salidas: Las bodegas actualizadas.
-}
actualizarBodegas :: [LineaIngreso] -> [Bodega] -> [Bodega]
actualizarBodegas [] bodegas = bodegas
actualizarBodegas (linea:lineas) bodegas =
    let idBodega = identificadorBodega linea
        bodegasActualizadas = actualizarBodega (read idBodega) linea bodegas
    in actualizarBodegas lineas bodegasActualizadas

{-
Entradas: El ID de la bodega. La linea que se desea actualizar. La lista de bodegas.
Salidas: La lista de bodegas actualizada.
-}
actualizarBodega :: Int -> LineaIngreso -> [Bodega] -> [Bodega]
actualizarBodega _ _ [] = []
actualizarBodega idBodega nuevaLinea (bodega:bodegas) =
    if idBodega == getID bodega then
        let stockActualizado = actualizarStock (stock bodega) nuevaLinea
        in bodega { stock = stockActualizado } : bodegas
    else
        bodega : actualizarBodega idBodega nuevaLinea bodegas

{-
Entradas: La lista de artículos a actualizar. La nueva línea a agregar.
Salidas: La lista de artículos actualizada.
-}
actualizarStock :: [LineaIngreso] -> LineaIngreso -> [LineaIngreso]
actualizarStock [] nuevaLinea = [nuevaLinea]
actualizarStock (l:ls) nuevaLinea =
    if getCodigoArticuloLineaIngreso l == getCodigoArticuloLineaIngreso nuevaLinea then
        l { cantidad = cantidad l + cantidad nuevaLinea } : ls
    else
        l : actualizarStock ls nuevaLinea

{-
Entradas: La lista de bodegas.
Salidas: La información del stock.
-}
verStockBodegas :: [Bodega] -> IO ()
verStockBodegas [] = putStrLn "No hay bodegas disponibles."
verStockBodegas bodegas = mapM_ mostrarStockBodega bodegas

{-
Entradas: Una bodega
Salidas: La información del stock que se encuentra en esa bodega.
-}
mostrarStockBodega :: Bodega -> IO ()
mostrarStockBodega bodega = do
    putStrLn "==============================="
    printf "ID de Bodega: %d\n" (idBodega bodega)
    printf "Capacidad: %.2f\n" (capacidad bodega)
    printf "Ubicación: %s\n" (show $ ubicacion bodega)
    putStrLn "Stock de la Bodega:"
    mapM_ mostrarLineaStock (stock bodega)
    putStrLn ""

{-
Entradas: Un artículo
Salidas: La información de ese stock.
-}
mostrarLineaStock :: LineaIngreso -> IO ()
mostrarLineaStock linea = putStrLn $ "Codigo: " ++ getCodigoArticuloLineaIngreso linea ++ ", Cantidad: " ++ show (cantidad linea)
