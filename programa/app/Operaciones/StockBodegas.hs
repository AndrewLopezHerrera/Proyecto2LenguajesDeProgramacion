module Operaciones.StockBodegas(
    actualizarBodegas,
    verStockBodegas
) where

import System.IO
import Datas.Data

actualizarBodegas :: [LineaIngreso] -> [Bodega] -> [Bodega]
actualizarBodegas [] bodegas = bodegas
actualizarBodegas (linea:lineas) bodegas =
    let idBodega = identificadorBodega linea
        bodegasActualizadas = actualizarBodega (read idBodega) linea bodegas
    in actualizarBodegas lineas bodegasActualizadas

actualizarBodega :: Int -> LineaIngreso -> [Bodega] -> [Bodega]
actualizarBodega _ _ [] = []
actualizarBodega idBodega nuevaLinea (bodega:bodegas) =
    if idBodega == getID bodega then
        let stockActualizado = actualizarStock (stock bodega) nuevaLinea
        in bodega { stock = stockActualizado } : bodegas
    else
        bodega : actualizarBodega idBodega nuevaLinea bodegas

actualizarStock :: [LineaIngreso] -> LineaIngreso -> [LineaIngreso]
actualizarStock [] nuevaLinea = [nuevaLinea]
actualizarStock (l:ls) nuevaLinea =
    if getCodigoArticuloLineaIngreso l == getCodigoArticuloLineaIngreso nuevaLinea then
        l { cantidad = cantidad l + cantidad nuevaLinea } : ls
    else
        l : actualizarStock ls nuevaLinea

verStockBodegas :: [Bodega] -> IO ()
verStockBodegas [] = putStrLn "No hay bodegas disponibles."
verStockBodegas bodegas = mapM_ mostrarStockBodega bodegas

mostrarStockBodega :: Bodega -> IO ()
mostrarStockBodega bodega = do
    putStrLn $ "Bodega\nID: " ++ show (idBodega bodega)
    putStrLn "Stock:"
    mapM_ mostrarLineaStock (stock bodega)
    putStrLn ""

mostrarLineaStock :: LineaIngreso -> IO ()
mostrarLineaStock linea = putStrLn $ "Codigo: " ++ getCodigoArticuloLineaIngreso linea ++ ", Cantidad: " ++ show (cantidad linea)
