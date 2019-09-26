-- Mauro Hemerly (Hämmerli) Gazzani
-- Disciplina de Programação Funcional
-- Curso de Engenharia de Computação
-- UEMG - Unidade de Ituiutaba

module PontoFlutuante  where
import Prelude hiding (repeat)

-- Laço sem fim provocado pela comparação de diferente em ponto flutuante
repeat :: Double -> Double -> IO()
repeat inicio fim = do
          let y = inicio + 0.1
          print y
          if (y /= fim) then (repeat y fim) else print y


-- Solução genérica para evitar laço sem fim em uma comparação
--    de ponto flutuante no interior deste laço.
repeat' :: Double -> Double -> IO()
repeat' inicio fim = do
          let y = inicio + 0.1
          print y
          if (abs(y - fim) > 0.0000001) then (repeat' y fim) else print y