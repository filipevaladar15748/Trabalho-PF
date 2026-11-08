module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)

import DadosExemplo
import Model
import Schedule

titulo :: String -> IO ()
titulo t = do
  putStrLn ""
  putStrLn ("== " ++ t ++ " ==")

textoResultado :: Maybe Resultado -> String
textoResultado Nothing = "por jogar"
textoResultado (Just VenceCasa) = "vence a casa"
textoResultado (Just VenceFora) = "vence o de fora"
textoResultado (Just Empate) = "empate"

textoGolos :: Maybe Golos -> String
textoGolos Nothing = "por jogar"
textoGolos (Just (Golos a b)) = show a ++ "-" ++ show b

linhaPartida :: (Maybe r -> String) -> Partida r -> String
linhaPartida mostrar p = pNome (casa p) ++ " vs " ++ pNome (fora p) ++ ": " ++ mostrar (resultado p)

linhaConfronto :: (Participante, Participante) -> String
linhaConfronto (a, b) = pNome a ++ " vs " ++ pNome b

linhaContagem :: [Partida r] -> String
linhaContagem ps = "Jogadas: " ++ show (contarJogadas ps) ++ ", por jogar: " ++ show (contarPorJogar ps)

textoRegisto :: Maybe [Partida r] -> String
textoRegisto Nothing = "recusado"
textoRegisto (Just ps) = "aceite (" ++ show (contarJogadas ps) ++ " jogadas)"

main :: IO ()
main = do
  hSetEncoding stdout utf8

  titulo "Calendario"
  mapM_ (putStrLn . linhaConfronto) (todosContraTodos participantesExemplo)

  titulo "Partidas com vitoria, empate ou derrota"
  mapM_ (putStrLn . linhaPartida textoResultado) partidasExemplo
  putStrLn (linhaContagem partidasExemplo)

  titulo "Partidas com golos"
  mapM_ (putStrLn . linhaPartida textoGolos) partidasGolos
  putStrLn (linhaContagem partidasGolos)

  titulo "Registo de resultados"
  putStrLn ("Bruno vs Diogo, empate: " ++ textoRegisto (registarResultado 2 4 Empate partidasExemplo))
  putStrLn ("Ana vs Bruno outra vez: " ++ textoRegisto (registarResultado 1 2 VenceFora partidasExemplo))
  putStrLn ("Bruno vs Diogo, 1-2: " ++ textoRegisto (registarResultado 2 4 (Golos 1 2) partidasGolos))
