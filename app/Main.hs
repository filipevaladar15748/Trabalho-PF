module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)

import DadosExemplo
import Model
import Schedule
import Standings

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

linhaClassificacao :: Classificacao -> String
linhaClassificacao c =
  pNome (participante c) ++ ": " ++ show (pontos c) ++ " pts, "
    ++ show (jogos c) ++ "J " ++ show (vitorias c) ++ "V "
    ++ show (empates c) ++ "E " ++ show (derrotas c) ++ "D, dif "
    ++ show (diferenca c)

descreverTorneio :: Maybe (Torneio r) -> String
descreverTorneio Nothing = "recusado"
descreverTorneio (Just t) =
  show (formato t) ++ " com " ++ show (length (participantes t)) ++ " participantes e "
    ++ show (contarPorJogar (partidas t)) ++ " partidas por jogar"

desenharBracket :: Int -> Bracket -> [String]
desenharBracket n (Folha p) = [replicate (2 * n) ' ' ++ pNome p]
desenharBracket n (Jogo e d) =
  (replicate (2 * n) ' ' ++ "jogo") : desenharBracket (n + 1) e ++ desenharBracket (n + 1) d

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

  titulo "Classificacao com 3 pontos por vitoria"
  mapM_ (putStrLn . linhaClassificacao) (tabela regrasPadrao participantesExemplo partidasExemplo)

  titulo "Classificacao com 2 pontos por vitoria"
  mapM_ (putStrLn . linhaClassificacao) (tabela regrasAlternativas participantesExemplo partidasExemplo)

  titulo "Classificacao com golos"
  mapM_ (putStrLn . linhaClassificacao) (tabela regrasGolos participantesExemplo partidasGolos)

  titulo "Criacao de torneios"
  putStrLn ("Liga com 4: " ++ descreverTorneio (novoTorneio Liga regrasPadrao participantesExemplo))
  putStrLn ("Liga com id repetido: " ++ descreverTorneio (novoTorneio Liga regrasPadrao [ana, bruno, Participante 1 "Outra"]))
  putStrLn ("Eliminatorias com 4: " ++ descreverTorneio (novoTorneio Eliminatorias regrasGolos participantesExemplo))
  putStrLn ("Eliminatorias com 3: " ++ descreverTorneio (novoTorneio Eliminatorias regrasGolos [ana, bruno, carla]))

  titulo "Arvore de eliminatorias"
  case construirBracket participantesExemplo of
    Nothing -> putStrLn "Sem participantes"
    Just b -> do
      mapM_ putStrLn (desenharBracket 0 b)
      putStrLn ("Jogos: " ++ show (jogosBracket b) ++ ", rondas: " ++ show (rondasBracket b))
