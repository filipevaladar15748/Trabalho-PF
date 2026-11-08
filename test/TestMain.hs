module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import DadosExemplo
import Model
import Schedule
import Standings

type Teste = (String, Bool)

semAutoconfronto :: [(Participante, Participante)] -> Bool
semAutoconfronto [] = True
semAutoconfronto ((a, b) : ps) = pId a /= pId b && semAutoconfronto ps

nomes :: [Partida r] -> [(String, String)]
nomes [] = []
nomes (p : ps) = (pNome (casa p), pNome (fora p)) : nomes ps


testesParticipantes :: [Teste]
testesParticipantes =
  [ ("procurarParticipante existente", fmap pNome (procurarParticipante 3 participantesExemplo) == Just "Carla")
  , ("procurarParticipante inexistente", procurarParticipante 9 participantesExemplo == Nothing)
  , ("procurarParticipante lista vazia", procurarParticipante 1 [] == Nothing)
  , ("idsDistintos nos exemplos", idsDistintos participantesExemplo)
  , ("idsDistintos com repetido", not (idsDistintos [ana, bruno, Participante 1 "Outro"]))
  , ("idsDistintos lista vazia", idsDistintos [])
  ]

testesCalendario :: [Teste]
testesCalendario =
  [ ("calendario de 4 tem 6 partidas", length (todosContraTodos participantesExemplo) == 6)
  , ("calendario de 2 tem 1 partida", length (todosContraTodos [ana, bruno]) == 1)
  , ("calendario de 1 e vazio", null (todosContraTodos [ana]))
  , ("calendario vazio", null (todosContraTodos []))
  , ("calendario sem autoconfrontos", semAutoconfronto (todosContraTodos participantesExemplo))
  , ("calendario primeira partida", fmap (\(a, b) -> (pNome a, pNome b)) (take 1 (todosContraTodos participantesExemplo)) == [("Ana", "Bruno")])
  , ("numeroPartidas 4", numeroPartidas 4 == 6)
  , ("numeroPartidas 0", numeroPartidas 0 == 0)
  , ("numeroPartidas 1", numeroPartidas 1 == 0)
  , ("numeroPartidas coincide com o calendario", length (todosContraTodos participantesExemplo) == numeroPartidas (length participantesExemplo))
  ]

testesPartidas :: [Teste]
testesPartidas =
  [ ("criarPartidas todas por jogar", contarPorJogar (criarPartidas (todosContraTodos participantesExemplo) :: [Partida Resultado]) == 6)
  , ("criarPartidas nenhuma jogada", contarJogadas (criarPartidas (todosContraTodos participantesExemplo) :: [Partida Golos]) == 0)
  , ("contarJogadas dos exemplos", contarJogadas partidasExemplo == 4)
  , ("contarPorJogar dos exemplos", contarPorJogar partidasExemplo == 2)
  , ("contarJogadas com golos", contarJogadas partidasGolos == 4)
  , ("contarJogadas lista vazia", contarJogadas ([] :: [Partida Resultado]) == 0)
  , ("partidaJogada sem resultado", not (partidaJogada (Partida ana bruno (Nothing :: Maybe Resultado))))
  , ("partidaJogada com resultado", partidaJogada (Partida ana bruno (Just Empate)))
  , ("partidaJogada com golos", partidaJogada (Partida ana bruno (Just (Golos 0 0))))
  , ("jogosDe Ana tem 3", length (jogosDe 1 partidasExemplo) == 3)
  , ("jogosDe inexistente", null (jogosDe 9 partidasExemplo))
  , ("registarResultado valido", fmap contarJogadas (registarResultado 2 4 Empate partidasExemplo) == Just 5)
  , ("registarResultado ja jogada", registarResultado 1 2 VenceFora partidasExemplo == Nothing)
  , ("registarResultado ordem trocada", registarResultado 4 2 Empate partidasExemplo == Nothing)
  , ("registarResultado inexistente", registarResultado 8 9 Empate partidasExemplo == Nothing)
  , ("registarResultado lista vazia", registarResultado 1 2 Empate [] == Nothing)
  , ("registarResultado preserva ordem", fmap nomes (registarResultado 2 4 Empate partidasExemplo) == Just (nomes partidasExemplo))
  , ("registarResultado com golos", fmap contarJogadas (registarResultado 3 4 (Golos 1 0) partidasGolos) == Just 5)
  , ("registarResultado com golos ja jogada", registarResultado 1 2 (Golos 0 1) partidasGolos == Nothing)
  , ("novoTorneio sem partidas jogadas", contarJogadas (partidas (novoTorneio participantesExemplo :: Torneio Resultado)) == 0)
  , ("novoTorneio com 6 por jogar", contarPorJogar (partidas (novoTorneio participantesExemplo :: Torneio Resultado)) == 6)
  ]

nomesTabela :: [Classificacao] -> [String]
nomesTabela [] = []
nomesTabela (c : cs) = pNome (participante c) : nomesTabela cs

linha :: Participante -> Int -> Int -> Classificacao
linha p pts dif = Classificacao p pts 0 0 0 0 dif

testesPontuacao :: [Teste]
testesPontuacao =
  [ ("pontosVVE vence a casa", pontosVVE 3 1 VenceCasa == (3, 0))
  , ("pontosVVE vence o de fora", pontosVVE 3 1 VenceFora == (0, 3))
  , ("pontosVVE empate", pontosVVE 3 1 Empate == (1, 1))
  , ("pontosVVE outras regras", pontosVVE 2 1 VenceCasa == (2, 0))
  , ("saldoVVE vence a casa", saldoVVE VenceCasa == 1)
  , ("saldoVVE vence o de fora", saldoVVE VenceFora == -1)
  , ("saldoVVE empate", saldoVVE Empate == 0)
  , ("pontosGolos vence a casa", pontosGolos 3 1 (Golos 2 1) == (3, 0))
  , ("pontosGolos vence o de fora", pontosGolos 3 1 (Golos 0 4) == (0, 3))
  , ("pontosGolos empate", pontosGolos 3 1 (Golos 2 2) == (1, 1))
  , ("saldoGolos", saldoGolos (Golos 1 4) == -3)
  ]

testesClassificacao :: [Teste]
testesClassificacao =
  [ ("classificacaoDe Ana", classificacaoDe regrasPadrao ana partidasExemplo == Classificacao ana 4 3 1 1 1 0)
  , ("classificacaoDe Bruno", classificacaoDe regrasPadrao bruno partidasExemplo == Classificacao bruno 1 2 0 1 1 (-1))
  , ("classificacaoDe Carla", classificacaoDe regrasPadrao carla partidasExemplo == Classificacao carla 2 2 0 2 0 0)
  , ("classificacaoDe Diogo", classificacaoDe regrasPadrao diogo partidasExemplo == Classificacao diogo 3 1 1 0 0 1)
  , ("classificacaoDe sem jogos", classificacaoDe regrasPadrao (Participante 9 "Zé") partidasExemplo == classificacaoVazia (Participante 9 "Zé"))
  , ("classificacaoDe lista vazia", classificacaoDe regrasPadrao ana [] == classificacaoVazia ana)
  , ("classificacaoDe outras regras", pontos (classificacaoDe regrasAlternativas ana partidasExemplo) == 3)
  , ("classificacaoDe golos Ana", classificacaoDe regrasGolos ana partidasGolos == Classificacao ana 4 3 1 1 1 (-1))
  , ("classificacaoDe golos Diogo", classificacaoDe regrasGolos diogo partidasGolos == Classificacao diogo 3 1 1 0 0 3)
  , ("desempate mais pontos primeiro", desempatePadrao (linha ana 5 0) (linha bruno 3 9) == LT)
  , ("desempate pela diferenca", desempatePadrao (linha ana 3 1) (linha bruno 3 2) == GT)
  , ("desempate pelo nome", desempatePadrao (linha ana 3 1) (linha bruno 3 1) == LT)
  , ("ordenar inteiros", ordenar compare [3, 1, 2 :: Int] == [1, 2, 3])
  , ("ordenar lista vazia", null (ordenar compare ([] :: [Int])))
  , ("tabela 3 pontos", nomesTabela (tabela regrasPadrao participantesExemplo partidasExemplo) == ["Ana", "Diogo", "Carla", "Bruno"])
  , ("tabela 2 pontos desempata Diogo e Carla", nomesTabela (tabela regrasAlternativas participantesExemplo partidasExemplo) == ["Ana", "Diogo", "Carla", "Bruno"])
  , ("tabela golos", nomesTabela (tabela regrasGolos participantesExemplo partidasGolos) == ["Ana", "Diogo", "Carla", "Bruno"])
  , ("tabela tem todos", length (tabela regrasPadrao participantesExemplo partidasExemplo) == 4)
  , ("tabela sem participantes", null (tabela regrasPadrao [] partidasExemplo))
  ]

testes :: [Teste]
testes = testesParticipantes ++ testesCalendario ++ testesPartidas
  ++ testesPontuacao ++ testesClassificacao

main :: IO ()
main = do
  let falhas = [nome | (nome, ok) <- testes, not ok]
  mapM_ (\n -> putStrLn ("FALHOU: " ++ n)) falhas
  putStrLn (show (length testes - length falhas) ++ "/" ++ show (length testes) ++ " testes passaram")
  if null falhas then exitSuccess else exitFailure
