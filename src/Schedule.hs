module Schedule where

import Model

-- Participantes

procurarParticipante :: Int -> [Participante] -> Maybe Participante
procurarParticipante _ [] = Nothing
procurarParticipante i (p : ps)
  | pId p == i = Just p
  | otherwise = procurarParticipante i ps

existeId :: Int -> [Participante] -> Bool
existeId _ [] = False
existeId i (p : ps)
  | pId p == i = True
  | otherwise = existeId i ps

idsDistintos :: [Participante] -> Bool
idsDistintos [] = True
idsDistintos (p : ps) = not (existeId (pId p) ps) && idsDistintos ps

-- Calendário

contra :: Participante -> [Participante] -> [(Participante, Participante)]
contra _ [] = []
contra x (y : ys) = (x, y) : contra x ys

todosContraTodos :: [Participante] -> [(Participante, Participante)]
todosContraTodos [] = []
todosContraTodos (x : xs) = contra x xs ++ todosContraTodos xs

numeroPartidas :: Int -> Int
numeroPartidas n
  | n < 2 = 0
  | otherwise = n * (n - 1) `div` 2

-- Partidas

criarPartidas :: [(Participante, Participante)] -> [Partida r]
criarPartidas [] = []
criarPartidas ((a, b) : ps) = Partida a b Nothing : criarPartidas ps

partidaJogada :: Partida r -> Bool
partidaJogada (Partida _ _ Nothing) = False
partidaJogada _ = True

contarJogadas :: [Partida r] -> Int
contarJogadas [] = 0
contarJogadas (p : ps)
  | partidaJogada p = 1 + contarJogadas ps
  | otherwise = contarJogadas ps

contarPorJogar :: [Partida r] -> Int
contarPorJogar [] = 0
contarPorJogar (p : ps)
  | partidaJogada p = contarPorJogar ps
  | otherwise = 1 + contarPorJogar ps

envolve :: Int -> Partida r -> Bool
envolve i p = pId (casa p) == i || pId (fora p) == i

jogosDe :: Int -> [Partida r] -> [Partida r]
jogosDe _ [] = []
jogosDe i (p : ps)
  | envolve i p = p : jogosDe i ps
  | otherwise = jogosDe i ps

-- Só aceita o resultado se a partida (casa a, fora b) existir e ainda não tiver sido jogada.
registarResultado :: Int -> Int -> r -> [Partida r] -> Maybe [Partida r]
registarResultado _ _ _ [] = Nothing
registarResultado a b r (p : ps)
  | pId (casa p) == a && pId (fora p) == b && not (partidaJogada p) =
      Just (p { resultado = Just r } : ps)
  | otherwise = case registarResultado a b r ps of
      Nothing -> Nothing
      Just resto -> Just (p : resto)

-- Torneio

novoTorneio :: [Participante] -> Torneio r
novoTorneio ps = Torneio ps (criarPartidas (todosContraTodos ps))
