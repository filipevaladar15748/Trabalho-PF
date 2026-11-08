module DadosExemplo where

import Model

ana, bruno, carla, diogo :: Participante
ana = Participante 1 "Ana"
bruno = Participante 2 "Bruno"
carla = Participante 3 "Carla"
diogo = Participante 4 "Diogo"

participantesExemplo :: [Participante]
participantesExemplo = [ana, bruno, carla, diogo]

partidasExemplo :: [Partida Resultado]
partidasExemplo =
  [ Partida ana bruno (Just VenceCasa)
  , Partida ana carla (Just Empate)
  , Partida ana diogo (Just VenceFora)
  , Partida bruno carla (Just Empate)
  , Partida bruno diogo Nothing
  , Partida carla diogo Nothing
  ]

-- O mesmo calendário, mas num jogo com golos.
partidasGolos :: [Partida Golos]
partidasGolos =
  [ Partida ana bruno (Just (Golos 2 0))
  , Partida ana carla (Just (Golos 1 1))
  , Partida ana diogo (Just (Golos 0 3))
  , Partida bruno carla (Just (Golos 2 2))
  , Partida bruno diogo Nothing
  , Partida carla diogo Nothing
  ]
