module Model where

data Participante = Participante
  { pId :: Int
  , pNome :: String
  } deriving (Eq, Show)

-- Resultado de jogos sem marcador: vitória da casa, vitória de fora ou empate.
data Resultado = VenceCasa | VenceFora | Empate
  deriving (Eq, Show)

-- Resultado de jogos com marcador: golos da casa e golos de fora.
data Golos = Golos Int Int
  deriving (Eq, Show)

-- Uma partida serve para qualquer tipo de resultado r.
data Partida r = Partida
  { casa :: Participante
  , fora :: Participante
  , resultado :: Maybe r
  } deriving (Eq, Show)

data Torneio r = Torneio
  { participantes :: [Participante]
  , partidas :: [Partida r]
  } deriving (Eq, Show)
