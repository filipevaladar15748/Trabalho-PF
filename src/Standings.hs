module Standings where

import Model

-- Pontuação de jogos com vitória, empate ou derrota

pontosVVE :: Int -> Int -> Resultado -> (Int, Int)
pontosVVE v _ VenceCasa = (v, 0)
pontosVVE v _ VenceFora = (0, v)
pontosVVE _ e Empate = (e, e)

saldoVVE :: Resultado -> Int
saldoVVE VenceCasa = 1
saldoVVE VenceFora = -1
saldoVVE Empate = 0

-- Pontuação de jogos com golos

pontosGolos :: Int -> Int -> Golos -> (Int, Int)
pontosGolos v e (Golos a b)
  | a > b = (v, 0)
  | a < b = (0, v)
  | otherwise = (e, e)

saldoGolos :: Golos -> Int
saldoGolos (Golos a b) = a - b

-- Classificação

classificacaoVazia :: Participante -> Classificacao
classificacaoVazia p = Classificacao p 0 0 0 0 0 0

-- Junta um jogo: pontos ganhos e saldo do ponto de vista do participante.
somarJogo :: Int -> Int -> Classificacao -> Classificacao
somarJogo pts s c
  | s > 0 = c' { vitorias = vitorias c + 1 }
  | s < 0 = c' { derrotas = derrotas c + 1 }
  | otherwise = c' { empates = empates c + 1 }
  where
    c' = c { pontos = pontos c + pts, jogos = jogos c + 1, diferenca = diferenca c + s }

classificacaoDe :: Regras r -> Participante -> [Partida r] -> Classificacao
classificacaoDe _ p [] = classificacaoVazia p
classificacaoDe rs p (Partida c f (Just r) : ps)
  | pId c == pId p = somarJogo (fst (pontuar rs r)) (saldo rs r) resto
  | pId f == pId p = somarJogo (snd (pontuar rs r)) (negate (saldo rs r)) resto
  where
    resto = classificacaoDe rs p ps
classificacaoDe rs p (_ : ps) = classificacaoDe rs p ps

-- Mais pontos primeiro; depois maior diferença; depois ordem alfabética.
desempatePadrao :: Classificacao -> Classificacao -> Ordering
desempatePadrao a b
  | pontos a /= pontos b = compare (pontos b) (pontos a)
  | diferenca a /= diferenca b = compare (diferenca b) (diferenca a)
  | otherwise = compare (pNome (participante a)) (pNome (participante b))

inserir :: (a -> a -> Ordering) -> a -> [a] -> [a]
inserir _ x [] = [x]
inserir cmp x (y : ys)
  | cmp x y == GT = y : inserir cmp x ys
  | otherwise = x : y : ys

ordenar :: (a -> a -> Ordering) -> [a] -> [a]
ordenar _ [] = []
ordenar cmp (x : xs) = inserir cmp x (ordenar cmp xs)

classificacoes :: Regras r -> [Participante] -> [Partida r] -> [Classificacao]
classificacoes _ [] _ = []
classificacoes rs (p : ps) js = classificacaoDe rs p js : classificacoes rs ps js

-- Tabela ordenada com o critério de desempate das regras.
tabela :: Regras r -> [Participante] -> [Partida r] -> [Classificacao]
tabela rs ps js = ordenar (desempate rs) (classificacoes rs ps js)
