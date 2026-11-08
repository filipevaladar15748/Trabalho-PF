# Gestor de Torneios Genérico

Trabalho prático de Programação Funcional.

## Equipa

| Nome | Número |
|------|--------|
| _Membro 1_ | _a00000_ |
| _Membro 2_ | _a00000_ |
| _Membro 3_ | _a00000_ |

## Descrição

Gestão de torneios de qualquer jogo. O que é comum a todos os torneios
(participantes, partidas, calendário, classificação, eliminatórias) está
separado do que é específico de cada jogo (pontuação e desempate).

Uma partida é do tipo `Partida r`, onde `r` é o tipo de resultado do jogo. As
regras de um jogo são um valor `Regras r` com três funções: os pontos que cada
resultado dá, o saldo do resultado (quem ganhou e por quanto) e o critério de
desempate. Para mudar de jogo basta mudar as regras.

Há dois tipos de resultado de exemplo:

- `Resultado`: vitória da casa, vitória de fora ou empate;
- `Golos`: golos da casa e golos de fora.

## Funcionalidades da Fase 1

- Modelo genérico: `Partida r`, `Regras r`, `Classificacao`, `Torneio r`.
- Pesquisa de participantes e validação de identificadores repetidos.
- Calendário todos contra todos.
- Criação de um torneio com todas as partidas por jogar.
- Registo de resultados (recusa partidas inexistentes ou já jogadas).
- Contagem de partidas jogadas e por jogar.
- Pontuação por pattern matching para os dois tipos de resultado.
- Classificação de cada participante (pontos, jogos, vitórias, empates,
  derrotas, diferença) e tabela ordenada pelo critério de desempate.

## Estrutura

```
.
├── README.md
├── cabal.project
├── gestor-torneios.cabal
├── src/
│   ├── Model.hs
│   ├── Schedule.hs
│   ├── Standings.hs
│   └── DadosExemplo.hs
├── app/
│   └── Main.hs
├── test/
│   └── TestMain.hs
├── data/
└── docs/
```

- `Model.hs`: tipos do domínio.
- `Schedule.hs`: participantes, calendário, partidas e criação de torneios.
- `Standings.hs`: pontuação, classificação e desempate.
- `DadosExemplo.hs`: dados de demonstração para os dois tipos de resultado.
- `Main.hs`: demonstração, único módulo com IO.

## Compilar e executar

Com GHC, na raiz do projeto:

```
ghc -isrc app/Main.hs -o gestor-torneios
./gestor-torneios
```

Com Cabal:

```
cabal run gestor-torneios
```

## Testes

```
ghc -isrc test/TestMain.hs -o testes
./testes
```

ou:

```
cabal test
```

## Próximas fases

- Fase 2: classificação com folds, desempates compostos, estatísticas com
  map/filter/fold, erros com `Either`.
- Fase 3: leitura de CSV, eliminatórias, menu textual e relatórios.
