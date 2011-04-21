------------------------------------------------------------------------------------------------
--  Universidade de Pernambuco                                                                --
--  Escola Politécnica de Pernambuco                                                          --
--  Departamento de Sistemas Computacionais                                                   --
--  Disciplina: Linguangem de Programação                                                     --
--  Professor: Sérgio Soares                                                                  --
--  Turma: K2                                                                                 --
------------------------------------------------------------------------------------------------
--  Projeto: hsReversi                                                                        --
------------------------------------------------------------------------------------------------
--  Equipe:                                                                                   --
--      Lucas Torreão       <lucastds@gmail.com>                                              --
--      Emanoel Barreiros   <emanoelbarreiros@gmail.com>                                      --
--      Hilda Borborema     <hildaborborema@yahoo.com.br>                                     --
--      Keldjan Alves       <keldjan@hotmail.com>                                             --
------------------------------------------------------------------------------------------------
--  Objetivo:                                                                                 --
--      Desenvolvimento do projeto referente à segunda unidade do semestre letivo 2005.1.     --
--                                                                                            --
--  Descrição do Projeto:                                                                     --
--      Este projeto trata-se da implementação em Haskell do jogo Reversi, com interface      --
--      gráfica baseada na biblioteca hxHaskell. Pode-se jogar com dois jogadores humanos ou  --
--      um jogador contra a CPU. Foram utilizados recursos avançados do wxHaskell, visando    --
--      um ambiente visual agradável, inclusive com a possibilidade de troca de "skins".      --
------------------------------------------------------------------------------------------------
--  Arquivo: Regras.hs                                                                        --
--      Módulo que implementa as regras e lógica do jogo Reversi.                             --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Regras (
    testaJogada,
    executaJogada,
    tabInicial,
    tabZerado,
    jogadasPossiveis,
    jogoConcluido,
    mostraGanhador,
    qtdPreto,
    qtdBranco,
    melhorJogada
    ) where

-- Importando módulos necessários
import Tipos
import Aleatorio




--------------------------------------------------------------------------------
-- FUNÇÕES BÁSICAS -------------------------------------------------------------
--------------------------------------------------------------------------------

-- Recebe as coordenadas e retorna o estado naquela posição
retornaEstado :: Tabuleiro -> Int -> Int -> Estado
retornaEstado [] _ _ = error "Tabuleiro Inválido!"
retornaEstado ((tx, ty, te):ts) x y
    | tx == x && ty == y = te
    | otherwise          = retornaEstado ts x y


-- Altera o estado de uma determinada posição do tabuleiro
alteraEstado :: Tabuleiro -> Int -> Int -> Estado -> Tabuleiro
alteraEstado [] _ _ _ = error "Tabuleiro Inválido!"
alteraEstado ((tx, ty, te):ts) x y e
    | tx == x && ty == y = (tx, ty, e) : ts
    | otherwise          = (tx, ty, te) : alteraEstado ts x y e


-- Arrumação inicial do tabuleiro
tabInicial :: Tabuleiro
tabInicial =
  [(1,1,Vazio),(1,2,Vazio),(1,3,Vazio),(1,4,Vazio),(1,5,Vazio),(1,6,Vazio),(1,7,Vazio),(1,8,Vazio),
   (2,1,Vazio),(2,2,Vazio),(2,3,Vazio),(2,4,Vazio),(2,5,Vazio),(2,6,Vazio),(2,7,Vazio),(2,8,Vazio),
   (3,1,Vazio),(3,2,Vazio),(3,3,Vazio),(3,4,Vazio),(3,5,Vazio),(3,6,Vazio),(3,7,Vazio),(3,8,Vazio),
   (4,1,Vazio),(4,2,Vazio),(4,3,Vazio),(4,4,Branco),(4,5,Preto),(4,6,Vazio),(4,7,Vazio),(4,8,Vazio),
   (5,1,Vazio),(5,2,Vazio),(5,3,Vazio),(5,4,Preto),(5,5,Branco),(5,6,Vazio),(5,7,Vazio),(5,8,Vazio),
   (6,1,Vazio),(6,2,Vazio),(6,3,Vazio),(6,4,Vazio),(6,5,Vazio),(6,6,Vazio),(6,7,Vazio),(6,8,Vazio),
   (7,1,Vazio),(7,2,Vazio),(7,3,Vazio),(7,4,Vazio),(7,5,Vazio),(7,6,Vazio),(7,7,Vazio),(7,8,Vazio),
   (8,1,Vazio),(8,2,Vazio),(8,3,Vazio),(8,4,Vazio),(8,5,Vazio),(8,6,Vazio),(8,7,Vazio),(8,8,Vazio)]


-- Tabuleiro zerado
tabZerado :: Tabuleiro
tabZerado =
  [(1,1,Vazio),(1,2,Vazio),(1,3,Vazio),(1,4,Vazio),(1,5,Vazio),(1,6,Vazio),(1,7,Vazio),(1,8,Vazio),
   (2,1,Vazio),(2,2,Vazio),(2,3,Vazio),(2,4,Vazio),(2,5,Vazio),(2,6,Vazio),(2,7,Vazio),(2,8,Vazio),
   (3,1,Vazio),(3,2,Vazio),(3,3,Vazio),(3,4,Vazio),(3,5,Vazio),(3,6,Vazio),(3,7,Vazio),(3,8,Vazio),
   (4,1,Vazio),(4,2,Vazio),(4,3,Vazio),(4,4,Vazio),(4,5,Vazio),(4,6,Vazio),(4,7,Vazio),(4,8,Vazio),
   (5,1,Vazio),(5,2,Vazio),(5,3,Vazio),(5,4,Vazio),(5,5,Vazio),(5,6,Vazio),(5,7,Vazio),(5,8,Vazio),
   (6,1,Vazio),(6,2,Vazio),(6,3,Vazio),(6,4,Vazio),(6,5,Vazio),(6,6,Vazio),(6,7,Vazio),(6,8,Vazio),
   (7,1,Vazio),(7,2,Vazio),(7,3,Vazio),(7,4,Vazio),(7,5,Vazio),(7,6,Vazio),(7,7,Vazio),(7,8,Vazio),
   (8,1,Vazio),(8,2,Vazio),(8,3,Vazio),(8,4,Vazio),(8,5,Vazio),(8,6,Vazio),(8,7,Vazio),(8,8,Vazio)]




--------------------------------------------------------------------------------
-- FUNÇÕES DE VERIFICAÇÃO DE JOGADAS -------------------------------------------
--------------------------------------------------------------------------------

-- Direções:
-- C = cima, B = baixo, E = esquerda, D = direta
-- CE = cima-esquerda, CD = cima-direta, BE = baixo-esquerda, BD = baixo-direita


-- Função para testar se uma jogada é válida ou não
testaJogada :: Tabuleiro -> Jogada -> Bool
testaJogada t (x, y, e) = ( retornaEstado t x y == Vazio ) &&
                          ( testaDirecaoC t  (x, y, e) False || testaDirecaoB t  (x, y, e) False ||
                            testaDirecaoE t  (x, y, e) False || testaDirecaoD t  (x, y, e) False ||
                            testaDirecaoCE t (x, y, e) False || testaDirecaoCD t (x, y, e) False ||
                            testaDirecaoBE t (x, y, e) False || testaDirecaoBD t (x, y, e) False )


-- Função geral para testar jogadas em uma determinada direção
testaDirecao :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Tabuleiro -> Jogada -> Bool -> Bool
testaDirecao fx fy mx my t (x, y, e) f
    -- Se tocar a margem - False
    | x == mx || y == my = False
    -- Se a proxima posição imediata for diferente da cor oposta - False
    | f == False && retornaEstado t (fx x) (fy y) /= oposto e = False
    -- Se a proxima posição não-imediata for vazia - False
    | f == True && retornaEstado t (fx x) (fy y) == Vazio = False
    -- Se a proxima posição não-imediata for de mesma cor - True
    | f == True && retornaEstado t (fx x) (fy y) == e = True
    -- Caso contrário, teste a próxima posição
    | otherwise = testaDirecao fx fy mx my t ((fx x), (fy y), e) True


-- Restringindo a função testaDirecao para cada possivel direção
testaDirecaoC  = testaDirecao menosUm id      1 0
testaDirecaoB  = testaDirecao maisUm  id      8 0
testaDirecaoE  = testaDirecao id      menosUm 0 1
testaDirecaoD  = testaDirecao id      maisUm  0 8
testaDirecaoCE = testaDirecao menosUm menosUm 1 1
testaDirecaoCD = testaDirecao menosUm maisUm  1 8
testaDirecaoBE = testaDirecao maisUm  menosUm 8 1
testaDirecaoBD = testaDirecao maisUm  maisUm  8 8


-- Funções auxiliares para determinar direções
maisUm :: Int -> Int
maisUm x = x+1
menosUm :: Int -> Int
menosUm x = x-1




--------------------------------------------------------------------------------
-- FUNÇÕES DE EFETIVAÇÃO DE JOGADAS --------------------------------------------
--------------------------------------------------------------------------------

-- Função principal para executar uma jogada
-- Esta função será usada diretamente na execução de jogadas
executaJogada :: Tabuleiro -> Jogada -> Tabuleiro
executaJogada t j
    | testaJogada t j == True = efetivaJogada t j
    | otherwise               = t
    where
        efetivaJogada t j = alteraDirecaoBD (alteraDirecaoBE
                           (alteraDirecaoCD (alteraDirecaoCE
                           (alteraDirecaoD  (alteraDirecaoE 
                           (alteraDirecaoB  (alteraDirecaoC t
                            j) j) j) j) j) j) j) j


-- Função geral para alterar as peças numa jogada
alteraDirecao :: (Int -> Int) -> (Int -> Int) -> Tabuleiro -> Jogada -> Tabuleiro
alteraDirecao fx fy t (x, y, e)
    | retornaEstado t (fx x) (fy y) == e = alteraEstado t x y e
    | otherwise = alteraDirecao fx fy (alteraEstado t x y e) ((fx x), (fy y), e)


-- Restringindo a função alteraDirecao para cada direção possível
alteraDirecaoC :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoC t j
    | testaDirecaoC t j False == True  = alteraDirecao menosUm id t j
    | otherwise                        = t
alteraDirecaoB :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoB t j
    | testaDirecaoB t j False == True  = alteraDirecao maisUm id t j
    | otherwise                        = t
alteraDirecaoE :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoE t j
    | testaDirecaoE t j False == True  = alteraDirecao id menosUm t j
    | otherwise                        = t
alteraDirecaoD :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoD t j
    | testaDirecaoD t j False == True  = alteraDirecao id maisUm t j
    | otherwise                        = t
alteraDirecaoCE :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoCE t j
    | testaDirecaoCE t j False == True = alteraDirecao menosUm menosUm t j
    | otherwise                        = t
alteraDirecaoCD :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoCD t j
    | testaDirecaoCD t j False == True = alteraDirecao menosUm maisUm t j
    | otherwise                        = t
alteraDirecaoBE :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoBE t j
    | testaDirecaoBE t j False == True = alteraDirecao maisUm menosUm t j
    | otherwise                        = t
alteraDirecaoBD :: Tabuleiro -> Jogada -> Tabuleiro
alteraDirecaoBD t j
    | testaDirecaoBD t j False == True = alteraDirecao maisUm maisUm t j
    | otherwise                        = t




--------------------------------------------------------------------------------
-- FUNÇÕES PARA DETERMINAÇÃO DE PLACAR E TÉRMINO DE JOGO -----------------------
--------------------------------------------------------------------------------


-- Funções para fazer a contagem de peças de cada cor no tabuleiro
qtdBranco :: Tabuleiro -> Int
qtdBranco t = length [1 | (x, y, e) <- t, (retornaEstado t x y) == Branco]
qtdPreto :: Tabuleiro -> Int
qtdPreto  t = length [1 | (x, y, e) <- t, (retornaEstado t x y) == Preto]


-- Função para testar se o jogo terminou (se não existem mais jogadas possíveis)
jogadasPossiveis :: Tabuleiro -> Estado -> Bool
jogadasPossiveis t e = length [1 | (tx, ty, te) <- t, testaJogada t (tx, ty, e)] /= 0


-- Função para testar se o jogo terminou (se não existem mais jogadas possíveis)
jogoConcluido :: Tabuleiro -> Bool
jogoConcluido t = (not (jogadasPossiveis t Preto)) && (not (jogadasPossiveis t Branco))


-- Função para verificar qual jogador ganhou (o que tem mais peças de sua cor)
mostraGanhador :: Tabuleiro -> Estado
mostraGanhador t
    | qtdBranco t > qtdPreto t = Branco
    | qtdPreto t > qtdBranco t = Preto
    | otherwise                = Vazio




--------------------------------------------------------------------------------
-- FUNÇÕES PARA DETERMINAR AS JOGADAS DA CPU -----------------------------------
--------------------------------------------------------------------------------


-- Função que conta quantas peças serão viradas para em uma jogada
contaJogada :: Tabuleiro -> Jogada -> Int
contaJogada t j = contaDirecaoBD t j + contaDirecaoBE t j +
                  contaDirecaoCD t j + contaDirecaoCE t j +
                  contaDirecaoD  t j + contaDirecaoE  t j +
                  contaDirecaoB  t j + contaDirecaoC  t j


-- Função que conta quantas peças serão viradas em uma determinada direção
contaDirecao :: (Int -> Int) -> (Int -> Int) -> Tabuleiro -> Jogada -> Int
contaDirecao fx fy t (x, y, e)
    | retornaEstado t (fx x) (fy y) == e = 1
    | otherwise = 1 + contaDirecao fx fy t ((fx x), (fy y), e)


-- Restringindo a função contaDirecao para cada direção possível
contaDirecaoC :: Tabuleiro -> Jogada -> Int
contaDirecaoC t j
    | testaDirecaoC t j False == True = contaDirecao menosUm id t j
    | otherwise                       = 0
contaDirecaoB :: Tabuleiro -> Jogada -> Int
contaDirecaoB t j
    | testaDirecaoB t j False == True = contaDirecao maisUm id t j
    | otherwise                       = 0
contaDirecaoE :: Tabuleiro -> Jogada -> Int
contaDirecaoE t j
    | testaDirecaoE t j False == True = contaDirecao id menosUm t j
    | otherwise                       = 0
contaDirecaoD :: Tabuleiro -> Jogada -> Int
contaDirecaoD t j
    | testaDirecaoD t j False == True = contaDirecao id maisUm t j
    | otherwise                       = 0
contaDirecaoCE :: Tabuleiro -> Jogada -> Int
contaDirecaoCE t j
    | testaDirecaoCE t j False == True = contaDirecao menosUm menosUm t j
    | otherwise                        = 0
contaDirecaoCD :: Tabuleiro -> Jogada -> Int
contaDirecaoCD t j
    | testaDirecaoCD t j False == True = contaDirecao menosUm maisUm t j
    | otherwise                        = 0
contaDirecaoBE :: Tabuleiro -> Jogada -> Int
contaDirecaoBE t j
    | testaDirecaoBE t j False == True = contaDirecao maisUm menosUm t j
    | otherwise                        = 0
contaDirecaoBD :: Tabuleiro -> Jogada -> Int
contaDirecaoBD t j
    | testaDirecaoBD t j False == True = contaDirecao maisUm maisUm t j
    | otherwise                        = 0


-- Função que determina as melhores jogadas possíveis e escolhe uma aleatoriamente
melhorJogada :: Tabuleiro -> Estado -> Jogada
melhorJogada t e = maior (listaJogadas t e)
    where
        maior [j]    = j
        maior (j:ts) = comparaDuas (aleatorio2 (maior ts) j)
        listaJogadas t e = [(tx, ty, te) | (tx, ty, te) <- t, testaJogada t (tx, ty, e)]
        comparaDuas (j1, j2)
            | contaJogada t j1 > contaJogada t j2  = j1
            | contaJogada t j1 == contaJogada t j2 = aleatorio1 j1 j2
            | otherwise                            = j2

