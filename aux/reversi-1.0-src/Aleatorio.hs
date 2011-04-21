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
--  Arquivo: Aleatorio.hs                                                                     --
--      Módulo que implementa funções simples de retorno aleatório.                           --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Aleatorio (aleatorio1, aleatorio2) where

-- Importando módulos necessários
import System.IO.Unsafe
import System.Random


-- Função que recebe dois argumentos de mesmo tipo e
-- retorna aleatoriamente um dos dois
aleatorio1 :: a -> a -> a
aleatorio1 a b = unsafePerformIO (aux1 a b)
    where
    aux1 a b = do
        newStdGen
        x <- getStdGen;
        if (aux2 (fst (next x)))
            then do
                return a
            else do
                return b
    aux2 x = (mod x 2) == 0


-- Função que recebe dois argumentos de mesmo tipo e
-- retorna uma tupla com os dois em ordem aleatória
aleatorio2 :: a -> a -> (a, a)
aleatorio2 a b = unsafePerformIO (aux1 a b)
    where
    aux1 a b = do
        newStdGen
        x <- getStdGen;
        if (aux2 (fst (next x)))
            then do
                return (a, b)
            else do
                return (b, a)
    aux2 x = (mod x 2) == 0

