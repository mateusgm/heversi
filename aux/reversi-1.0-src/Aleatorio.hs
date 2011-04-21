------------------------------------------------------------------------------------------------
--  Universidade de Pernambuco                                                                --
--  Escola Polit�cnica de Pernambuco                                                          --
--  Departamento de Sistemas Computacionais                                                   --
--  Disciplina: Linguangem de Programa��o                                                     --
--  Professor: S�rgio Soares                                                                  --
--  Turma: K2                                                                                 --
------------------------------------------------------------------------------------------------
--  Projeto: hsReversi                                                                        --
------------------------------------------------------------------------------------------------
--  Equipe:                                                                                   --
--      Lucas Torre�o       <lucastds@gmail.com>                                              --
--      Emanoel Barreiros   <emanoelbarreiros@gmail.com>                                      --
--      Hilda Borborema     <hildaborborema@yahoo.com.br>                                     --
--      Keldjan Alves       <keldjan@hotmail.com>                                             --
------------------------------------------------------------------------------------------------
--  Objetivo:                                                                                 --
--      Desenvolvimento do projeto referente � segunda unidade do semestre letivo 2005.1.     --
--                                                                                            --
--  Descri��o do Projeto:                                                                     --
--      Este projeto trata-se da implementa��o em Haskell do jogo Reversi, com interface      --
--      gr�fica baseada na biblioteca hxHaskell. Pode-se jogar com dois jogadores humanos ou  --
--      um jogador contra a CPU. Foram utilizados recursos avan�ados do wxHaskell, visando    --
--      um ambiente visual agrad�vel, inclusive com a possibilidade de troca de "skins".      --
------------------------------------------------------------------------------------------------
--  Arquivo: Aleatorio.hs                                                                     --
--      M�dulo que implementa fun��es simples de retorno aleat�rio.                           --
------------------------------------------------------------------------------------------------
--  �ltima Modifica��o: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Aleatorio (aleatorio1, aleatorio2) where

-- Importando m�dulos necess�rios
import System.IO.Unsafe
import System.Random


-- Fun��o que recebe dois argumentos de mesmo tipo e
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


-- Fun��o que recebe dois argumentos de mesmo tipo e
-- retorna uma tupla com os dois em ordem aleat�ria
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

