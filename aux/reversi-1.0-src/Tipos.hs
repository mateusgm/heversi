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
--  Arquivo: Tipos.hs                                                                         --
--      M�dulo com defini��es dos tipos utilizados e fun��es sobre tipos.                     --
------------------------------------------------------------------------------------------------
--  �ltima Modifica��o: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Tipos (
    Estado (
        Branco,
        Preto,
        Vazio
        ),
    Jogada,
    Tabuleiro,
    Ambiente,
    oposto,
    ambFrm, ambTbl,
    ambMod, ambVez,
    ambAvs, ambSkn,
    ambPos, ambPn1,
    ambPn2, ambFch,
    strEstado,
    toIO
    ) where

-- Importando m�dulos necess�rios
import Graphics.UI.WX
import Mensagens




--------------------------------------------------------------------------------
-- DEFINI��ES DE TIPOS E DADOS -------------------------------------------------
--------------------------------------------------------------------------------

-- Definindo o tipo alg�brico de estados
-- Corresponde aos poss�veis estados de uma posi��o no tabuleiro
data Estado = Branco | Preto | Vazio deriving (Eq)

-- Definindo um sin�nimo de tipos para representar uma jogada
-- Consiste em uma tupla com as coordenadas (horizontal e vertical) e o estado
type Jogada = (Int,Int,Estado)

-- Definindo um sin�nimo de tipo para o tabuleiro do jogo
type Tabuleiro = [Jogada]

-- Definindo um sin�nimo de tipo para o ambiente do jogo
-- Consiste em uma tupla de vari�veis e elementos "alter�veis" durante o jogo
type Ambiente = (
    Frame (),       -- Janela principal do jogo
    Var Tabuleiro,  -- Tabuleiro do jogo
    Var Int,        -- Modo de jogo
    Var Estado,     -- Estado que est� na vez de jogar
    Var Bool,       -- Vari�vel de aviso de jogada inv�lida
    Var String,     -- Skin do jogo
    [Panel ()],     -- Lista de posi��es (pain�is) do tabuleiro
    Panel (),       -- Painel que represente o fundo da janela
    Panel (),       -- Painel que representa o placar do jogo
    MenuItem ()     -- Item do menu que encerra a partida
    )




--------------------------------------------------------------------------------
-- FUN��ES PARA RETORNO E MANIPULA��O DE TIPOS ---------------------------------
--------------------------------------------------------------------------------

-- Recebe um estado e retorna seu oposto
oposto :: Estado -> Estado
oposto e
    | e == Preto  = Branco
    | e == Branco = Preto
    | otherwise   = Vazio


-- Retorna somenta o elemento janela (frame)
ambFrm :: Ambiente -> Frame ()
ambFrm (a, _, _, _, _, _, _, _, _, _) = a

-- Retorna somente o tabuleiro
ambTbl :: Ambiente -> Var Tabuleiro
ambTbl (_, a, _, _, _, _, _, _, _, _) = a

-- Retorna somente o modo de jogo
ambMod :: Ambiente -> Var Int
ambMod (_, _, a, _, _, _, _, _, _, _) = a

-- Retorna somente a vari�vel de vez
ambVez :: Ambiente -> Var Estado
ambVez (_, _, _, a, _, _, _, _, _, _) = a

-- Retorna somente a vari�vel de aviso de jogada inv�lida
ambAvs :: Ambiente -> Var Bool
ambAvs (_, _, _, _, a, _, _, _, _, _) = a

-- Retorna somente o skin
ambSkn :: Ambiente -> Var String
ambSkn (_, _, _, _, _, a, _, _, _, _) = a

-- Retorna somente a lista de posi��es (pain�is)
ambPos :: Ambiente -> [Panel ()]
ambPos (_, _, _, _, _, _, a, _, _, _) = a

-- Retorna somente o painel de fundo
ambPn1 :: Ambiente -> Panel ()
ambPn1 (_, _, _, _, _, _, _, a, _, _) = a

-- Retorna somente o painel do placar
ambPn2 :: Ambiente -> Panel ()
ambPn2 (_, _, _, _, _, _, _, _, a, _) = a

-- Retorna somente o item do menu que encerra a partida
ambFch :: Ambiente -> MenuItem ()
ambFch (_, _, _, _, _, _, _, _, _, a) = a



-- Converte Estado para String
strEstado :: Estado -> String
strEstado e
    | e == Preto  = strPreto
    | e == Branco = strBranco
    | otherwise   = strVazio


-- Converte um tipo t para o tipo IO t
toIO :: t -> IO t
toIO t = do {return (t)}

