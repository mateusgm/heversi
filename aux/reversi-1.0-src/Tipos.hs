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
--  Arquivo: Tipos.hs                                                                         --
--      Módulo com definições dos tipos utilizados e funções sobre tipos.                     --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
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

-- Importando módulos necessários
import Graphics.UI.WX
import Mensagens




--------------------------------------------------------------------------------
-- DEFINIÇÕES DE TIPOS E DADOS -------------------------------------------------
--------------------------------------------------------------------------------

-- Definindo o tipo algébrico de estados
-- Corresponde aos possíveis estados de uma posição no tabuleiro
data Estado = Branco | Preto | Vazio deriving (Eq)

-- Definindo um sinônimo de tipos para representar uma jogada
-- Consiste em uma tupla com as coordenadas (horizontal e vertical) e o estado
type Jogada = (Int,Int,Estado)

-- Definindo um sinônimo de tipo para o tabuleiro do jogo
type Tabuleiro = [Jogada]

-- Definindo um sinônimo de tipo para o ambiente do jogo
-- Consiste em uma tupla de variáveis e elementos "alteráveis" durante o jogo
type Ambiente = (
    Frame (),       -- Janela principal do jogo
    Var Tabuleiro,  -- Tabuleiro do jogo
    Var Int,        -- Modo de jogo
    Var Estado,     -- Estado que está na vez de jogar
    Var Bool,       -- Variável de aviso de jogada inválida
    Var String,     -- Skin do jogo
    [Panel ()],     -- Lista de posições (painéis) do tabuleiro
    Panel (),       -- Painel que represente o fundo da janela
    Panel (),       -- Painel que representa o placar do jogo
    MenuItem ()     -- Item do menu que encerra a partida
    )




--------------------------------------------------------------------------------
-- FUNÇÕES PARA RETORNO E MANIPULAÇÃO DE TIPOS ---------------------------------
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

-- Retorna somente a variável de vez
ambVez :: Ambiente -> Var Estado
ambVez (_, _, _, a, _, _, _, _, _, _) = a

-- Retorna somente a variável de aviso de jogada inválida
ambAvs :: Ambiente -> Var Bool
ambAvs (_, _, _, _, a, _, _, _, _, _) = a

-- Retorna somente o skin
ambSkn :: Ambiente -> Var String
ambSkn (_, _, _, _, _, a, _, _, _, _) = a

-- Retorna somente a lista de posições (painéis)
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

