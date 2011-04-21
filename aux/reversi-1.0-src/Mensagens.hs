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
--  Arquivo: Mensagens.hs                                                                     --
--      M�dulo com todas as mensagens e textos utilizados no jogo.                            --
------------------------------------------------------------------------------------------------
--  �ltima Modifica��o: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Mensagens where

-- T�tulo da janela
janelaTitulo :: String
janelaTitulo = "hsReversi"




--------------------------------------------------------------------------------
-- NOMES DAS CORES DAS PE�AS ---------------------------------------------------
--------------------------------------------------------------------------------

strPreto :: String
strPreto = "PRETO"

strBranco :: String
strBranco = "BRANCO"

strVazio :: String
strVazio = "VAZIO"




--------------------------------------------------------------------------------
-- TEXTOS DOS ITENS DO MENU ----------------------------------------------------
--------------------------------------------------------------------------------

-- Menu 'Jogo'
menuJogo :: String
menuJogo = "&Jogo"

menuNovoJogoCPU :: String
menuNovoJogoCPU = "Novo jogo - &contra CPU"
helpNovoJogoCPU :: String
helpNovoJogoCPU = "Inicia um novo jogo contra a CPU"

menuNovoJogo2J :: String
menuNovoJogo2J = "Novo jogo - &2 jogadores"
helpNovoJogo2J :: String
helpNovoJogo2J = "Inicia um novo jogo para 2 jogadores"

menuFecha :: String
menuFecha = "&Encerrar partida"
helpFecha :: String
helpFecha = "Encerra a partida atual"

menuSair :: String
menuSair = "&Sair"
helpSair :: String
helpSair = "Sai do hsReversi"


-- Menu 'Op��es'
menuOpcoes :: String
menuOpcoes = "&Op��es"

menuAvisar :: String
menuAvisar = "&Avisar jogada inv�lida"
helpAvisar :: String
helpAvisar = "Ativa ou desativa a exibi��o de uma mensagem quando a jogada for inv�lida"

menuSkins :: String
menuSkins = "&Selecionar skin"

menuSkin1 :: String
menuSkin1 = "&1. Padr�o"

menuSkin2 :: String
menuSkin2 = "&2. Madeira"

menuSkin3 :: String
menuSkin3 = "&3. Metal"

menuSkin4 :: String
menuSkin4 = "&4. Vidro"


-- menu 'Ajuda'
menuAjuda :: String
menuAjuda = "&Ajuda"

menuRegras :: String
menuRegras = "&Regras do Jogo"
helpRegras :: String
helpRegras = "Mostra as regras do jogo"

menuSobre :: String
menuSobre = "&Sobre..."
helpSobre :: String
helpSobre = "Sobre o hsReversi"




--------------------------------------------------------------------------------
-- MENSAGENS DE AJUDA DIVERSAS -------------------------------------------------
--------------------------------------------------------------------------------

-- Regras do Jogo
msgRegrasT :: String
msgRegrasT = "Regras do Jogo"

msgRegras :: String
msgRegras =
    "REGRAS DO JOGO\n" ++
    "------------------------------------------------------------\n\n" ++
    "Objetivo do Jogo\n\n" ++
    "    Reversi � um jogo de estrat�gia que envolve racioc�nio de longo alcance.\n" ++
    "    O Reversi � jogado em um tabuleiro 8x8, com pedras pretas e brancas.\n" ++
    "    O objetivo � ter a maior quantidade de pedras da sua cor dispostas no tabuleiro.\n\n" ++
    "Jogando\n\n" ++
    "    O jogo inicia com quatro pedras, ao centro do tabuleiro, duas pretas e duas brancas.\n" ++
    "    O jogador com a pedra preta come�a o jogo.\n" ++
    "    Voc� inverte a cor das pedras do advers�rio, cercando as mesmas, em qualquer dire��o.\n" ++
    "    Os �nicos movimentos v�lidos s�o cercar as pedras do advers�rio.\n" ++
    "    Voc� tem que virar pelo menos uma pedra do advers�rio por jogada, caso n�o seja\n" ++
    "    poss�vel realizar qualquer movimento v�lido, o advers�rio joga novamente.\n\n" ++
    "T�rmino do Jogo\n\n" ++
    "    O jogo termina quando nenhum dos dois jogadores puder validar uma jogada,\n" ++
    "    podendo ocorrer o preenchimento total do tabuleiro."


-- Sobre o hsReversi
msgSobreT :: String
msgSobreT = "Sobre o hsReversi"

msgSobre :: String
msgSobre =
    "hsReversi 1.0\n" ++
    "------------------------------------------------------------\n\n" ++
    "Compilado com GHC 6.4 e wxHaskell 0.9.4\n\n" ++ 
    "Equipe:\n" ++
    "- Lucas Torre�o\n" ++
    "- Emanoel Barreiros\n" ++
    "- Hilda Borborema\n" ++
    "- Keldjan Alves"




--------------------------------------------------------------------------------
-- MENSAGENS DAS CAIXAS DE DI�LOGO ---------------------------------------------
--------------------------------------------------------------------------------

-- Mensagem de jogada Inv�lida
dlgInvalidaT :: String
dlgInvalidaT = "Jogada Inv�lida!"

dlgInvalida :: String
dlgInvalida = "Voc� deve virar pelo menos uma pe�a do advers�rio!"


-- Mensagem de novo jogo
dlgNovoJogoT :: String
dlgNovoJogoT = "Novo Jogo?"

dlgNovoJogo :: String
dlgNovoJogo = "Deseja iniciar um novo jogo?"


-- Mesangem de encerrar jogo
dlgFecharT :: String
dlgFecharT = "Encerrar Partida?"

dlgFechar :: String
dlgFechar = "Deseja encerrar a partida atual?"


-- Mensagem de jogar novamente
dlgJogaNovamenteT :: String
dlgJogaNovamenteT = "Jogar Novamente"

dlgJogaNovamente :: String -> String -> String
dlgJogaNovamente e1 e2 = "N�o h� jogadas poss�veis para o " ++ e1 ++ ".\nO " ++ e2 ++ " joga novamente."


-- Mensagem de jogo concluido
dlgConcluidoT :: String
dlgConcluidoT = "Jogo Conclu�do!"

dlgConcluido :: String -> Int -> Int -> String
dlgConcluido v p b =
    v ++ "\n" ++ 
    "------------------------------------------------------------\n\n\n" ++
    "Pontua��o Final:\n\n" ++
    strPreto ++ ": " ++ show (p) ++ "\n" ++
    strBranco ++ ": " ++ show (b)

dlgVPreto :: String
dlgVPreto = strPreto ++ " VENCEU!"

dlgVBranco :: String
dlgVBranco = strBranco ++ " VENCEU!"

dlgVEmpate :: String
dlgVEmpate = "JOGO EMPATADO!"

