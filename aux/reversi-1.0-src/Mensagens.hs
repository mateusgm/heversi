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
--  Arquivo: Mensagens.hs                                                                     --
--      Módulo com todas as mensagens e textos utilizados no jogo.                            --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Mensagens where

-- Título da janela
janelaTitulo :: String
janelaTitulo = "hsReversi"




--------------------------------------------------------------------------------
-- NOMES DAS CORES DAS PEÇAS ---------------------------------------------------
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


-- Menu 'Opções'
menuOpcoes :: String
menuOpcoes = "&Opções"

menuAvisar :: String
menuAvisar = "&Avisar jogada inválida"
helpAvisar :: String
helpAvisar = "Ativa ou desativa a exibição de uma mensagem quando a jogada for inválida"

menuSkins :: String
menuSkins = "&Selecionar skin"

menuSkin1 :: String
menuSkin1 = "&1. Padrão"

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
    "    Reversi é um jogo de estratégia que envolve raciocínio de longo alcance.\n" ++
    "    O Reversi é jogado em um tabuleiro 8x8, com pedras pretas e brancas.\n" ++
    "    O objetivo é ter a maior quantidade de pedras da sua cor dispostas no tabuleiro.\n\n" ++
    "Jogando\n\n" ++
    "    O jogo inicia com quatro pedras, ao centro do tabuleiro, duas pretas e duas brancas.\n" ++
    "    O jogador com a pedra preta começa o jogo.\n" ++
    "    Você inverte a cor das pedras do adversário, cercando as mesmas, em qualquer direção.\n" ++
    "    Os únicos movimentos válidos são cercar as pedras do adversário.\n" ++
    "    Você tem que virar pelo menos uma pedra do adversário por jogada, caso não seja\n" ++
    "    possível realizar qualquer movimento válido, o adversário joga novamente.\n\n" ++
    "Término do Jogo\n\n" ++
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
    "- Lucas Torreão\n" ++
    "- Emanoel Barreiros\n" ++
    "- Hilda Borborema\n" ++
    "- Keldjan Alves"




--------------------------------------------------------------------------------
-- MENSAGENS DAS CAIXAS DE DIÁLOGO ---------------------------------------------
--------------------------------------------------------------------------------

-- Mensagem de jogada Inválida
dlgInvalidaT :: String
dlgInvalidaT = "Jogada Inválida!"

dlgInvalida :: String
dlgInvalida = "Você deve virar pelo menos uma peça do adversário!"


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
dlgJogaNovamente e1 e2 = "Não há jogadas possíveis para o " ++ e1 ++ ".\nO " ++ e2 ++ " joga novamente."


-- Mensagem de jogo concluido
dlgConcluidoT :: String
dlgConcluidoT = "Jogo Concluído!"

dlgConcluido :: String -> Int -> Int -> String
dlgConcluido v p b =
    v ++ "\n" ++ 
    "------------------------------------------------------------\n\n\n" ++
    "Pontuação Final:\n\n" ++
    strPreto ++ ": " ++ show (p) ++ "\n" ++
    strBranco ++ ": " ++ show (b)

dlgVPreto :: String
dlgVPreto = strPreto ++ " VENCEU!"

dlgVBranco :: String
dlgVBranco = strBranco ++ " VENCEU!"

dlgVEmpate :: String
dlgVEmpate = "JOGO EMPATADO!"

