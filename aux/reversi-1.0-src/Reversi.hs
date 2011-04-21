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
--  Arquivo: Reversi.hs                                                                       --
--      Módulo principal do jogo. Contém os elementos da janela principal.                    --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Main where

-- Importando módulos necessários
import Graphics.UI.WX
import Graphics.UI.WXCore
import Tipos
import Regras
import Auxiliar
import Mensagens


main :: IO ()
main
  = start gui


gui :: IO ()
gui = do

    --------------------------------------------------------------------------------
    -- CRIANDO OS ELEMENTOS PRINCIPAIS DA JANELA -----------------------------------
    --------------------------------------------------------------------------------


    -- Cria a janela do jogo
    f             <- frameFixed [text := janelaTitulo]

    -- Cria os paineis da janela (fundo e placar)
    p1            <- panel f [clientSize := sz 480 480]
    p2            <- panel f [clientSize := sz 480 48]

    -- Um painel para cada posição do tabuleiro 8x8
    p_1_1         <- panel p1 [clientSize := sz 48 48]
    p_1_2         <- panel p1 [clientSize := sz 48 48]
    p_1_3         <- panel p1 [clientSize := sz 48 48]
    p_1_4         <- panel p1 [clientSize := sz 48 48]
    p_1_5         <- panel p1 [clientSize := sz 48 48]
    p_1_6         <- panel p1 [clientSize := sz 48 48]
    p_1_7         <- panel p1 [clientSize := sz 48 48]
    p_1_8         <- panel p1 [clientSize := sz 48 48]

    p_2_1         <- panel p1 [clientSize := sz 48 48]
    p_2_2         <- panel p1 [clientSize := sz 48 48]
    p_2_3         <- panel p1 [clientSize := sz 48 48]
    p_2_4         <- panel p1 [clientSize := sz 48 48]
    p_2_5         <- panel p1 [clientSize := sz 48 48]
    p_2_6         <- panel p1 [clientSize := sz 48 48]
    p_2_7         <- panel p1 [clientSize := sz 48 48]
    p_2_8         <- panel p1 [clientSize := sz 48 48]

    p_3_1         <- panel p1 [clientSize := sz 48 48]
    p_3_2         <- panel p1 [clientSize := sz 48 48]
    p_3_3         <- panel p1 [clientSize := sz 48 48]
    p_3_4         <- panel p1 [clientSize := sz 48 48]
    p_3_5         <- panel p1 [clientSize := sz 48 48]
    p_3_6         <- panel p1 [clientSize := sz 48 48]
    p_3_7         <- panel p1 [clientSize := sz 48 48]
    p_3_8         <- panel p1 [clientSize := sz 48 48]

    p_4_1         <- panel p1 [clientSize := sz 48 48]
    p_4_2         <- panel p1 [clientSize := sz 48 48]
    p_4_3         <- panel p1 [clientSize := sz 48 48]
    p_4_4         <- panel p1 [clientSize := sz 48 48]
    p_4_5         <- panel p1 [clientSize := sz 48 48]
    p_4_6         <- panel p1 [clientSize := sz 48 48]
    p_4_7         <- panel p1 [clientSize := sz 48 48]
    p_4_8         <- panel p1 [clientSize := sz 48 48]

    p_5_1         <- panel p1 [clientSize := sz 48 48]
    p_5_2         <- panel p1 [clientSize := sz 48 48]
    p_5_3         <- panel p1 [clientSize := sz 48 48]
    p_5_4         <- panel p1 [clientSize := sz 48 48]
    p_5_5         <- panel p1 [clientSize := sz 48 48]
    p_5_6         <- panel p1 [clientSize := sz 48 48]
    p_5_7         <- panel p1 [clientSize := sz 48 48]
    p_5_8         <- panel p1 [clientSize := sz 48 48]

    p_6_1         <- panel p1 [clientSize := sz 48 48]
    p_6_2         <- panel p1 [clientSize := sz 48 48]
    p_6_3         <- panel p1 [clientSize := sz 48 48]
    p_6_4         <- panel p1 [clientSize := sz 48 48]
    p_6_5         <- panel p1 [clientSize := sz 48 48]
    p_6_6         <- panel p1 [clientSize := sz 48 48]
    p_6_7         <- panel p1 [clientSize := sz 48 48]
    p_6_8         <- panel p1 [clientSize := sz 48 48]

    p_7_1         <- panel p1 [clientSize := sz 48 48]
    p_7_2         <- panel p1 [clientSize := sz 48 48]
    p_7_3         <- panel p1 [clientSize := sz 48 48]
    p_7_4         <- panel p1 [clientSize := sz 48 48]
    p_7_5         <- panel p1 [clientSize := sz 48 48]
    p_7_6         <- panel p1 [clientSize := sz 48 48]
    p_7_7         <- panel p1 [clientSize := sz 48 48]
    p_7_8         <- panel p1 [clientSize := sz 48 48]

    p_8_1         <- panel p1 [clientSize := sz 48 48]
    p_8_2         <- panel p1 [clientSize := sz 48 48]
    p_8_3         <- panel p1 [clientSize := sz 48 48]
    p_8_4         <- panel p1 [clientSize := sz 48 48]
    p_8_5         <- panel p1 [clientSize := sz 48 48]
    p_8_6         <- panel p1 [clientSize := sz 48 48]
    p_8_7         <- panel p1 [clientSize := sz 48 48]
    p_8_8         <- panel p1 [clientSize := sz 48 48]




    --------------------------------------------------------------------------------
    -- DEFININDO OS ELEMENTOS DA BARRA DE MENU E STATUS DA JANELA ------------------
    --------------------------------------------------------------------------------


    -- Menu 'Jogo'
    m_jogo        <- menuPane           [text := menuJogo]
    m_novo1       <- menuItem m_jogo    [text := menuNovoJogoCPU, help := helpNovoJogoCPU]
    m_novo2       <- menuItem m_jogo    [text := menuNovoJogo2J, help := helpNovoJogo2J]
    m_fecha       <- menuItem m_jogo    [text := menuFecha, help := helpFecha]
    m_jogo_l      <- menuLine m_jogo    -- separador
    m_sair        <- menuQuit m_jogo    [text := menuSair, help := helpSair]

    -- Menu 'Opções'
    m_opcoes      <- menuPane           [text := menuOpcoes]
    m_avisar      <- menuItem m_opcoes  [text := menuAvisar, help := helpAvisar]
    m_opcoes_l    <- menuLine m_opcoes  -- separador
    m_skins       <- menuPane           [text := menuSkins]
    m_skins_sub   <- menuSub  m_opcoes  m_skins []
    m_skin_1      <- menuRadioItem      m_skins [text := menuSkin1]
    m_skin_2      <- menuRadioItem      m_skins [text := menuSkin2]
    m_skin_3      <- menuRadioItem      m_skins [text := menuSkin3]
    m_skin_4      <- menuRadioItem      m_skins [text := menuSkin4]

    -- Menu 'Ajuda'
    m_ajuda       <- menuHelp           [text := menuAjuda]
    m_regras      <- menuItem m_ajuda   [text := menuRegras, help := helpRegras]
    m_sobre       <- menuAbout m_ajuda  [text := menuSobre, help := helpSobre]

    -- Barra de status
    status        <- statusField        [text := ""]




    --------------------------------------------------------------------------------
    -- DEFININDO AS VARIÁVEIS E AMBIENTE DO JOGO -----------------------------------
    --------------------------------------------------------------------------------


    -- Variável que armazena o odo de jogo
    -- (0 = nenhum, 1 = contra CPU, 2 = dois jogadores)
    modo          <- variable [value := 0]

    -- Variável que liga ou desliga o aviso de jogada inválida
    aviso         <- variable [value := True]

    -- Variável que armazena o tabuleiro atual do jogo
    tabuleiro     <- variable [value := tabZerado]
    
    -- Variável que armazena a vez de jogar
    vez           <- variable [value := Vazio]

    -- Variável que armazena o skin utilizado no momento
    skin          <- variable [value := ""]

    -- Lista de posições para serem maniplados por outras funções
    posicoes      <- toIO [p_1_1, p_1_2, p_1_3, p_1_4, p_1_5, p_1_6, p_1_7, p_1_8,
                           p_2_1, p_2_2, p_2_3, p_2_4, p_2_5, p_2_6, p_2_7, p_2_8,
                           p_3_1, p_3_2, p_3_3, p_3_4, p_3_5, p_3_6, p_3_7, p_3_8,
                           p_4_1, p_4_2, p_4_3, p_4_4, p_4_5, p_4_6, p_4_7, p_4_8,
                           p_5_1, p_5_2, p_5_3, p_5_4, p_5_5, p_5_6, p_5_7, p_5_8,
                           p_6_1, p_6_2, p_6_3, p_6_4, p_6_5, p_6_6, p_6_7, p_6_8,
                           p_7_1, p_7_2, p_7_3, p_7_4, p_7_5, p_7_6, p_7_7, p_7_8,
                           p_8_1, p_8_2, p_8_3, p_8_4, p_8_5, p_8_6, p_8_7, p_8_8]

    -- Cria a estrutura Ambiente com variáveis e elementos do jogo
    amb           <- toIO (f, tabuleiro, modo, vez, aviso, skin, posicoes, p1, p2, m_fecha)




    --------------------------------------------------------------------------------
    -- DEFININDO PROPRIEDADES DE ALGUNS ELEMENTOS ----------------------------------
    --------------------------------------------------------------------------------


    -- Menu 'Jogo'
    set m_novo1   [on command := novoJogoP amb 1]
    set m_novo2   [on command := novoJogoP amb 2]
    set m_fecha   [on command := fecharJogoP amb, enabled := False]

    -- Menu 'Opções'
    set m_sair    [on command := close f]
    set m_avisar  [on command := mudaAviso aviso m_avisar, checkable := True, checked := True]
    set m_skin_1  [on command := aplicaSkin amb "padrao", checked := True]
    set m_skin_2  [on command := aplicaSkin amb "madeira"]
    set m_skin_3  [on command := aplicaSkin amb "metal"]
    set m_skin_4  [on command := aplicaSkin amb "vidro"]

    -- Menu 'Ajuda'
    set m_regras  [on command := infoDialog f msgRegrasT msgRegras]
    set m_sobre   [on command := infoDialog f msgSobreT msgSobre]

    -- Define a disposição dos elementos na janela
    set f [statusBar := [status],
           menuBar := [m_jogo, m_opcoes, m_ajuda],
           layout := column 0 [row 0 [widget p1], row 1 [widget p2]],
           clientSize := sz 480 528]

    -- Define a disposição dos elementos do tabuleiro
    set p1 [layout := floatCentre $ column 0 [
            row 1 [row 0 [widget p_1_1, widget p_1_2, widget p_1_3, widget p_1_4,
                          widget p_1_5, widget p_1_6, widget p_1_7, widget p_1_8]],
            row 2 [row 0 [widget p_2_1, widget p_2_2, widget p_2_3, widget p_2_4,
                          widget p_2_5, widget p_2_6, widget p_2_7, widget p_2_8]],
            row 3 [row 0 [widget p_3_1, widget p_3_2, widget p_3_3, widget p_3_4,
                          widget p_3_5, widget p_3_6, widget p_3_7, widget p_3_8]],
            row 4 [row 0 [widget p_4_1, widget p_4_2, widget p_4_3, widget p_4_4,
                          widget p_4_5, widget p_4_6, widget p_4_7, widget p_4_8]],
            row 5 [row 0 [widget p_5_1, widget p_5_2, widget p_5_3, widget p_5_4,
                          widget p_5_5, widget p_5_6, widget p_5_7, widget p_5_8]],
            row 6 [row 0 [widget p_6_1, widget p_6_2, widget p_6_3, widget p_6_4,
                          widget p_6_5, widget p_6_6, widget p_6_7, widget p_6_8]],
            row 7 [row 0 [widget p_7_1, widget p_7_2, widget p_7_3, widget p_7_4,
                          widget p_7_5, widget p_7_6, widget p_7_7, widget p_7_8]],
            row 8 [row 0 [widget p_8_1, widget p_8_2, widget p_8_3, widget p_8_4,
                          widget p_8_5, widget p_8_6, widget p_8_7, widget p_8_8]]],
            clientSize := sz 480 480]




    --------------------------------------------------------------------------------
    -- APLICANDO OS GRÁFICOS DO JOGO -----------------------------------------------
    --------------------------------------------------------------------------------


    -- Aplica o ícone da janela
    frameSetIconFromFile f "reversi.ico"

    -- Aplica o skin padrão
    aplicaSkin amb "padrao"

