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
--  Arquivo: Auxiliar.hs                                                                      --
--      Módulo auxiliar ao principal, constituído basicamente por funções de IO.              --
------------------------------------------------------------------------------------------------
--  Última Modificação: 03/06/2005                                                            --
------------------------------------------------------------------------------------------------

module Auxiliar (
    aplicaSkin,
    novoJogoP,
    fecharJogoP,
    mudaAviso
    ) where

-- Importando módulos necessários
import Graphics.UI.WX
import Graphics.UI.WXCore
import Tipos
import Regras
import Mensagens




--------------------------------------------------------------------------------
-- FUNÇÕES PARA ATUALIZAÇÃO DE GRÁFICOS ----------------------------------------
--------------------------------------------------------------------------------

-- Atualiza a figura mostrada em uma posição do tabuleiro
atualizaPosicao :: Ambiente -> Panel () -> Estado -> IO ()
atualizaPosicao a p e = do
    set p [on paint := aux a e]
    repaint p
    where
        aux a e dc _ = do
            s <- get (ambSkn a) value
            case e of
                Preto -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/preto.bmp")
                    drawBitmap dc bmp (pt 0 0) False []
                Branco -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/branco.bmp")
                    drawBitmap dc bmp (pt 0 0) False []
                Vazio -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/vazio.bmp")
                    drawBitmap dc bmp (pt 0 0) False []


-- Atualiza as figuras do placar de acordo com a situação do jogo
atualizaPlacar :: Ambiente -> IO ()
atualizaPlacar a = do
    v <- get (ambVez a) value
    t <- get (ambTbl a) value
    s <- get (ambSkn a) value
    qtdp <- toIO (aux2 (show (qtdPreto t)))
    qtdb <- toIO (aux2 (show (qtdBranco t)))
    set (ambPn2 a) [on paint := aux3 s (aux1 v) qtdp qtdb]
    repaint (ambPn2 a)
    where
        aux1 v
            | v == Preto  = "vezpreto"
            | v == Branco = "vezbranco"
            | otherwise   = ""
        aux2 [x]     = ("0", [x])
        aux2 (x:y:_) = ([x], [y])
        aux3 s v (p1, p2) (b1, b2) dc viewArea = do
            bmp_fundo <- bitmapCreateFromFile ("skins/" ++ s ++ "/placar.bmp")
            drawBitmap dc bmp_fundo (pt 0 0) False []
            if (v /= "")
                then do
                    bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ v ++ ".gif"))
                    drawBitmap dc bmp (pt 10 10) True []
                    bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ p1 ++ ".gif"))
                    drawBitmap dc bmp (pt 350 10) True []
                    bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ p2 ++ ".gif"))
                    drawBitmap dc bmp (pt 364 10) True []
                    bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ b1 ++ ".gif"))
                    drawBitmap dc bmp (pt 435 10) True []
                    bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ b2 ++ ".gif"))
                    drawBitmap dc bmp (pt 449 10) True []
                else do
                    return ()


-- Atualiza a figura do fundo de acordo com o skin atual
atualizaFundo :: Ambiente -> IO ()
atualizaFundo a = do
    set (ambPn1 a) [on paint := aux a]
    repaint (ambPn1 a)
    where
        aux a dc _ = do
            s <- get (ambSkn a) value
            bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/fundo.bmp")
            drawBitmap dc bmp (pt 0 0) False []


-- Muda o skin do jogo e atualiza as figuras
aplicaSkin :: Ambiente -> String -> IO ()
aplicaSkin a s = do
    set (ambSkn a) [value := s]
    t1 <- get (ambTbl a) value
    atualiza a t1 (ambPos a)
    atualizaFundo a
    atualizaPlacar a


-- Atualiza todas as posições do tabuleiro de acordo com a situação do jogo
atualiza :: Ambiente -> Tabuleiro -> [Panel ()] -> IO ()
atualiza _ [] [] = do {return ()}
atualiza a ((x, y, e):ts) (p:ps) = do
    atualizaPosicao a p e
    atualiza a ts ps




--------------------------------------------------------------------------------
-- FUNÇÕES PARA EFETIVAÇÃO DE JOGADAS ------------------------------------------
--------------------------------------------------------------------------------

-- Efetiva ou rejeita uma jogada realizada por um jogador
jogar :: Ambiente -> (Int, Int) -> Point -> IO ()
jogar a (x, y) _ = do
    t0 <- get (ambTbl a) value
    e  <- get (ambVez a) value
    m  <- get (ambMod a) value
    if (not (testaJogada t0 (x, y, e)))
        then do
            av <- get (ambAvs a) value
            if (av)
                then do
                    warningDialog (ambFrm a) dlgInvalidaT dlgInvalida
                else do
                    return ()
        else do
            set (ambTbl a) [value := executaJogada t0 (x, y, e)]
            t1 <- get (ambTbl a) value
            atualiza a t1 (ambPos a)
            atualizaPlacar a
            if (jogoConcluido t1)
                then do
                    ganhador <- toIO (mostraGanhador t1)
                    case ganhador of
                        Preto  -> do
                            infoDialog (ambFrm a) dlgConcluidoT (dlgConcluido dlgVPreto (qtdPreto t1) (qtdBranco t1))
                        Branco -> do
                            infoDialog (ambFrm a) dlgConcluidoT (dlgConcluido dlgVBranco (qtdPreto t1) (qtdBranco t1))
                        Vazio  -> do
                            infoDialog (ambFrm a) dlgConcluidoT (dlgConcluido dlgVEmpate (qtdPreto t1) (qtdBranco t1))
                    resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
                    if (resp)
                        then do
                            novoJogo a m
                        else do
                            fecharJogo a
                else do
                    if (jogadasPossiveis t1 (oposto e))
                        then do
                            set (ambVez a) [value := oposto e]
                            atualizaPlacar a
                        else do
                            infoDialog (ambFrm a) dlgJogaNovamenteT (dlgJogaNovamente (strEstado (oposto e)) (strEstado e))
                    e1 <- get (ambVez a) value
                    if (e1 == Branco && m == 1)
                        then do
                            jogarCPU a
                        else do
                            return ()


-- Faz a CPU jogar, com delay de 1 segundo, após a jogada do jogador
jogarCPU :: Ambiente -> IO ()
jogarCPU a = do
    desativaJogo (ambPos a)
    tmp <- timer (ambFrm a) [interval := 1000]
    set tmp [on command := aux1 a tmp]
    where
        aux1 a tmp = do
            set tmp [enabled := False]
            t0 <- get (ambTbl a) value
            jogar a (aux2 (melhorJogada t0 Branco)) (pt 0 0)
            ativaJogo a t0 (ambPos a)
        aux2 (x, y, _) = (x, y)




--------------------------------------------------------------------------------
-- FUNÇÕES PARA MUDAR PARÂMETROS DO JOGO ---------------------------------------
--------------------------------------------------------------------------------

-- Ativa a ação do click nas posições do tabuleiro
ativaJogo :: Ambiente -> Tabuleiro -> [Panel ()] -> IO ()
ativaJogo _ [] [] = do {return ()}
ativaJogo a ((x, y, e):ts) (p:ps) = do
    set p [on click := jogar a (x, y)]
    ativaJogo a ts ps


-- Desativa a ação do click nas posições do tabuleiro
desativaJogo :: [Panel ()] -> IO ()
desativaJogo [] = do {return ()}
desativaJogo (p:ps) = do
    set p [on click := nada]
    desativaJogo ps
    where
        nada _ = do {return ()}


-- Inicia um novo jogo
novoJogo :: Ambiente -> Int -> IO ()
novoJogo a m = do
    t0 <- get (ambTbl a) value
    ativaJogo a t0 (ambPos a)
    atualiza a tabInicial (ambPos a)
    set (ambFch a) [enabled := True]
    set (ambMod a) [value := m]
    set (ambTbl a) [value := tabInicial]
    set (ambVez a) [value := Preto]
    atualizaPlacar a


-- Pergunta se o usuário deseja iniciar um novo jogo
novoJogoP :: Ambiente -> Int -> IO ()
novoJogoP a m = do
    resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
    if (resp)
        then do
            novoJogo a m
        else do
            return ()


-- Encerra a partida ativa, caso exista
fecharJogo :: Ambiente -> IO ()
fecharJogo a = do
    desativaJogo (ambPos a)
    atualiza a tabZerado (ambPos a)
    set (ambFch a) [enabled := False]
    set (ambMod a) [value := 0]
    set (ambTbl a) [value := tabZerado]
    set (ambVez a) [value := Vazio]
    atualizaPlacar a


-- Pergunta se o usuário deseja encerrar a partida ativa
fecharJogoP :: Ambiente -> IO ()
fecharJogoP a = do
    resp <- confirmDialog (ambFrm a) dlgFecharT dlgFechar False
    if (resp)
        then do
            fecharJogo a
        else do
            return ()


-- Muda a variável que avisa quando uma jogada é inválida
mudaAviso :: Var Bool -> MenuItem () -> IO ()
mudaAviso a m = do
    av <- get a value
    if (av)
        then do
            set a [value := False]
            set m [checked := False]
        else do
            set a [value := True]
            set m [checked := True]

