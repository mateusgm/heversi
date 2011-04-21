{-# OPTIONS -fglasgow-exts #-}

---------
-- GUI -- 
---------

module GUI (gui, version) where

import Game               -- hiding (name)
import Graphics.UI.WX     hiding (bitmap, children, click, selections, stop)
import Graphics.UI.WXCore
import Char
import Tools              hiding (field)
import List

version :: String
version = "1.7.2" 

gui :: [GeneralGame] -> IO ()
gui games = do

  {--== CREATION PHASE ==--}

  f <- mdiParentFrame []
  c <- mdiParentFrameGetClientWindow f
  
  mGame  <- menuPane        []
  iNew   <- menuItem  mGame []
  ()     <- menuLine  mGame 
  iQuit  <- menuQuit  mGame []
 
  mHelp  <- menuHelp        []
  iHelp  <- menuItem  mHelp []
  iAbout <- menuAbout mHelp []

  field  <- statusField []

  logo   <- bitmapCreateLoad "images\\gebop.bmp" wxBITMAP_TYPE_ANY

  {--== DEFENITION PHASE ==--}

  let onpaint :: DC () -> Rect -> IO ()
      onpaint dc r = do
        tileBitmap dc r logo

  {--== MODIFICATION PHASE ==--}

  set mGame  [ text := "&Game"                                                      ]
  set iNew   [ text := "&New Game\tCtrl+N", help := "Start a new game"              , on command := newgame f games] 
  set iQuit  [                              help := "Quit the application"          , on command := close f] 
  set iHelp  [ text := "&Contents"        , help := "Show the contents of GeBoP"    , on command := html f "help\\index.html"]
  set iAbout [                              help := "Information about this program", on command := infoDialog f "About GeBoP" about]

  set f [ visible           := True
        , clientSize        := sz 640 480
        , picture           := "gebop.ico"
        , text              := "GeBoP"
        , menuBar           := [mGame, mHelp] 
        , statusBar         := [field]
        ]
  
  set c [ on paint    := onpaint
        , on resize  ::= repaint
        ]
  
  return ()

newgame :: MDIParentFrame () -> [GeneralGame] -> IO ()
newgame mdiparent games = do 
  mi <- askGame
  case mi of 
    Nothing -> return ()
    Just i  -> do
      let gc = games !! i
          pr = (\(Game g) -> standard g) gc
          ra = (\(Game g) -> possible g) gc
      msettings <- askSettings pr ra
      case msettings of 
        Nothing -> return ()
        Just settings -> do
          (\(Game g) -> game mdiparent g settings (newgame mdiparent games)) $ gc
          return ()
          
  where
  
    askGame :: IO (Maybe Int)
    askGame = do
      d   <- dialog     mdiparent [text := "Choose a game"]
      mes <- staticText d [text := "There are currently " ++ numberword (length games) ++ " games you can play."]
      ok  <- button     d [text := "Play", tooltip := "click to proceed"]
      can <- button     d [text := "Cancel", tooltip := "click to cancel"]
      r   <- radioBox   d Vertical (map (\(Game g) -> name g) games) [tooltip := "select a game"]
      set d [ layout := margin 4 $ column 4
              [ widget mes
              , hfill        $ widget r
              , hfloatCentre $ row 4 [widget ok, widget can]
              ]
            ]
      showModal d (\stop -> do
        set ok  [on command := get r selection >>= stop . Just]
        set can [on command := stop Nothing]
                            )
      
    askSettings :: Properties -> PropertyRange -> IO (Maybe Properties)
    askSettings pr ra = do
      let pra = playersrange ra
          bra = sort $ boardsizerange ra
      d <- dialog mdiparent [text := "Settings"]
      mes <- staticText d [text := "Select the settings."]
      ok  <- button     d [text := "Play", tooltip := "click to start the game"]
      can <- button     d [text := "Cancel", tooltip := "click to cancel"]
      cbs <- sequence $ replicate (maximum pra) $ checkBox d [tooltip := "should this player participate?"]
      sts <- sequence $ map (\i -> staticText d [text := "player " ++ show i]) [1 .. maximum pra]
      rbs <- sequence $ replicate (maximum pra) $ radioBox d Horizontal ["Human", "Computer"] [tooltip := "will you play this player?"]
      sequence_ $ map (\i -> set (cbs !!! (i - 1)) [enabled := False]) [1 .. minimum pra]
      let check b i = do set (cbs !!! (i - 1)) [checked := b]
                         set (sts !!! (i - 1)) [enabled := b]
                         set (rbs !!! (i - 1)) [enabled := b]
          click True  i | i `elem` pra = do for 1 i $ check True
                        | otherwise    = click True (i + 1)
          click False i | (i - 1) `elem` pra = do for i (maximum pra) $ check False
                        | otherwise          = click False (i - 1)
      for 1 (maximum pra) (\i -> check False i)
      click True $ players pr
      sequence_ $ zipWith (\i h -> set (rbs !!! (i - 1)) [selection := if h then 0 else 1]) [1 ..] (human pr)
      sequence_ $ map (\i -> set (cbs !!! (i - 1)) [on command ::= \cb -> get cb checked >>= flip click i]) [1 .. maximum pra]
      sli <- if length (bra) == 1 then return Nothing
             else fmap Just $ hslider d False 0 (length bra - 1) [selection := maybe 0 id $ elemIndex (boardsize pr) bra, tooltip := "select the size of the board"]
      let hcselection = map (\i -> [vfloatCentre $ widget $ cbs !!! (i - 1), vfloatCentre $ widget $ sts !!! (i - 1), widget $ rbs !!! (i - 1)]) [1 .. maximum pra]
          selections = case sli of
            Nothing -> grid 4 4 hcselection
            Just s  -> column 4 [ grid 4 4 hcselection
                                , hfill $ widget s
                                , row 4 [label "small", glue, label "large"]
                                ]
      set d [ layout := margin 4 $ column 4
              [ widget mes
              , selections
              , hfloatCentre $ row 4 [widget ok, widget can]
              ]
            ]
      showModal d (\stop -> do
        set ok  [on command := do
          p <- fmap (length . takeWhile id) $ sequence $ map (flip get checked) cbs
          h <- sequence $ (map.fmap) (== 0) $ map (flip get selection) rbs
          b <- case sli of Nothing -> return $ boardsize pr
                           Just s -> fmap (bra !!!) $ get s selection
          stop $ Just Properties {players = p, human = h, boardsize = b}]
        set can [on command := stop Nothing]
                            )

game :: Game g => MDIParentFrame () -> g -> Properties -> IO () -> IO ()
game mdiparent g pr newg = do

  {--== CREATION PHASE ==--}

  f <- mdiChildFrame mdiparent []
  
  mGame  <- menuPane         []
  iNew   <- menuItem  mGame  []
  iAgain <- menuItem  mGame  []
  ()     <- menuLine  mGame 
  iClose <- menuItem  mGame  []
  iQuit  <- menuQuit  mGame  []
  mBrain <- menuPane         []
  iOpen  <- menuItem  mBrain []
  mHelp  <- menuHelp         []
  iHelp  <- menuItem  mHelp  []
  iRules <- menuItem  mHelp  []
  iInfo  <- menuItem  mHelp  []
  iAbout <- menuAbout mHelp  []

  logoblue   <- bitmap "blue"
  logored    <- bitmap "red"
  logogreen  <- bitmap "green"
  logopurple <- bitmap "purple"
  logobrown  <- bitmap "brown"
  logogrey   <- bitmap "grey"

  turn   <- bitmap "turn"
  winner <- bitmap "winner"

  humanbmp    <- bitmap "human"
  computerbmp <- bitmap "computer"

  vart <- varCreate $ createtree g pr
  varb <- varCreate $ emptyBrain

  p <- panel f []

  clock <- timer f []
                    
  ps <- sequence $ replicate (players pr) $ panel f []
  ss <- sequence $ map (\p' -> hslider p' False 0 10 []) ps
  bs <- sequence $ map (\p' -> singleListBox p' []) ps

--{  field <- statusField []

  {--== DEFENITION PHASE ==--}

  let 
  
    stopClock :: IO ()
    stopClock = do timerStop clock
                   info "clock stopped"

    ifhuman :: IO () -> IO ()
    ifhuman io = do t <- getTree
                    if human pr !!! player t && movesnr t > 0 then io else return ()

    ifcomputer :: IO () -> IO ()
    ifcomputer io = do t <- getTree
                       if human pr !!! player t || movesnr t == 0 then return () else io
  
    getTree = varGet vart
    setTree = varSet vart
    updateTree = varUpdate vart
  
    logo :: Int -> Bitmap ()
    logo 0 = logoblue
    logo 1 = logored
    logo 2 = logogreen
    logo 3 = logopurple
    logo 4 = logobrown
    logo 5 = logogrey
    logo _ = error "logo: Unexpected value"
    
    onpaintplayer :: Int -> DC () -> Rect -> IO ()
    onpaintplayer i dc (Rect _x _y w _h) = do
      let xi = (w - 40) `div` 2
      drawBitmap dc (logo i) (pt xi 10) True []
      t <- getTree
      drawRect dc (Rect xi 60 40 40) [brushKind := BrushTransparent]
      for 0 (floor $ 19 * val t !!! i) (\j -> line dc (pt (xi + 1) (80 - j)) (pt (xi + 39) (80 - j)) [penColor := green])
      for (floor $ 18 * val t !!! i) 0 (\j -> line dc (pt (xi + 1) (80 - j)) (pt (xi + 39) (80 - j)) [penColor := red  ])
      line dc (pt xi 80) (pt (xi + 40) 80) []
      if player t == i && movesnr t > 0 then drawBitmap dc turn (pt xi 110) True [] else return ()
      drawBitmap dc (if human pr !!! i then humanbmp else computerbmp) (pt xi 160) True []
      ifIO (win i) $ drawBitmap dc winner (pt xi 110) True []

    oncommand i = do
      t <- getTree
      when (player t == i) $ send clock

    playerpanel i p' sli box = do
      but <- button p' []
      stt <- staticText p' []
      itemAppend box $ ""

      set sli [ clientSize := Size 60 20
              , color      := setLum 0.25 $ colorplayer i
              , bgcolor    := setLum 0.95 $ colorplayer i
              , on command := do v <- sliderGetValue sli
                                 set stt [text := show ((2 :: Int) ^ v) ++ " sec"]
              , visible    := not (human pr !!! i)
              , selection  := 3
              ]
      set stt [ clientSize := Size 70 20
              , text       := "8 sec"
              , color      := setLum 0.25 $ colorplayer i
              , bgcolor    := setLum 0.95 $ colorplayer i
              , visible    := not (human pr !!! i)
              ]
      set but [ clientSize := sz 60 20
              , text       := "move now"
              , color      := setLum 0.25 $ colorplayer i
              , bgcolor    := setLum 0.95 $ colorplayer i
              , on command := oncommand i
              , visible    := not (human pr !!! i)
              ] 
      set box [ clientSize := Size 60 20
              , color      := setLum 0.25 $ colorplayer i
              , bgcolor    := setLum 0.95 $ colorplayer i
              , on select  := do info ("select " ++ show i)
                                 s <- get box selection
                                 mapM_ (\b -> set b [selection := s]) bs
              ]
      set p' [ bgcolor    := setLum 0.95 $ colorplayer i
             , clientSize := Size 80 0
             , on paint   := onpaintplayer i
             , layout     := margin 4 $ column 4
               [ space 72 210
               , hfill                $ widget sli
               ,         hfloatCentre $ widget stt
               ,         hfloatCentre $ widget but
               , vfill $ hfloatCentre $ widget box
               ]
             ]
      return ()

    boxInsert :: Player -> String -> IO ()
    boxInsert p' s = do
      let b = (bs !!! p')
      i <- get b itemCount
      vs <- mapM (\p'' -> get (bs !!! p'') (item $ i - 1)) [p' .. length bs - 1]
      when (not $ all null vs) $ mapM_ (\b' -> itemAppend b' "") bs
      i' <- get b itemCount
      set b [item (i' - 1) := s]
      mapM_ (\b' -> set b' [selection := i' - 1]) bs

    win :: Int -> IO Bool
    win i' = do
      t <- getTree
      return $ (movesnr t == 0) && (val t !!! i' == 1)

    gamename = (\(c : cs) -> toUpper c : cs) $ name g

    repaintall :: IO ()
    repaintall = do repaint p
                    sequence_ $ map repaint ps
  
    startClock :: IO ()
    startClock = do t <- getTree
                    v <- sliderGetValue $ ss !!! player t
                    let int = 1000 * 2 ^ v
                    _ <- timerStart clock int True
                    info "clock started"
                    
    next :: Int -> IO ()
    next n = do t <- getTree
                _ <- updateTree (shear n)
                u <- getTree
                b <- varGet varb
                bRefresh b
                boxInsert (player t) $ showmove pr (player t) (state t) (childid u)
                repaintall
                if movesnr t == 0
                  then do infoDialog f "End of Game" $ "The game ended with value " ++ show (val u)
                          info "end of this game"
                  else do ifcomputer $ info "computer's turn" >> think >> startClock
                          ifhuman $ info "player's turn"

    onidle :: IO Bool
    onidle = do ifcomputer $ do think
                                u <- getTree
                                when (movesnr u == 1) movenow
                                when (closed u) movenow
                --{ andere constructie (ook denken in spelertijd?)
                t <- getTree
                return $ human pr !!! player t

    think :: IO ()
    think = do t <- getTree
               js <- randomList
               let p' = path followcombination js t
--{               let p = path followbest js t
               _ <- updateTree $ step p'
               u <- getTree

               when (val t /= val u) $ sequence_ $ map repaint ps
               b <- varGet varb
               bUpdatePath b p'

               return ()

    movenow :: IO ()
    movenow = ifcomputer $ do
      stopClock
      t <- getTree
      i <- randomElement $ best t
      info "computer moves"
      next i

    newquick :: IO ()
    newquick = do stopClock
                  setTree $ createtree g pr
                  info "new game started"
                  sequence_ $ map (\b -> set b [items := [" "]]) bs
                  repaintall
                  ifcomputer $ info "computer's turn" >> startClock
                  return ()

    showRules :: IO ()
    showRules = html f $ "help\\rules_" ++ gamename ++ ".html"
    
    showInfo :: IO ()
    showInfo = html f $ "help\\info_" ++ gamename ++ ".html"

    playerinput :: Int -> IO ()
    playerinput i = do
      info $ "moverequest: " ++ show i
      ifhuman $ do
        t <- getTree
        when (0 <= i && i < movesnr t) $ info "correct move" >> next i

  {--== MODIFICATION PHASE ==--}

  sequence_ $ zipWith4 playerpanel [0..] ps ss bs

  board p pr vart True playerinput

  set mGame  [ text := "&Game"                                                                                                                                      ]
  set iNew   [ text := "&New Game\tCtrl+N"        , help := "Start a new game"                                 , on command := newg                                 ]
  set iAgain [ text := "Play &Again\tCtrl+A"      , help := "Play again with the same settings"                , on command := newquick >> varGet varb >>= bRefresh ]
  set iClose [ text := "&Close\tCtrl+C"           , help := "Close this game"                                  , on command := close f                              ]
  set iQuit  [                                      help := "Quit the application"                             , on command := close mdiparent                      ]
  set mBrain [ text := "&Brain"                                                                                                                                     ]
  set iOpen  [ text := "&Open\tCtrl+B"            , help := "Open the brain for " ++ gamename                  , on command := brain mdiparent pr vart varb newg    ]
  set iHelp  [ text := "&Contents"                , help := "Show the contents of GeBoP"                       , on command := html f "help\\index.html"            ]
  set iRules [ text := gamename ++ " &Rules"      , help := "Display the rules of this game"                   , on command := showRules                            ]
  set iInfo  [ text := gamename ++ " &Information", help := "Display some background information on this game" , on command := showInfo                             ]
  set iAbout [                                      help := "Information about this program"                   , on command := infoDialog f "About GeBoP" about     ]

  set clock [ on command := info "alarm" >> movenow ]

  set p [ clientSize := sz 400 400 ]

  set f [ text              := gamename
        , menuBar           := [mGame, mBrain, mHelp] 
        , layout            := row 0 ( fill (widget p)
                                     : map (vfill . widget) ps
                                     )
        , on idle           := onidle
        , picture           := "gebop.ico"
        , on closing        :~ \io -> do b <- varGet varb
                                         bClose b
                                         io
--{        , statusBar := [field]
        ]

  ifhuman $ stopClock

  return ()

data Brain
  = Brain { bClose      :: IO ()
          , bUpdatePath :: [Int] -> IO ()
          , bRefresh    :: IO ()
          }

emptyBrain :: Brain
emptyBrain
  = Brain { bClose      =         return ()
          , bUpdatePath = const $ return ()
          , bRefresh    =         return ()
          }

brain :: Game g => MDIParentFrame () -> Properties -> Var (Tree g) -> Var Brain -> IO () -> IO ()
brain mdiparent pr vart varb newg = do

  {--== CREATION PHASE ==--}

  f <- mdiChildFrame mdiparent []
  sw <- splitterWindow f []
 
  pLeft  <- panel sw []
  pRight <- panel sw []
  pBoard <- panel pRight []
  pValue <- panel pRight []
  pInfo  <- panel pRight []

  mGame  <- menuPane         []
  iNew   <- menuItem  mGame  []
  ()     <- menuLine  mGame
  iQuit  <- menuQuit  mGame  []
  mBrain <- menuPane         []
  iClose <- menuItem  mBrain []
  iMini  <- menuItem  mBrain []
  iLarge <- menuItem  mBrain []
  mHelp  <- menuHelp         []
  iHelp  <- menuItem  mHelp  []
  iBrain <- menuItem  mHelp  []
  iAbout <- menuAbout mHelp  []

  logos  <- mapM (bitmap.nameplayer) [0 .. 5]
  winner <- bitmap "winner"
  leeg   <- bitmap "empty"

  bPlayer   <- staticBitmapCreate pRight (-1) (logos !!! 0) rectNull (-1)

  -- tPlayer   <- staticText pRight []
  tMind     <- staticText pInfo []
  tMaxd     <- staticText pInfo []
  tVolume   <- staticText pInfo []

  tc <- treeCtrl pLeft []

  g <- varGet vart >>= return . state
  varu <- varCopy vart

  {--== DEFENITION PHASE ==--}

  let 

    gamename :: String
    gamename = (\(c : cs) -> toUpper c : cs) $ name g

    getData :: TreeItem -> IO [Int]
    getData treeItem = do
      mmovs <- unsafeTreeCtrlGetItemClientData tc treeItem 
      case mmovs of Just movs -> return movs
                    Nothing   -> return []

    setImages :: Bool -> IO ()
    setImages True = do
      il <- imageListCreate (Size 40 40) True 0
      sequence_ $ map (bitmapImageList il) $ map nameplayer [0 .. 5]
      sequence_ $ map (bitmapHighImageList il) $ map nameplayer [0 .. 5]
      bitmapImageList il "brain"
      treeCtrlAssignImageList tc il
    setImages False = do
      il <- imageListCreate (Size 10 10) True 0
      sequence_ $ map (bitmapImageList il) $ map ("small_" ++) $ map nameplayer [0 .. 5]
      sequence_ $ map (bitmapHighImageList il) $ map ("small_" ++) $ map nameplayer [0 .. 5]
      bitmapImageList il "small_brain"
      treeCtrlAssignImageList tc il

    onmini :: IO ()
    onmini = do
      set f [clientSize := sz 256 100]
      splitterWindowSetSashPosition sw 128 True
   
    onpaintValue :: DC () -> Rect -> IO ()
    onpaintValue dc (Rect x y w_ h) = do
      let pl = players pr
          w = min (w_) (20 * pl)
          r = Rect x y w h
--{      line dc (pt 0 0) (pt w h) []
--{      line dc (pt 0 h) (pt w 0) []
      u <- varGet varu
      for 0 (pl - 1) (\i -> do
        let xi  = x + w * i `div` pl
            xi1 = x + w * (i + 1) `div` pl
        for 0 (floor $ 19 * val u !!! i) (\j -> line dc (pt (xi + 1) (20 - j)) (pt (xi1) (20 - j)) [penColor := colorplayer i])
        for (floor $ 18 * val u !!! i) 0 (\j -> line dc (pt (xi + 1) (20 - j)) (pt (xi1) (20 - j)) [penColor := setLum 0.4 $ colorplayer i])
        )
      drawRect dc r [brushKind := BrushTransparent]
      line dc (pt x 20) (pt (x + w) 20) []

    onTreeEvent :: EventTree -> IO ()
    onTreeEvent (TreeItemExpanding treeItem _veto) | treeItemIsOk treeItem = do
      wxcBeginBusyCursor
      children' <- treeCtrlGetChildren tc treeItem 
      mapM_ visualise children'
      wxcEndBusyCursor
      propagateEvent
    onTreeEvent (TreeSelChanged treeItem _olditem) | treeItemIsOk treeItem = do
      wxcBeginBusyCursor
      selectRight treeItem 
      wxcEndBusyCursor
      propagateEvent
    onTreeEvent _ = propagateEvent

    visualise :: TreeItem -> IO ()
    visualise treeItem = do
      c <- treeCtrlItemHasChildren tc treeItem 
      when (c == 0) $ giveBirth treeItem 
      updateThickness treeItem 
      updateImages treeItem 

    selectRight :: TreeItem -> IO ()
    selectRight treeItem = do
      updateRight treeItem 
      t <- varGet varu
      staticBitmapSetBitmap bPlayer (logos !!! player t)
      when (movesnr t == 0) $ staticBitmapSetBitmap bPlayer (if any (== 1) (val t) then winner else leeg)
      repaint pBoard

    updateRight :: TreeItem -> IO ()
    updateRight treeItem = do
      t <- gametree treeItem 
      varSet varu t
      set tMind     [text := "complete depth: " ++ show (mind     t)]
      set tMaxd     [text := "maximum depth: "  ++ show (maxd     t)]
      set tVolume   [text := "volume: "         ++ show (volume   t)]
      repaint pValue

    bClose' :: IO ()
    bClose' = close f

    bUpdatePath' :: [Int] -> IO ()
    bUpdatePath' path' = do
      root <- treeCtrlGetRootItem tc
      updatePath root path'
     where
      updatePath :: TreeItem -> [Int] -> IO ()
      updatePath treeItem path'' = do
        updateThickness treeItem 
        updateImages treeItem 
        ifIO (treeCtrlIsSelected tc treeItem) $ updateRight treeItem 
        ifIO (treeCtrlIsExpanded tc treeItem) $ case path'' of
          (i:is) -> do cs <- treeCtrlGetChildren tc treeItem 
                       updatePath (cs !! i) is
          [] -> return ()

    bRefresh' :: IO ()
    bRefresh' = do
      treeCtrlDeleteAllItems tc
--{      root <- treeCtrlAddRoot tc "current situation" 5 (-1) objectNull
      root <- treeCtrlAddRoot tc "current situation" 12 12 objectNull
      treeCtrlSetItemClientData tc root (return ()) []
      giveBirth root
      updateThickness root
      updateImages root
      treeCtrlSelectItem tc root
      selectRight root

    giveBirth :: TreeItem -> IO ()
    giveBirth treeItem = do
      movs <- getData treeItem 
      t <- gametree treeItem 
      for 0 (movesnr t - 1) (\i -> do
        let -- u = shear i t
            m = showmove pr (player t) (state t) i
--{        jtem <- treeCtrlAppendItem tc treeItem (show i ++ " (" ++ m ++ ")") (player t) (-1) objectNull 
        jtem <- treeCtrlAppendItem tc treeItem (show i ++ " (" ++ m ++ ")") (-1) (-1) objectNull 
        treeCtrlSetItemClientData tc jtem (return ()) (movs ++ [i])
        updateThickness jtem
        )
      treeCtrlSetItemHasChildren tc treeItem (movesnr t > 0)

    updateThickness :: TreeItem -> IO ()
    updateThickness treeItem = do
      t <- gametree treeItem 
      let itemcolor | filled t  = black
                    | otherwise = grey
      treeCtrlSetItemTextColour tc treeItem itemcolor
      treeCtrlSetItemBold       tc treeItem (closed t)
  
    updateImages :: TreeItem -> IO ()
    updateImages treeItem = do
      t <- gametree treeItem 
      c <- treeCtrlItemHasChildren tc treeItem 
      cs <- treeCtrlGetChildren tc treeItem 
      when (c > 0) $ for 0 (movesnr t - 1) (\i -> do
        let offset | i `elem` best t = 6
                   | otherwise       = 0
        setImage (cs !! i) (offset + player t)
        )

    setImage :: TreeItem -> Int -> IO ()
    setImage treeItem i = do
      j <- treeCtrlGetItemImage tc treeItem 0 --{ wxTreeItemIcon_Normal
      when (i /= j) $ do
        treeCtrlSetItemImage tc treeItem i 0 --{ wxTreeItemIcon_Normal
        treeCtrlSetItemImage tc treeItem i 1 --{ wxTreeItemIcon_Selected
--{        treeCtrlSetItemImage tc treeItem i 2 --{ wxTreeItemIcon_Expanded
--{        treeCtrlSetItemImage tc treeItem i 3 --{ wxTreeItemIcon_SelectedExpanded

--  gametree :: TreeItem -> IO (Tree g)
    gametree treeItem = do
      t <- varGet vart
      movs <- getData treeItem 
      return $ gametree_ movs t
     where
--    gametree_ :: [Int] -> Tree g -> Tree g
      gametree_ (i:is) t | i >= 0 && i < movesnr t = gametree_ is $ shear i t
      gametree_ _      t = t


    playerinput :: Int -> IO ()
    playerinput i = do
      info $ "brain moverequest: " ++ show i
      treeItem <- treeCtrlGetSelection tc
      t <- gametree treeItem 
      cs <- treeCtrlGetChildren tc treeItem 
      when (0 <= i && i < movesnr t) $ do
        treeCtrlExpand tc treeItem 
        treeCtrlSelectItem tc (cs !! i)

  {--== MODIFICATION PHASE ==--}

  board pBoard pr varu False playerinput

  bRefresh'
--  setImages True
  setImages False
        

  varSet varb $ Brain bClose' bUpdatePath' bRefresh'

  set mGame  [ text := "&Game"                                                                                                                                     ] 
  set iNew   [ text := "&New Game\tCtrl+N"        , help := "Start a new game"                                 , on command  := newg                               ]
  set iQuit  [                                      help := "Quit the application"                             , on command  := close mdiparent                    ] 
  set mBrain [ text := "&Brain"                                                                                                                                    ] 
  set iClose [ text := "&Close\tCtrl+C"           , help := "Close the brain for " ++ gamename                 , on command  := close f                            ]  
  set iMini  [ text := "&Minimal\tCtrl+M"         , help := "Minimize the brain size"                          , on command  := onmini                             ]  
  set iLarge [ text := "&Large Icons\tCtrl+L"     , checkable := True, checked := False                        , on command ::= (>>= setImages) . flip get checked ]  
  set iHelp  [ text := "&Contents"                , help := "Show the contents of GeBoP"                       , on command  := html f "help\\index.html"          ]
  set iBrain [ text := "The &Brain"               , help := "Shows help about the brain viewer"                , on command  := html f "help\\brain.html"          ]
  set iAbout [                                      help := "Information about this program"                   , on command  := infoDialog f "About GeBoP" about   ]

  set tc [ on treeEvent := onTreeEvent
         ] 

  set pLeft [ layout := fill $ widget tc
            ]

  set pValue [ clientSize := Size 40 40
             , on paint   := onpaintValue
             , on resize ::= repaint
             ]

  set pInfo [ layout := fill $ column 4 $
              [ widget tMind    
              , widget tMaxd    
              , widget tVolume    
              ]
            ]

  set pBoard [ on resize ::= repaint
             ] 

  set pRight [ layout := margin 4 $ column 4 $
               [ row 4 [widget bPlayer, fill $ widget pValue]
               , hfill $ widget pInfo
               ,  fill $ widget pBoard
               ]
             ]

  set f [ text              := gamename ++ " Brain"
        , menuBar           := [mGame, mBrain, mHelp] 
        , layout            := fill $ vsplit sw 4 200 (widget pLeft) (widget pRight)
        , picture           := "brain.ico"
        , clientSize        := Size 480 360
        , on closing        :~ (varSet varb emptyBrain >>)
        ]
        
colorplayer :: Int -> Color
colorplayer 0 = hsl 0.66 1   0.5
colorplayer 1 = hsl 0    1   0.5
colorplayer 2 = hsl 0.33 1   0.5
colorplayer 3 = hsl 0.82 1   0.5
colorplayer 4 = hsl 0.11 0.7 0.5
colorplayer 5 = hsl 0    0   0.5
colorplayer _ = hsl 0    0   0

nameplayer :: Int -> String
nameplayer 0 = "blue"
nameplayer 1 = "red"
nameplayer 2 = "green"
nameplayer 3 = "purple"
nameplayer 4 = "brown"
nameplayer 5 = "grey"
nameplayer _ = "black"

info :: String -> IO ()
info s = do
  putStrLn $ "[" ++ s ++ "]"
  return ()
  
html :: Window a -> FilePath -> IO ()
html w f = do
  d <- dialog w [text := "Help"]
  -- w <- htmlWindowCreate d (-1) (rect (point 0 0) (size 640 480)) 0 []
  w' <- htmlWindowCreate d (-1) (rect (point 0 0) (Size 640 480)) 0 []
  _  <- htmlWindowLoadPage w' f
  set d [layout := widget w', visible := True]
  return ()

about :: String
about = "GeBoP - General Boardgames Player"
     ++ "\nversion " ++ version
     ++ "\n"
     ++ "\nby Maarten Löffler"
     ++ "\nmloffler@cs.uu.nl"
     ++ "\n"
     ++ "\nGeBoP was written using wxHaskell"
     
bitmap :: String -> IO (Bitmap ())
bitmap name' = do bmp    <- bitmapCreateLoad ("images\\" ++ name' ++      ".bmp") wxBITMAP_TYPE_ANY
                  mskbmp <- bitmapCreateLoad ("images\\" ++ name' ++ "_mask.bmp") wxBITMAP_TYPE_ANY 
                  bitmapSetDepth mskbmp 1
                  msk    <- maskCreate mskbmp
                  bitmapSetMask bmp msk
                  return bmp

bitmapImageList :: ImageList () -> String -> IO ()
bitmapImageList il name' = do
  bmp    <- bitmapCreateLoad ("images\\" ++ name' ++      ".bmp") wxBITMAP_TYPE_ANY
  mskbmp <- bitmapCreateLoad ("images\\" ++ name' ++ "_mask.bmp") wxBITMAP_TYPE_ANY 
  bitmapSetDepth mskbmp 1
  _ <- imageListAddBitmap il bmp mskbmp
  return ()

bitmapHighImageList :: ImageList () -> String -> IO ()
bitmapHighImageList il name' = do
  bmp    <- bitmapCreateLoad ("images\\high_" ++ name' ++      ".bmp") wxBITMAP_TYPE_ANY
  mskbmp <- bitmapCreateLoad ("images\\"      ++ name' ++ "_mask.bmp") wxBITMAP_TYPE_ANY 
  bitmapSetDepth mskbmp 1
  _ <- imageListAddBitmap il bmp mskbmp
  return ()
