-----------------------
-- GeBoP version 1.7 --
-----------------------

This is the readme.txt file that comes with the source code of GeBoP, version 1.7. This source consists of the following files:

* 15 Haskell source code files
*  2 Icon files
*    This readme file
* 52 Bitmap images in a directory called 'images'
* 20 HTML files and a bitmap in a directory called 'help'

------------------------------------
-- meaning of the various modules --
------------------------------------

* Game
GeBoP works with a class Game, which is defined in the module Game. This class describes the general properties a boardgame should have. The Game module also includes the concept of a game tree, and a general algorithm to traverse this tree in order to find sensible moves.

* Ataxx, Bamp, Halma, Hez, Kram, Nim, Reversi, TicTacToe, and Zenix
These are the implemented games. Each of these modules contains an instance of the Game class.

* GUI
GUI is the module that contains the GUI itself. 

* Tools
This module is just an unstructured bunch of functions I use in other modules :).

* HSL
This module implements the HSL color model.

* Inf
This module defines the set of integers including two extra values <+infinity> and <-infinity>

* Main
Main just imports the games and starts the GUI.

--------------------------
-- state of the program --
--------------------------

Version 1.7 of GeBoP is a nice and complete version. However, I could still do some work on the algorithms for playing the computer uses, and some of the games use rather naïve evaluation functions at the moment. I plan to do this some time in the near or otherwise far future.

Since you are reading this file, you are probably a Haskell programmer. If you feel like implementing your favorite game for GeBoP, please go ahead and mail it to me when you're done :)

--------
-- me --
--------

My name is Maarten Löffler. I am currently a student at Utrecht University. 

I wrote GeBoP in my free time, because I like games and I like Haskell. When wxHaskell came to be, it became even more fun because of the nice graphical effects :)

website at www.students.cs.uu.nl/people/mloffler
email   at mloffler@cs.uu.nl