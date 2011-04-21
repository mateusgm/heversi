@echo off
path c:\ghc\ghc-6.4\bin;%PATH%

ghc --make -package wx Reversi.hs -o Reversi.exe

del *.hi
del *.o

pause