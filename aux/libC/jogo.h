#ifndef JOGO_H
#define JOGO_H

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "engine/game.h"
#include "engine/player.h"
#include "uololou.h"

#define PRETO		1
#define BRANCO		2
#define POSSIVEL_JOGADA 3

// cabeçalho funções:

void jogo_init (void);
int uololouthello (int X, int Y, int num_players, int the_single_player, int *next_turn);
void seta_tabuleiro ();
int avalia_posicao (int i, int j);

// variaveis

extern struct Game* game;
extern int tabuleiro[8][8];
extern int odds[8][8];
extern int qtdBrancas;
extern int qtdPretas;

#endif
