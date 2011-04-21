#ifndef UOLOLOU_H
#define UOLOLOU_H

#include <stdlib.h>
#include <stdio.h>

#include "engine/game.h"

// estagios

#define NUM_STAGES 3

#define OPENING 0
#define MIDDLE 1
#define ENDING 2

#define OPENING_TURNS 20
#define MIDDLE_TURNS 40
#define ENDING_TURNS 60

// variaveis

#define NUM_VARIABLES 6

#define MOBILITY 5
#define CORNER 50
#define EDGE 11
#define FLIPS 3
#define X_SQUARE -30
#define C_SQUARE -20
#define THIRD 10

// jogadores

#define NUM_PLAYERS 2

#define SELF 0
#define ENEMY 1

// constantes

#define INITIAL_DEPTH 1
#define MAX_DEPTH 1

#define WIN 20000000
#define LOSE -20000000
#define FIM -1

#define TRUE 1
#define FALSE 0

//estruturas

struct Mobilidade{
	int self, enemy, turns;
};

// funções: (cabeçalho)

struct Coord aiPlay(const struct Game* game);
int alpha_beta_pruning (struct Coord jogada, int alfa, int beta, const struct Game* game, int depth, int self);
struct Coord* obter_movimentos_validos (const struct Game *game);
int executa_jogada (struct Coord jogada, struct Game* game, int self, int *fim_de_jogo);
int obter_estagio (int turns);
struct Mobilidade obter_mobilidade (struct Game* game, int self);
int verify_edge (struct Coord jogada, const struct Game *game);
int verify_corner (struct Coord jogada, const struct Game *game);
int verify_Csquare (struct Coord jogada, const struct Game *game);
int verify_Xsquare (struct Coord jogada, const struct Game *game);




#endif
