
#include "jogo.h"
#include "uololou.h"

struct Game* game;
int tabuleiro[8][8];
int odds[8][8];
int qtdBrancas = 2;
int qtdPretas = 2;

// funções

void jogo_init (void){

	struct Player *player1, *player2;

	player1 = playerConstruct ("Player1");
	player2 = playerConstruct ("Player2");

	game = gameConstruct (player1, player2, 8, 8);

}

int uololouthello (int X, int Y, int num_players, int the_single_player, int *next_turn){

	struct Coord jogada;
	int flips, player, resultado;

	player = gameCurrentPlayer (game);
	if (num_players == 1 && player != the_single_player)	
		jogada = aiPlay (game);
	else
		jogada = coord (X, Y);

	flips = gameNumFlips(game, jogada);
	if (player == BRANCO){
		qtdBrancas += flips+1;
		qtdPretas -= flips;
	}else{
		qtdBrancas -= flips;
		qtdPretas += flips+1;
	}
	resultado = gamePlayCurrent (game, jogada);
	*next_turn = gameCurrentPlayer (game);
	return resultado;

}

void seta_tabuleiro (){

	int i, j;
	for (i = 0; i < 8; i++)
		for (j = 0; j < 8; j++){
			tabuleiro[i][j] = gameSquareType (game, coord (i,j));
			if (tabuleiro[i][j] == 0 && gameNumFlips(game, coord(i,j)))
				tabuleiro[i][j] = POSSIVEL_JOGADA;
		}

	for (i = 0; i < 8; i++)
		for (j = 0; j < 8; j++)
			if (tabuleiro[i][j] == POSSIVEL_JOGADA)
				odds[i][j] = avalia_posicao (i, j);
				
}

int avalia_posicao (int i, int j){

	int alfa, beta, player;

	alfa = LOSE;
	beta = WIN;
	player = gameCurrentPlayer (game);

	return alpha_beta_pruning (coord (i,j), alfa, beta, game, INITIAL_DEPTH, player);

}

