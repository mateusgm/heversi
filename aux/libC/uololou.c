
#include "uololou.h"

struct Coord* obter_movimentos_validos (const struct Game *game)
{
	/* obtem as possíveis jogadas para o jogador da vez de um determinado estado de jogo */

	//declarações

	int width, height, count;
	struct Coord *moves, temp;
	
	width = (int) gameWidth(game);
	height = (int) gameHeight(game);
	moves = NULL;
	count = 0;

	// itera sobre toda o board verificando as possíveis movimentos:

	int i, j;
	for(i = 0; i < width; i++)
		for(j = 0; j < height; j++)
		{
			temp.x = i; temp.y = j;
			if(gameNumFlips(game, temp))
			{
				moves = realloc (moves, (count + 1) * sizeof(struct Coord));
				moves[count] = coord (i,j);
				count++;
			}
		}

	// fecha a lista com um movimento artificial que indica o término da lista de movimentos

	if (moves != NULL)
	{
		moves = realloc(moves, (count + 1) * sizeof(struct Coord));
		moves[count] = coord (FIM,FIM);
	}

	return moves;

}

int obter_estagio (int turns)
{
	/* obtem o estagio do jogo, dado o número de rodadas passadas */

	if (turns <= OPENING_TURNS)
		return OPENING;
	else if (turns <= MIDDLE_TURNS)
		return MIDDLE;
	else
		return ENDING;

}

unsigned int gameNumFlips2 (struct Game const* game, struct Coord coord, int player)
{
	/* verificas se a jogada especificada é possível para o jogador fornecido */

	if (!gameSquareEmpty(game, coord)) return 0;

	static const int DX[] = { 1, 1, 0, -1, -1, -1, 0, 1 };
	static const int DY[] = { 0, 1, 1, 1, 0, -1, -1, -1 };
	unsigned int num = 0;

	for (int dir = 0; dir < 8; ++dir)
	{
		int dx = DX[dir];
		int dy = DY[dir];
		struct Coord pos = coord;
		pos.x += dx;
		pos.y += dy;

		int type = gameSquareType(game, pos);
		if (type <= 0 || type == player) continue;
		unsigned int count = 0;

		do
		{
			++count;
			pos.x += dx;
			pos.y += dy;
		} while (gameSquareType(game, pos) == type);

		if (gameSquareType(game, pos) == player) return TRUE;
	}

	return FALSE;
}

struct Mobilidade obter_mobilidade (struct Game* game, int self)
{
	/* obtem o estado da mobilidade no jogo para ambos os jogadores */

	// declarações
	
	struct Mobilidade final;
	struct Coord coord_aux;
	int i, j, width, height, enemy;

	width = (int) gameWidth(game);
	height = (int) gameHeight(game);
	final.self = final.enemy = final.turns = 0;

	// verifica qual o jogador de referência a ser analisado

	if (gameCurrentPlayer(game) == self)
	{
		// itera sobre o board, verificando os possíveis movimentos para cada jogador

		enemy = (self % 2) + 1;
		for(i = 0; i < width; i++)
			for(j = 0; j < height; j++)
			{
				coord_aux.x = i; coord_aux.y = j;
				if (gameSquareEmpty(game, coord_aux))
				{
					if(gameNumFlips(game, coord_aux)) final.self ++;
					else if (gameNumFlips2 (game, coord_aux, enemy)) final.enemy ++;
				}
				else
				{
					final.turns ++;
				}
			}

	}
	else
	{
		// itera sobre o board, verificando os possíveis movimentos para cada jogador

		enemy = self;
		for(i = 0; i < width; i++)
			for(j = 0; j < height; j++){
				coord_aux.x = i; coord_aux.y = j;
				if (gameSquareEmpty(game, coord_aux))
				{
					if(gameNumFlips(game, coord_aux)) final.enemy ++;
					else if (gameNumFlips2 (game, coord_aux, enemy)) final.self ++;
				}
				else
				{
					final.turns ++;
				}
			}

	}

	return final;

}

int avalia_weak_edge (struct Coord jogada, const struct Game *game, int player, int edgeX)
{
	/*
	* avalia a força da jogada na parede:
	* - se a jogada for tornar o edge weak, retorna 1
	* - do contrário, retorna 0
	*/

	// inicializações

	struct Coord aux;
	int width = (int) gameWidth (game),
	    height = (int) gameHeight (game),
	    x = jogada.x,
	    y = jogada.y,
	    weak_count = 0,
            self_squares = 1,
	    square = 0;

	// verifica em qual parede está a jogada

	if (edgeX)
	{
		// itera sobre as posições da parede contando o número de casas entre duas peças do jogador
		// se o numero de casas (vazias ou ocupadas) entre duas peças do jogador for ímpar, a parede ficou fraca

		x--;
		while (x >= 0 && square != player)
		{
			aux = coord (x,y);
			square = gameSquareType (game, aux);
			if (square == 0 || square != player)
				weak_count++;
			else
				self_squares++;
			x--;
		}

		if (self_squares == 2 && weak_count % 2 == 1)
			return 1;

		weak_count = 0;
		self_squares = 1;
		x = jogada.x+1;

		while (x <= width-1 && square != player)
		{
			aux = coord (x,y);
			square = gameSquareType (game, aux);
			if (square == 0 || square != player)
				weak_count++;
			else
				self_squares++;
			x++;
		}

		if (self_squares == 2 && weak_count % 2 == 1)
			return 1;

	}
	else
	{
		// itera sobre as posições da parede contando o número de casas entre duas peças do jogador
		// se o numero de casas (vazias ou ocupadas) entre duas peças do jogador for ímpar, a parede ficou fraca
		y--;
		while (y >= 0 && square != player)
		{
			aux = coord (x,y);
			square = gameSquareType (game, aux);
			if (square == 0 || square != player)
				weak_count++;
			else
				self_squares++;
			y--;
		}

		if (self_squares == 2 && weak_count % 2 == 1)
			return 1;

		weak_count = 0;
		self_squares = 1;
		y = jogada.y + 1;

		while (y <= height-1 && square != player)
		{
			aux = coord (x,y);
			square = gameSquareType (game, aux);
			if (square == 0 || square != player)
				weak_count++;
			else
				self_squares++;
			y++;
		}

	}

	return 0;
}

int avalia_wedge (struct Coord jogada, const struct Game *game, int player, int edgeX)
{
	/*
	* avalia se a jogada faz um wedge no oponente:
	* - se a jogada fizer, retorna 1
	* - do contrário, retorna 0
	*/

	// inicializações

	struct Coord lado1, lado2;
	int width = (int) gameWidth (game),
	    height = (int) gameHeight (game),
	    tipo1 = -1,
	    tipo2 = -2;

	// verifica em qual parede está a jogada e pega o status das peças laterais à jogada

	if (edgeX)
	{
		if (jogada.x > 0 && jogada.x < width - 1)
		{
			lado1 = coord (jogada.x - 1, jogada.y);
			lado2 = coord (jogada.x + 1, jogada.y);
			tipo1 = gameSquareType (game, lado1);
			tipo2 = gameSquareType (game, lado2);
		}
	}
	else
	{
		if (jogada.y > 0 && jogada.y < width - 1)
		{
			lado1 = coord (jogada.x, jogada.y - 1);
			lado2 = coord (jogada.x, jogada.y + 1);
			tipo1 = gameSquareType (game, lado2);
			tipo2 = gameSquareType (game, lado2);
		}
	}

	// se ambas as peças forem do oponente, a jogada realizou um wedge

	return tipo1 == tipo2 && tipo1 != player;

}

int avalia_edge (struct Coord jogada, const struct Game *game) {

	/*
	* avalia jogadas na parede:
	* - se a jogada fizer um wedge, retorna 2
	* - se ela tornar o edge weak, retorna -1
	* - se for edge e nenhuma das alternativas anteriores, retorna 1
	* - do contrário, retorna 0
	*/

	// inicializações

	int width = (int) gameWidth (game),
	    height = (int) gameHeight (game),
	    player = (gameCurrentPlayer (game) % 2) + 1;

	int edgeX = jogada.y == 0 || jogada.y == height - 1,
	    edgeY = jogada.x == 0 || jogada.x == width - 1;

	// verificações

	if (edgeX || edgeY)
	{
		if (avalia_wedge (jogada, game, player, edgeX))
			return 2;
		else if (avalia_weak_edge (jogada, game, player, edgeX))
			return -1;
		else
			return 1;
	}
	else
	{
		return 0;
	}

}

int avalia_corner (struct Coord jogada, const struct Game *game)
{
	/*
	* avalia jogadas nas quinas:
	* - se for quinta, retorna 1
	* - do contrário, retorna 0
	*/

	int width = (int) gameWidth(game),
	    height = (int) gameHeight(game);

	return (jogada.x == 0 || jogada.x == width - 1) && (jogada.y == 0 || jogada.y == height - 1);

}

struct Coord obtem_corner_proximo (struct Coord jogada, const struct Game *game)
{
	/* retorna a quina mais próxima à jogada, que previamente sabe-se que é um Xsquare ou um Xsquare */
	
	// inicializações

	int width = (int) gameWidth(game),
	    height = (int) gameHeight(game),
	    x, y;

	// declarações

	if (jogada.x == 1)
		x = 0;
	else if (jogada.x == width-2)
		x = width-1;
	else
		x = jogada.x;		

	if (jogada.y == 1)
		y = 0;
	else if (jogada.y == width-2)
		y = width-1;
	else
		y = jogada.y;

	return coord (x,y);	

}

int avalia_Csquare (struct Coord jogada, const struct Game *game)
{
	/*
	* avalia jogadas C_square:
	* - se for Csquare e o corner mais próximo já for meu, retorna -1
	* - se for Csquare e o corner mais próximo não for meu, retorna 1
	* - se for Csquare e o corner mais próximo é vazio, retorna 2
	* - do contrário, retorna 0
	*/

	// inicializações

	int width = (int) gameWidth(game),
	    height = (int) gameHeight(game),
	    player = (gameCurrentPlayer (game) % 2) + 1;

	int CsquareY = (jogada.x == 0 || jogada.x == width - 1) && (jogada.y == 1 || jogada.y == height - 2),
	    CsquareX = (jogada.y == 0 || jogada.y == height - 1) && (jogada.x == 1 || jogada.x == width - 2);

	// verificações

	if (CsquareX || CsquareY)
	{
		struct Coord corner = obtem_corner_proximo (jogada, game);
		int square = gameSquareType (game, corner);
		if (square == player)
			return -1;
		else if (square > 0)
			return 1;
		else
			return 2;
	}
	else
	{
		return 0;
	}

}

int avalia_Xsquare (struct Coord jogada, const struct Game *game)
{

	/*
	* avalia jogadas Xsquare:
	* - se for Xsquare e o corner mais próximo já for meu, retorna -1
	* - se for Xsquare e o corner mais próximo não for meu, retorna 1
	* - se for Xsquare e o corner mais próximo é vazio, retorna 2
	* - do contrário, retorna 0
	*/
 
	// inicializações

	int width = (int) gameWidth(game),
	    height = (int) gameHeight(game),
	    player = (gameCurrentPlayer (game) % 2) + 1;

	int Xsquare = (jogada.x == 1 || jogada.x == width - 2) && (jogada.y == 1 || jogada.y == height - 2);

	// verificações

	if (Xsquare)
	{
		struct Coord corner = obtem_corner_proximo (jogada, game);
		int square = gameSquareType (game, corner);
		if (square == player)
			return -1;
		else if (square > 0)
			return 1;
		else
			return 2;
	}
	else
	{
		return 0;
	}

}

int avalia_third (struct Coord jogada, const struct Game *game)
{
	/*
	* avalia jogadas nas terças:
	* - se for terça-quina, retorna 2
	* - se for terça comum, retorna 1
	* - do contrário, retorna 0
	*/

	int width = (int) gameWidth(game),
	    height = (int) gameHeight(game);

	if ((jogada.x == 2 || jogada.x == width - 3) && (jogada.y == 2 || jogada.y == height - 3))
		return 2;
	else if (jogada.x == 2 || jogada.x == width - 3 || jogada.y == 2 || jogada.y == height - 3)
		return 1;
	else
		return 0;

}

int avalia_flips (struct Game const* game, struct Coord coord)
{

	if (!gameSquareEmpty(game, coord)) return 0;

	static const int DX[] = { 1, 1, 0, -1, -1, -1, 0, 1 };
	static const int DY[] = { 0, 1, 1, 1, 0, -1, -1, -1 };
	int total = 0,
	    player = gameCurrentPlayer (game);
	int corner, edge, Xsquare, Csquare, third;

	for (int dir = 0; dir < 8; ++dir)
	{
		int dx = DX[dir];
		int dy = DY[dir];
		struct Coord pos = coord;
		pos.x += dx;
		pos.y += dy;

		int type = gameSquareType(game, pos);
		if (type <= 0 || type == player) continue;
		corner = edge = Xsquare = Csquare = third = 0;

		do
		{
			corner += avalia_corner (pos, game);
			edge += avalia_edge (pos, game);
			Xsquare += avalia_Xsquare (pos, game);
			Csquare += avalia_Csquare (pos, game);
			third += avalia_third (pos, game);
			pos.x += dx;
			pos.y += dy;
		} while (gameSquareType(game, pos) == type);

		if (gameSquareType(game, pos) == player) total += CORNER*corner + EDGE*edge + X_SQUARE*Xsquare + C_SQUARE*Csquare + THIRD*third;
	}

	corner = avalia_corner (coord, game);
	edge = avalia_edge (coord, game);
	Xsquare = avalia_Xsquare (coord, game);
	Csquare = avalia_Csquare (coord, game);
	third = avalia_third (coord, game);	

	total += FLIPS*(CORNER*corner + EDGE*edge + X_SQUARE*Xsquare + C_SQUARE*Csquare + THIRD*third);

	// total += CORNER*corner + EDGE*edge + X_SQUARE*Xsquare + C_SQUARE*Csquare + THIRD*third;	
	
	return total;
}

		


int executa_jogada (struct Coord jogada, struct Game* game, int self, int *fim_de_jogo)
{

	/* executa a jogada, retornando sua avaliação bem como se ela terminou o jogo */

	// inicializações

	int mobility, corner, edge, flips, Xsquare, Csquare, third;
	int estagio, resultado, resultado_aux, player;
	struct Mobilidade depois;

	player = (gameCurrentPlayer (game) == self) ? SELF : ENEMY;
	flips = avalia_flips (game, jogada);

	// execução da jogada

	resultado_aux = gamePlayCurrent (game, jogada);

	// verificação do resultado e avaliação da jogada

	if(resultado_aux > 0)
	{
		*fim_de_jogo = TRUE;		
		if (resultado_aux == self) resultado = WIN;
		else resultado = LOSE;
	}
	else
	{

		depois = obter_mobilidade (game, self);

		mobility = MOBILITY * (depois.self - depois.enemy);
		
		resultado = mobility + flips;

		if (player == ENEMY) resultado *= -1;

	}
	
	return resultado;

}

int alpha_beta_pruning (struct Coord jogada, int alfa, int beta, const struct Game* game, int depth, int self)
{
	/* implementação do algoritmo MiniMax alpha-beta pruning */

	// inicializações

	int player, result, melhor, i, fim_de_jogo, local;
	struct Coord jogada_aux, *movimentos_validos;
	struct Game *game_aux;

	// execução da jogada

	melhor = 0;
	fim_de_jogo = FALSE;
	player = (gameCurrentPlayer (game) == self) ? SELF : ENEMY;
	game_aux = gameCopy (game);
	local = executa_jogada (jogada, game_aux, self, &fim_de_jogo);

	// verifica se já é hora de parar a recursão

	if (fim_de_jogo == FALSE && depth != MAX_DEPTH)
	{
		// obtem as jogadas válidas
		melhor = (player == SELF) ? LOSE : WIN;		
		movimentos_validos = obter_movimentos_validos (game_aux);

		if (movimentos_validos != NULL)
		{
			// se existir alguma jogada válida, itera sobre as mesmas
			i = 0;
			melhor = (player == SELF) ? LOSE : WIN;
			jogada_aux = movimentos_validos[i];

			while (jogada_aux.x != FIM)
			{
				// chama o minimax para a jogada corrente
				result = alpha_beta_pruning (jogada_aux, alfa, beta, game_aux, depth+1, self);

				// avalia se a jogada é melhor do que as já avaliadas, e atualiza alpha e beta
				if (player == SELF)
				{
					if (result > melhor) melhor = result;
					if (melhor > beta) break;
					if (melhor > alfa) alfa = melhor;
				}
				else
				{
					if (result < melhor) melhor = result;
					if (melhor < alfa) break;
					if (melhor < beta) beta = melhor;
				}

				i++; jogada_aux = movimentos_validos[i];
			}
			free (movimentos_validos);
		}

	}

	gameDestruct (game_aux);

	// retorna o valor da melhor jogada + o valor local
	// isso porque a função de avaliação avalia a jogada e não o estado do jogo
	return melhor+local;

}



struct Coord aiPlay(const struct Game* game){

	// inicializações

	int alfa, beta, i, player, result;
	struct Coord *movimentos_validos, jogada_aux, melhor_jogada;

	alfa = LOSE;
	beta = WIN;
	i = 0;
	player = gameCurrentPlayer (game);
	movimentos_validos = obter_movimentos_validos (game);

	// itera sobre as possíves jogadas

	melhor_jogada = jogada_aux = movimentos_validos[i];
	while (jogada_aux.x != FIM && alfa != WIN)
	{
		// chama o minimax para a jogada corrente
		result = alpha_beta_pruning (jogada_aux, alfa, beta, game, INITIAL_DEPTH, player);

		// avalia se a jogada é melhor do que as já avaliadas, e atualiza alpha
		if (result > alfa)
		{
			alfa = result;
			melhor_jogada = jogada_aux;
		}
		i++; jogada_aux = movimentos_validos[i];

	}

	free (movimentos_validos);

	return melhor_jogada;
}

