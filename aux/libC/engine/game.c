#include "board.h"
#include "game.h"
#include <stdlib.h>

// This variable is not supposed to be changed - it merely documents the code
// If you change it, fix the constructor.
#define NUM_PLAYERS 2

struct Game {
	int turn;
	struct Board* board;
	struct Player const* players[NUM_PLAYERS];
};

struct Game* gameConstruct(struct Player const* p1, struct Player const* p2, size_t width, size_t height) {
	struct Game* g = malloc(sizeof(struct Game));
	if (!g) return NULL;
	g->board = boardConstruct(width, height);
	g->players[0] = p1;
	g->players[1] = p2;
	g->turn = 1;
	boardWrite(g->board, coord(width / 2 - 1, height / 2), 1);
	boardWrite(g->board, coord(width / 2, height / 2), 2);
	boardWrite(g->board, coord(width / 2, height / 2 - 1), 1);
	boardWrite(g->board, coord(width / 2 - 1, height / 2 - 1), 2);
	return g;
}

void gameDestruct(struct Game* game) {
	boardDestruct(game->board);
	free(game);
}

struct Game* gameCopy(struct Game const* game) {
	struct Game* g = malloc(sizeof(struct Game));
	if (!g) return NULL;
	if (!(g->board = boardCopy(game->board))) { free(g); return NULL; }
	for (size_t i = 0; i < NUM_PLAYERS; ++i) g->players[i] = game->players[i];
	g->turn = game->turn;
	return g;
}

size_t gameWidth(struct Game const* game) { return boardWidth(game->board); }
size_t gameHeight(struct Game const* game) { return boardHeight(game->board); }

bool gameSquareMy(struct Game const* game, struct Coord coord) { return gameSquareType(game, coord) == game->turn; }
bool gameSquareEmpty(struct Game const* game, struct Coord coord) { return gameSquareType(game, coord) == 0; }
bool gameSquareHilighted(struct Game const* game, struct Coord coord) { return boardRead(game->board, coord) >= 0x100; }
int gameSquareType(struct Game const* game, struct Coord coord) { return boardRead(game->board, coord) % 0x100; }
int gameCurrentPlayer(struct Game const* game) { return game->turn; }

/** Private function for getting the currently playing player. **/
static struct Player const* currentPlayer(struct Game const* game) {
	size_t t = game->turn - 1;
	return t < NUM_PLAYERS ? game->players[t] : NULL;
}

struct Coord gameLastCoord(struct Game const* game) {
	return boardLastCoord(game->board);
}

static const int DX[] = { 1, 1, 0, -1, -1, -1, 0, 1 };
static const int DY[] = { 0, 1, 1, 1, 0, -1, -1, -1 };

unsigned int gameNumFlips(struct Game const* game, struct Coord coord) {
	if (!gameSquareEmpty(game, coord)) return 0;
	unsigned int num = 0;
	for (int dir = 0; dir < 8; ++dir) {
		int dx = DX[dir];
		int dy = DY[dir];
		struct Coord pos = coord;
		pos.x += dx;
		pos.y += dy;
		int type = gameSquareType(game, pos);
		if (type <= 0 || type == game->turn) continue;
		unsigned int count = 0;
		do {
			++count;
			pos.x += dx;
			pos.y += dy;
		} while (gameSquareType(game, pos) == type);
		if (gameSquareMy(game, pos)) num += count;
	}
	return num;
}

static void gameNextTurn(struct Game* game) {
	game->turn = game->turn % NUM_PLAYERS + 1;
}

int gamePlay(struct Game* game, struct Player const* player, struct Coord c) {
	if (player != currentPlayer(game)) return -1;
	return gamePlayCurrent(game, c);
}

int gamePlayCurrent(struct Game* game, struct Coord c) {
	size_t width = boardWidth(game->board);
	size_t height = boardHeight(game->board);
	if (gameNumFlips(game, c) == 0) return -2;
	int turn = game->turn;
	for (int dir = 0; dir < 8; ++dir) {
		int dx = DX[dir];
		int dy = DY[dir];
		struct Coord pos = c;
		pos.x += dx;
		pos.y += dy;
		int type = gameSquareType(game, pos);
		if (type <= 0 || type == turn) continue;
		do {
			pos.x += dx;
			pos.y += dy;
		} while (gameSquareType(game, pos) == type);
		if (gameSquareMy(game, pos)) {
			while (pos.x != c.x || pos.y != c.y) {
				pos.x -= dx;
				pos.y -= dy;
				boardWrite(game->board, pos, turn);
			}
		}
	}
	do {
		gameNextTurn(game);
		// Check if the opponent can play
		for (size_t y = 0; y < height; ++y) {
			for (size_t x = 0; x < width; ++x) {
				if (gameNumFlips(game, coord(x, y)) > 0) return 0;
			}
		}
	} while (game->turn != turn);
	// No-one can play, the game ends!
	unsigned int count[NUM_PLAYERS] = { 0 };
	for (size_t y = 0; y < height; ++y) {
		for (size_t x = 0; x < width; ++x) {
			int type = gameSquareType(game, coord(x, y));
			if (type) ++count[type - 1];
		}
	}
	// Count the markers to find out the winner
	int winner = 0;
	unsigned int max = 0;
	for (size_t player = 1; player <= NUM_PLAYERS; ++player) {
		if (count[player - 1] >= max) {
			winner = player;
			max = count[player - 1];
		}
	}
	game->turn = 0;
	return winner;
}
