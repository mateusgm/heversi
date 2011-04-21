#ifndef TKK_AS_C_GAME_H
#define TKK_AS_C_GAME_H

#include "util.h"
#include <stdbool.h>
#include <stddef.h>

struct Player;
struct Game;

/**
* Construct a new game, with players p1 and p2. The players must not be
* freed or reallocated until after the game has been destructed.
**/
struct Game* gameConstruct(struct Player const* p1, struct Player const* p2, size_t width, size_t height);

/** Free the resources allocated for the game. **/
void gameDestruct(struct Game* game);

/** Make a full copy of a game. **/
struct Game* gameCopy(struct Game const* game);

/** Return board width. **/
size_t gameWidth(struct Game const* game);

/** Return board height. **/
size_t gameHeight(struct Game const* game);

/** Return true if the square is empty **/
bool gameSquareEmpty(struct Game const* game, struct Coord coord);

/** Return true if the square contains marker of the current player. **/
bool gameSquareMy(struct Game const* game, struct Coord coord);

/**
* Return the number of enemy markers that can be flipped by playing coord.
* @return 0 if the move is invalid, positive value for valid moves.
**/
unsigned int gameNumFlips(struct Game const* game, struct Coord coord);

/**
* Return true if the square is hilighted (used for marking winning rows once
* the game has finished).
**/
bool gameSquareHilighted(struct Game const* game, struct Coord coord);

/** Return square type: 0 for empty, player number for filled squares. **/
int gameSquareType(struct Game const* game, struct Coord coord);

/** Return the current player number (1 or 2). **/
int gameCurrentPlayer(struct Game const* game);

/** Coordinates of the previous move. Initially (0, 0). **/
struct Coord gameLastCoord(struct Game const* game);

/**
* Place the player's marker at the given coordinates.
* This function is deprecated by gamePlayCurrent, but is still provided for
* compatibility with old code.
* @return zero player number if the player won, negative value on error, zero otherwise.
**/
int gamePlay(struct Game* game, struct Player const* player, struct Coord coord);

/**
* Place current player's marker at the given coordinates.
* @return zero player number if the player won, negative value on error, zero otherwise.
**/
int gamePlayCurrent(struct Game* game, struct Coord coord);

#endif

