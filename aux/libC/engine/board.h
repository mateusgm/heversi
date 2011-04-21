// A gaming board of unlimited size. Accessed using integer coordinates
// (negative or positive) and each element is an int value. The default value
// for elements not written to is 0.

// While this is meant to be used internally by xogame.c for holding the game
// state, it may also prove to be useful in AI algorithms, etc.

#ifndef TKK_AS_C_BOARD_H
#define TKK_AS_C_BOARD_H

#include "util.h"
#include <stddef.h>

struct Board;

/** Construct a new board. **/
struct Board* boardConstruct(size_t width, size_t height);

/** Destruct a board. **/
void boardDestruct(struct Board* board);

size_t boardWidth(struct Board const* board);
size_t boardHeight(struct Board const* board);

/** Make a copy of the board and its contents. **/
struct Board* boardCopy(struct Board const* board);

/** Read the value from coordinates c. -1 if out of bounds. **/
int boardRead(struct Board const* board, struct Coord c);

/** Write a value to coordinates c. Terminates the program if out of bounds. **/
void boardWrite(struct Board* board, struct Coord c, int value);

/** Get the coordinates of the last write. Initially (0, 0). **/
struct Coord boardLastCoord(struct Board const* board);

/** Return 1 if boards have identical contents. **/
int boardEqual(struct Board const* lhs, struct Board const* rhs);

#endif
