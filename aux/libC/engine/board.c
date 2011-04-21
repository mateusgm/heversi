#include "board.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Private structure that may only be accessed via the functions in this file.
struct Board {
	struct Coord last;
	size_t width;
	size_t height;
	int* array;
};

// Private functions, to be used internally by this module (file) only.
// They are not declared in headers and the keyword static would prevent using
// them from other modules even if they were.

/** Exit on fatal error. **/
static void boardError(char const* msg) {
	puts(msg);
	exit(EXIT_FAILURE);
}

/** Return a pointer to the given coordinates. **/
static int* boardIndex(struct Board* board, struct Coord c) {
	if (c.x < 0 || c.y < 0 || c.x >= (int)board->width || c.y >= (int)board->height) return NULL;
	return board->array + c.y * board->width + c.x;
}

int boardEqual(struct Board const* lhs, struct Board const* rhs) {
	if (lhs->width != rhs->width || lhs->height != rhs->height) return 0;
	for (size_t y = 0; y < lhs->height; ++y) {
		for (size_t x = 0; x <= lhs->width; ++x) {
			if (boardRead(lhs, coord(x, y)) != boardRead(rhs, coord(x, y))) return 0;
		}
	}
	return 1;
}

// Public functions (declared and documented in header xoboard.h)

struct Board* boardConstruct(size_t width, size_t height) {
	struct Board* b = malloc(sizeof(struct Board));
	if (!b || !(b->array = calloc(width * height, sizeof(*b->array)))) boardError("Unable to allocate new Board");
	b->width = width;
	b->height = height;
	b->last = coord(0, 0);
	return b;
}

void boardDestruct(struct Board* board) {
	free(board->array);
	free(board);
}

size_t boardWidth(struct Board const* board) { return board->width; }

size_t boardHeight(struct Board const* board) { return board->height; }

struct Board* boardCopy(struct Board const* board) {
	struct Board* b = boardConstruct(board->width, board->height);
	b->width = board->width;
	b->height = board->height;
	memcpy(b->array, board->array, b->width * b->height * sizeof(*b->array));
	b->last = board->last;
	return b;
}

struct Coord boardLastCoord(struct Board const* board) { return board->last; }

int boardRead(struct Board const* board, struct Coord c) {
	// Casting away constness is evil, but here we know that we don't really
	// write to the board, and this evilness is limited inside board.c, so
	// it is acceptable.
	int const* p = boardIndex((struct Board*)board, c);
	return p ? *p : -1;
}

void boardWrite(struct Board* board, struct Coord c, int value) {
	int* p = boardIndex(board, c);
	if (!p) boardError("boardWrite out of bounds");
	*p = value;
	board->last = c;
}
