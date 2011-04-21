#ifndef TKK_AS_C_UTIL_H
#define TKK_AS_C_UTIL_H

/** A data structure for board coordinates **/
struct Coord { int x, y; };

// Note: static inline means that the function will be inlined inside the module
// that it is called from. This means that no function call will occur, leading
// to a slight performance improvement when the function is very simple (like
// in this case). Generally inlining is not a good idea, so try to avoid it.

/** Create struct Coord for the given coordinates. **/
static inline struct Coord coord(int x, int y) {
	struct Coord c;
	c.x = x;
	c.y = y;
	return c;
}

/** A data structure for two pairs of coordinates. **/
struct Rect { struct Coord min, max; };

#endif
