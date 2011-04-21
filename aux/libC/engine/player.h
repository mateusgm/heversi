#ifndef TKK_AS_C_PLAYER_H
#define TKK_AS_C_PLAYER_H

struct Player;

struct Player* playerConstruct(char const* name);
void playerDestruct(struct Player* player);
char const* playerGetName(struct Player const* player);

#endif
