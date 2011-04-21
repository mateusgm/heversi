#include "player.h"
#include <stdlib.h>
#include <string.h>

struct Player {
	char* name;
};

struct Player* playerConstruct(char const* name) {
	struct Player* player = malloc(sizeof(struct Player));
	if (!player) return NULL;
	size_t len = strlen(name) + 1;
	if (!(player->name = malloc(len))) return NULL;
	memcpy(player->name, name, len);
	return player;
}

void playerDestruct(struct Player* player) {
	free(player->name);
	free(player);
}

char const* playerGetName(struct Player const* player) {
	return player->name;
}
