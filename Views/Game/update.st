{
   "state": {
      "turn": "$state.turn$",
      "black": $state.black$,
      "white": $state.white$
      },
   "user": {
      "id": $user.id$,
      "name": "$user.name$"
      },
   "game": {
      "id": $game.id$,
      "stone": "$game.stone$",
      "opponent": $game.opponent$     
      },
   "board": {
      $board:board_list()$
      "nenhum": ""
      },
   "available": [
      $available:available_list()$
      {"nenhum": ""}
   ]
}
