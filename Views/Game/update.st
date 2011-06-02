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
      $board:jsonList()$
      "nenhum": ""
      },
   "available": [
      $available:available()$
      {"nenhum": ""}
   ]
}
