{
   "state": {
      "turn": "$state.turn$",
      "black": $state.black$,
      "white": $state.white$
      },
   "game": {
      "id": $game.id$,
      "stone": "$game.stone$",
      "opponent": $game.opponent$     
      },
   "board": {
      $board:board_json()$
      "nenhum": ""
      },
   "available": [
      $available:available_json()$
      {"nenhum": ""}
   ]
}
