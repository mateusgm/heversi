{
   "state": {
      "turn": "$state.turn$",
      "idle": "$state.idle$", 
      "status": "$state.status$",     
      "black": $state.black$,
      "white": $state.white$
      },
   "game": {
      "id": $game.id$,
      "turn": $game.turn$,
      "idle": $game.idle$     
      },
   "board": {
      $board:board_json()$
      "nenhum": ""
      },
   "available": [
      $available:available_json()$
      "0"
   ]
}
