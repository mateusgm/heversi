$ header() $

   <div id="info">
      <span id="opponent">$game.opponent$</span>
   </div>
   
   <form id="move" action="/game/play" method="POST">
      <input id="game" type="hidden" name="game" value="$game.id$" />
      <input id="player" type="hidden" name="player" value="$game.stone$" />
      <input id="position-x" type="hidden" name="x" value="" />
      <input id="position-y" type="hidden" name="y" value="" />
   </form>
  
   <script>
      \$(document).ready(setUpdates(5000));
   </script>
  
   <div id="scores">
      <div id="turn">Vez do preto</div>
      <div id="black" class="count">2</div> 
      <div id="white" class="count">2</div>
   </div>
    
   <div id="board">
      $board:board_list()$
   </div>   

$ footer() $
