$ header() $

   <div id="info">
      <span id="opponent">$game.opponent$<span/>
      <span id="stone">$game.stone$<span/>
   </div>
   
   <form id="move" action="/game/play" method="POST">
      <input id="game" type="hidden" name="game" value="$game.id" />
      <input id="player" type="hidden" name="player" value="$user.id$" />
      <input id="position-x" type="hidden" name="position-x" value="" />
      <input id="position-y" type="hidden" name="position-y" value="" />
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
      $board:list()$
   </div>   

$ footer() $
