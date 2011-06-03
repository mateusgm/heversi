$ header() $
  
   <div id="info">
      <span id="game">$game.id$</span>
      <span id="id">$user.id$</span>
   </div>
   
   
   <form id="move" action="/game/play" method="POST">
      <input id="user" type="hidden" name="user" value="" />
      <input id="player" type="hidden" name="player" value="" />
      <input id="position-x" type="hidden" name="x" value="" />
      <input id="position-y" type="hidden" name="y" value="" />
   </form>
  
   <script>
      \$(document).ready(setUpdates(3000));
   </script>
  
   <div id="scores">
      <div id="turn">Vez do preto</span></div>
      <div id="black" class="count">2</div> 
      <div id="white" class="count">2</div>
   </div>
    
   <div id="board">
      $board:board_list()$
   </div>   

$ footer() $
