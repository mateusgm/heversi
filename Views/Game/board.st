$ header() $

   $if(game.turn)$
      <p>Sua vez!</p>
   $else$
      <p>Vez do oponente!</p>
   $endif$
    
  <form id="move" action="/game/play" method="POST">
     <input type="hidden" name="player" value="$user.id$" />
     <input id="position-x" type="hidden" name="position-x" value="" />
     <input id="position-y" type="hidden" name="position-y" value="" />
  </form>
  
  <div id="board">
     $board:list()$
  </div>   

  
  <p>State: $state$</p> 

  </hr>
  
  <p>Game: $game$</p> 
    
$ footer() $
