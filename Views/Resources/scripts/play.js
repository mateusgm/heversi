
   function setUpdates(time) {
      update()
      setInterval(update, time);
   }

   function play(x, y) {
      setPosition(x,y);
      setResponse();  
   }
   
   function update() {
      console.log('update!');
      var url = '/game/get';
      $.get(url, updateGame);
   }
   
   function setPosition(x,y) {
      $('#position-x').val(function(i,v){
         return x;
      });
      $('#position-y').val(function(i,v){
         return y;
      });
   }
   
   function setResponse() {
      var url = '/game/get';
      var move = $('#move').serializeArray();
      $.post(url, move, updateGame);
   }
   
   function updateGame(data) {
      var updates = $.parseJSON(data);
      
      if (updates.state) {
         // { turn: 'x', black: 50, white: 60 }
         $('#turn').val(function(){
            if (updates.state.turn == 'o') {
               return 'Vez do preto';
            } else if (updates.state.turn == 'x') {
               return 'Vez do branco';
            }
         });
         $('#black').val(function(){ return updates.state.black; });
         $('#white').val(function(){ return updates.state.black; });
         setUserTurn(updates.state.turn);
      }
      if (updates.board) {
         // { 11: "o", ... }
         $.each(updates.board, updatePosition);
      }      
      if (updates.available && isUserTurn()) {
         // [{x: 1, y: 2}, ...]
         $('.position').each(makeNotAvailable);
         $.each(updates.available, makeAvailable);
      }     
   }
   
   var isTurn = false;
   function setUserTurn(turn) {
      var user = $('#stone').val();
      if (user == turn) isTurn = true;
      else isTurn = false;
   }
   function isUserTurn(turn) {
      return isTurn;
   }
   
   
   function makeNotAvailable (index, element) {
      $(element).val(function() {
         return '';
      });
   }
   
   function makeAvailable (index, value) {
      $('#position-' + value.x + value.y).val(function() {
         var onclick = 'play(' + value.x + ',' + value.y + ');';
         return '<a href="javascript:void(0);" onclick="' + onclick + '"></a>';
      });
   }
   
   function updatePosition(index, value) {
      $('#position-' + index)
         .removeClass('stone-o stone-x stone--')
         .addClass('stone_' + value);
   }
