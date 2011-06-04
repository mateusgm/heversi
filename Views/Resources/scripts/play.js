
   var getURL = '/game/get';
   var updateURL = '/game/update';
   var userID = 0; 
   var stones = new Array();

   function setUpdates(time) {
      init();
      update();
      setInterval(update, time);
   }
   
   function init() {
      userID = parseInt($('#id').html());
   }
   
   function update() {
      $.get(getURL, updateGame);
   }

   function play(y,x) {
      setPosition(y,x);
      setPlayer(userID);
      makeMove();
   }
  
   function aiUpdate() {
      setPosition(0,0);
      setPlayer(0);
      makeMove();
   }
   
   function setPosition(y,x) {
      $('#position-x').val(x);
      $('#position-y').val(y);
   }
   
   function setPlayer(id) {
      $('#user').val(id);
      $('#player').val(stones[id]);
   }
   
   function makeMove() {
      var move = $('#move').serializeArray();
      $.post(updateURL, move, updateGame);
   }
   
   function updateGame(data) {
      console.log(data);
      var updates = $.parseJSON(data);
      
      if (stones.length == 0) {
         stones[updates.game.turn] = updates.state.turn;
         stones[updates.game.idle] = updates.state.idle;
      }
      
      if (updates.state) {
         updateCount(updates.state.black, updates.state.white);
         updateStatus(updates.state.status, updates.state.turn);
      }
      
      var positions = $('.position');
      if (updates.board) {
         positions.each(
            updatePosition(updates.board)
         );
      }
    
      if (updates.available) {
         var isTurn = stones[userID] == updates.state.turn;
         positions.each(
            updateAvailability(updates.available, isTurn)
         );
      }
      
      if (updates.game.turn == 0) {
         setTimeout(aiUpdate, 2000);           
      }
      
   }
   
   function updateStatus (status, turn) {
      switch (status) {              
         case 'play':
            text = (turn == 'o') ? 'Vez do preto' : 'Vez do branco';
            break;
         case 'over':
            text = (turn == 'o') ? 'Preto venceu!' : 'Branco venceu!';
            break;
         case 'draw':
            text = 'Empate!';
      }
      $('#turn').html(text);
   }
   
   function updateCount(black, white) {
      $('#black').html(black);
      $('#white').html(white);   
   }
   
   function updatePosition (board) {
      return function(index, element) {
         var id = $(element).attr('id');
         $(element).removeClass('stone-o stone-x stone--')
            .addClass('stone-' + board[id]);
      }
   }
   
   function updateAvailability(availables, isTurn) {
      return function (i, e) {
         var element = $(e);
         var id = element.attr('id');
         var index = $.inArray(id, availables);
         if (index == -1) {
            if (element.html()) element.html('');
         } else if (!element.html()) {
            var y = availables[index].charAt(0);
            var x = availables[index].charAt(1);
            var onclick = 'play(' + y + ',' + x + ');';
            var link = isTurn ? '<a href="javascript:void(0);" onclick="' + onclick + '"></a>' : '';
            var text = '<div class="available">' + link + '</div>';
            element.html(text);
         }
      }
   }

