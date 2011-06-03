
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

   function play(x,y) {
      setPosition(x,y);
      setPlayer(userID);
      makeMove();
   }
  
   function aiUpdate() {
      setPosition(0,0);
      setPlayer(0);
      makeMove();
   }
   
   function setPosition(x,y) {
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
         if (updates.state.turn == 'o')
            $('#turn').html('Vez do preto');
         else if (updates.state.turn == 'x')
            $('#turn').html('Vez do branco');
         $('#black').html(updates.state.black);
         $('#white').html(updates.state.white);
      }
      
      if (updates.board) {
         $.each(updates.board, updatePosition);
      }

      $('.position').each(makeNotAvailable);
      if (updates.available) {
         $.each(updates.available, makeAvailable(stones[userID] == updates.state.turn));
      }
      
      if (updates.game.turn == 0) {
         setTimeout(aiUpdate, 2000);           
      }
      
   }
   
   function updatePosition(index, value) {
      $('#position-' + index)
         .removeClass('stone-o stone-x stone--')
         .addClass('stone-' + value);
   }
 
   function makeNotAvailable (index, element) {
      $(element).html('');
   }
   
   function makeAvailable(turn) { 
      return function (index, value) {
         var onclick = 'play(' + value.x + ',' + value.y + ');';
         var link = turn ? '<a href="javascript:void(0);" onclick="' + onclick + '"></a>' : '';
         var div = '<div class="available">' + link + '</div>'; 
         $('#position-' + value.x + value.y).html(div);
      }
   }

