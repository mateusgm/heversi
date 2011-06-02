
   var updateURL = '/game/update';
   var isTurn = false;

   function setUpdates(time) {
      update();
      //setInterval(update, time);
   }

   function play(x,y) {
      setPosition(x,y);
      setResponse();  
   }
   
   function update() {
      console.log('update!');
      $.post(updateURL, updateGame);
   }
   
   function setPosition(x,y) {
      $('#position-x').val(x);
      $('#position-y').val(y);
   }
   
   function setResponse() {
      var move = $('#move').serializeArray();
      $.post(updateURL, move, updateGame);
   }
   
   function updateGame(data) {
      console.log(data);
      var updates = $.parseJSON(data);
      
      if (updates.state) {               
         if (updates.state.turn == 'o')
            $('#turn').html('Vez do preto');
         else if (updates.state.turn == 'x')
            $('#turn').html('Vez do branco');
         $('#black').html(updates.state.black);
         $('#white').html(updates.state.black);
         setUserTurn(updates.state.turn);
      }
      
      if (updates.board) {
         $.each(updates.board, updatePosition);
      }

      if (updates.available && isTurn) {
         $('.position').each(makeNotAvailable);
         $.each(updates.available, makeAvailable);
      }     
   }
   
   function updatePosition(index, value) {
      $('#position-' + index)
         .removeClass('stone-o stone-x stone--')
         .addClass('stone-' + value);
   }

   function setUserTurn(turn) {
      var user = $('#stone').text();
      if (user == turn) isTurn = true;
      else isTurn = false;
   }
 
   
   function makeNotAvailable (index, element) {
      $(element).text(function() {
         return '';
      });
   }
   
   function makeAvailable (index, value) {
      var onclick = 'play(' + value.x + ',' + value.y + ');';
      var link = '<a href="javascript:void(0);" onclick="' + onclick + '"></a>';
      $('#position-' + value.x + value.y).html(link);
   }
   

