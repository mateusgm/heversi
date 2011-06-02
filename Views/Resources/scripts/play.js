
   function play (x, y) {
      
      $('#position-x').val(function(i,v){
         return x;
      });
      $('#position-y').val(function(i,v){
         return y;
      });
      $('#move').submit();
   
   }
