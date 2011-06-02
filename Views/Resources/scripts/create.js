       
   function setFormHandler() {
      console.log('oi');
      $('#create').submit(function(){
         var url = '/game/create';
         var data = $(this).serializeArray();
         $.post(url,data,success);
         return false;
      }); 
   }
   
   function success() {
      var url = '/game/play';
      window.location.href = url;      
   }
