       
   function setFormHandler(target) {
      return function (){
         $('#create').submit(function(){
            var handler = $(this).attr('action');
            var data = $(this).serializeArray();
            $.post(handler,data,success(target));
            return false;
         });
      } 
   }
   
   function success(target) {
      return function () {
         window.location.href = target;
      }      
   }
