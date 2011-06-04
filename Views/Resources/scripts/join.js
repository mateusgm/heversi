
   var checkURL = '/game/check';

   function setJoinCheck(target, time) {
      setInterval(check(target), time);
   }
   
   function check(target) {
      return function() {
         $.get(checkURL, joinGame(target));
      }
   }
 
   function joinGame(target) {
      return function () {
         window.location.href = target;      
      }   
   }

