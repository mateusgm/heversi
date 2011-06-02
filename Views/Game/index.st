$ header() $

   <script>
      var target = '/game/play';
      \$(document).ready(setFormHandler(target));
   </script>

  <p>Olá, $user.name$!</p>
  
  <p>Possíveis adversários no momento:</p>
  <form id="create" method="POST" action="/game/create">
    <p>
      <input type="radio" name="opponent" value="0"/>Android</br>
      $logged:user_list()$  
    </p>
    <input type="submit" value="Jogar"/>
  </form>

$ footer() $
