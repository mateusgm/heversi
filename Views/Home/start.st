$ header() $

  <p>Olá, $request.name$!</p>
  
  <p>Possíveis adversários no momento:</p>
  <form method="POST" action="/game/begin">
    <p>
      <input type="radio" name="oponent" value="0"/>Android</br>
        $users:list()$  
    </p>
    <input type="submit" value="Jogar"/>
  </form>

$ footer() $
