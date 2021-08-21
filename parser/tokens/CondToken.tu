unit class CondToken
  inherit Token in "../../lexer/tokens/Token.tu"
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu" 
  export var all
  
  var tCond : ^LinkedList := nil
  var tThen : ^LinkedList := nil
  var tElse : ^LinkedList := nil
  
  body function ToString() : string
  
    if tCond ~= nil then
      put "IF"
      put tCond->ToString()
    end if
    if tThen ~= nil then
      put "THEN"
      put tThen->ToString()
    end if
    if tElse ~= nil then
      put "ELSE"
      put tElse->ToString()
      put "END"
    end if
    result "IF"
  end ToString
  
end CondToken