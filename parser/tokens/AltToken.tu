unit class AltToken
  inherit Token in "../../lexer/tokens/Token.tu"
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu" 
  export var all
  
  var tMatch : ^LinkedList := nil
  var tExpr : ^LinkedList := nil
  
  body function ToString() : string
  
    if tMatch ~= nil then
      put tMatch->ToString() ..
    end if
    if tExpr ~= nil then
      put " -> " ..
      put tExpr->ToString() ..
    end if
    result "ALT"
  end ToString
  
end AltToken