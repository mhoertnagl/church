unit class SubExprToken
  inherit Token in "../../lexer/tokens/Token.tu"
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu" 
  export var all

  var tExpr : ^LinkedList := nil
  
  body function ToString() : string
    if tExpr ~= nil then
      put tExpr->ToString()
    end if
    result "("
  end ToString
  
end SubExprToken