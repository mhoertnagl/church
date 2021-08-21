unit class ListToken
  inherit Token in "../../lexer/tokens/Token.tu"
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu" 
  export var all
  
  var tElems : ^LinkedList := nil
  
  body function ToString() : string
    %print()
    result tElems->ToString()
  end ToString
  
end ListToken