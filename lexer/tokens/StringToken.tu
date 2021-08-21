unit class StringToken
  inherit Token
  export var all
  
  var value : string
  
  body function ToString() : string
    result "\"" + value + "\""
  end ToString
  
end StringToken