unit class NumToken
  inherit Token
  export var all
  
  var value : int

  body function ToString() : string
    result "Integer '" + intstr(value) + "'"
  end ToString
  
end NumToken