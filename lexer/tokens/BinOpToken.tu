unit class BinOpToken
  inherit Token
  export var all
  
  var op : string
  
  body fcn ToString() : string
    result "Op(" + op + ")"
  end ToString
  
end BinOpToken