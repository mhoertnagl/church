unit class NString
  inherit Node
  export var all
  
  var value : string
  
  body proc print()
    put "'", value, "'" ..
  end print
  
  body fcn ToString() : string
    result "'" + value + "'"
  end ToString
  
end NString