unit class NNum
  inherit Node
  export var all
  
  var value : int
  
  body proc print()
    put value ..
  end print
  
  body fcn ToString() : string
    result intstr(value)
  end ToString
  
end NNum