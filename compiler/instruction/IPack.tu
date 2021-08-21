unit class IPack
  inherit Instruction
  export var all
  
  var name : string
  var arity : int
  
  body fcn ToString() : string
    result "Pack '" + name + "' " + intstr(arity)
  end ToString
  
end IPack