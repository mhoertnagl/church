unit class IPop
  inherit Instruction
  export var all
  
  var size : int
  
  body fcn ToString() : string
    result "Pop " + intstr(size)
  end ToString  
  
end IPop