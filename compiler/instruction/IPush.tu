unit class IPush
  inherit Instruction
  export var all
  
  var position : int
  
  body fcn ToString() : string
    result "Push " + intstr(position)
  end ToString
  
end IPush