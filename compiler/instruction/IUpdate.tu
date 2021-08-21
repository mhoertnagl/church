unit class IUpdate
  inherit Instruction
  export var all
  
  var size : int
  
  body fcn ToString() : string
    result "Update " + intstr(size)
  end ToString  
  
end IUpdate