unit class IPushInt
  inherit Instruction
  export var all
  
  var value : int
  
  body fcn ToString() : string
    result "PushInt " + intstr(value)
  end ToString
  
end IPushInt