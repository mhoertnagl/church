unit class IPushString
  inherit Instruction
  export var all
  
  var value : string
  
  body fcn ToString() : string
    result "PushString '" + value + "'"
  end ToString
  
end IPushString