unit class IPushGlobal
  inherit Instruction
  export var all
  
  var name : string
  
  body fcn ToString() : string
    result "PushGlobal " + name
  end ToString
  
end IPushGlobal