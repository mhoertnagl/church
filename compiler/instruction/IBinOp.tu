unit class IBinOp
  inherit Instruction
  export var all
  
  var op : string
  
  body fcn ToString() : string
    result "BinOp " + op
  end ToString
  
end IBinOp