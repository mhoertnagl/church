unit class IUnpack
  inherit Instruction
  %export var all
  
  %var count : int
  
  body fcn ToString() : string
    result "Unpack"
  end ToString
  
end IUnpack