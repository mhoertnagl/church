unit class ISlide
  inherit Instruction
  export var all
  
  var count : int
  
  body fcn ToString() : string
    result "Slide " + intstr(count)
  end ToString
  
end ISlide