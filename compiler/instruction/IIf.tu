unit class IIf
  inherit Instruction
  import LinkedList in "../../util/LinkedList.tu"
  export var all
  
  var cThen : ^LinkedList
  var cElse : ^LinkedList
  
  body fcn ToString() : string
    result "If " + cThen->ToString() + ", " + cElse->ToString()
  end ToString
  
end IIf