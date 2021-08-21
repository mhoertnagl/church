unit class NGlobal
  inherit Node
  import LinkedList in "../../util/LinkedList.tu"
  export var all
  
  var name : string
  var arity : int
  var code : ^LinkedList
  
  body proc print()
    put name, "/", intstr(arity) ..
  end print
  
  body fcn ToString() : string
    result name + "/" + intstr(arity) + " = " + code->ToString()
  end ToString
  
end NGlobal