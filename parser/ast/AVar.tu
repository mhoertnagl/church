unit class AVar
  inherit Expr
  export var all
  
  var name : string
  
  body proc print()
    put name ..
  end print
  
  body fcn ToString() : string
    result name
  end ToString
  
end AVar