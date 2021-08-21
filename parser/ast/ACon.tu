unit class ACon
  inherit Expr
  export var all
  
  var name : string
  var arity : int
  
  body proc print()
    put name ..
  end print

end ACon