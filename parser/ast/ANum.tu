unit class ANum
  inherit Expr
  export var all
  
  var value : int
  
  body proc print()
    put value ..
  end print

end ANum