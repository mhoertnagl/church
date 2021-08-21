unit class AString
  inherit Expr
  export var all
  
  var value : string
  
  body proc print()
    put "'", value, "'" ..
  end print

end AString