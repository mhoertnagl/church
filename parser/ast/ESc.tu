unit class ESc
  inherit Expr
  import HashMap in "../../util/HashMap.tu"
  export var all
  
  var name : string
  var vars : ^HashMap
  var expr : ^Expr

  body proc New(obj : ^Object)
    this := obj
    new vars
    vars->New(vars)
  end New

  body proc print()
    put name, " ", vars->ToString(), " = " ..
    
    if expr ~= nil then
      expr->print()
    else
      put "(nil)" ..
    end if
  end print

end ESc