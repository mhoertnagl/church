unit class EAp
  inherit Expr
  export var all
  
  var left, right : ^Expr
  
  body proc print()
    put "(" ..
    
    if left ~= nil then
      left->print()
    else
      put "(nil)" ..
    end if
    
    put " " ..
    
    if right ~= nil then
      right->print()
    else
      put "(nil)" ..
    end if
    
    put ")" ..
  end print

end EAp