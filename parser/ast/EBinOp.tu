unit class EBinOp
  inherit Expr
  export var all
  
  var op : string
  var left, right : ^Expr
  
  body proc print()
    put "(" ..
    
    if left ~= nil then
      left->print()
    else
      put "(nil)" ..
    end if
    
    put " ", op, " " ..
    
    if right ~= nil then
      right->print()
    else
      put "(nil)" ..
    end if
    
    put ")" ..
  end print

end EBinOp