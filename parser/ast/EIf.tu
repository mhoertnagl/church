unit class EIf
  inherit Expr
  export var all
  
  var eCond : ^Expr
  var eThen : ^Expr
  var eElse : ^Expr

  body proc print()
    put "if " ..
    
    if eCond ~= nil then
      eCond->print()
    else
      put "(nil)" ..
    end if
    
    put " then " ..
    
    if eThen ~= nil then
      eThen->print()
    else
      put "(nil)" ..
    end if
    
    put " else " ..
    
    if eElse ~= nil then
      eElse->print()
    else
      put "(nil)" ..
    end if
    
    put " end " ..
  end print
  
end EIf