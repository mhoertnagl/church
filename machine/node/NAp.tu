unit class NAp
  inherit Node
  export var all
  
  var left, right : ^Node
  
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
  
  body fcn ToString() : string
    result "(" + left->ToString() + " " + right->ToString() + ")"
  end ToString
  
end NAp