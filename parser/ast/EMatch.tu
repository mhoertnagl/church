unit class EMatch
  inherit Expr
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu"
  export var all
  
  var mexp : ^Expr
  var alts : ^LinkedList
  
  body proc New(obj : ^Object)
    this := obj
    new alts
  end New
  
  body proc print()
    put "match " ..
    
    if mexp ~= nil then
      mexp->print()
    else
      put "(nil)"
    end if
    
    put " with \n   " ..
    
    var iter : ^Iterator := alts->GetIterator()
    var expr : ^Expr

    loop exit when not(iter->HasNext())
    
      expr := iter->Next()
      if expr ~= nil then
        expr->print()
      else
        put "(nil)"
      end if
      
      if iter->HasNext() then
        put "\n | " ..
      end if
    end loop
    
    put "\n end " ..
  end print

end EMatch