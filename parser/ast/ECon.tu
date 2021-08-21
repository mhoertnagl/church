unit class ECon
  inherit Expr
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu"
  export var all
  
  var name : string
  var params : ^LinkedList
  
  body proc New(obj : ^Object)
    this := obj
    new params
  end New
  
  body proc print()
    put name ..

    var iter : ^Iterator := params->GetIterator()
    var expr : ^Expr

    if iter->HasNext() then
      put " " ..
    end if
    
    loop exit when not(iter->HasNext())
    
      expr := iter->Next()
      if expr ~= nil then
        expr->print()
      else
        put "(nil)" ..
      end if
      
      if iter->HasNext() then
        put " " ..
      end if
    end loop
  end print

end ECon