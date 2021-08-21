unit class NPack
  inherit Node
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu"
  export var all
  
  var name : string
  var params : ^LinkedList
  
  body proc New(obj : ^Object)
    this := obj
    new params
  end New
  
  body fcn ToString() : string
    result name + " " + params->ToString()
  end ToString
  
  body proc print()
  
    var iter : ^Iterator := params->GetIterator()
    var node : ^Node
    
    put name, " " ..
    loop exit when not(iter->HasNext())
      node := iter->Next()
      node->print()
    end loop
  end print
  
end NPack