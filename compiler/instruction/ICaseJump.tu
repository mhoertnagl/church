unit class ICaseJump
  inherit Instruction
  import HashMap in "../../Util/HashMap.tu"
  export var all
  
  var alts : ^HashMap
  
  body proc New(obj : ^Object)
    this := obj
    new alts
    alts->New(alts)
  end New
  
  body fcn ToString() : string
    result "ICaseJump " + alts->ToString()
  end ToString
  
end ICaseJump