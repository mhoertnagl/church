unit class CommandToken
  inherit Object in "../util/Object.tu"
  export var all
  
  var value : string
  
  body fcn ToString() : string
    result value
  end ToString
  
end CommandToken