unit class WithToken
  inherit Token
  
  body function ToString() : string
    result "WITH"
  end ToString
  
end WithToken