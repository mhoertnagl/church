unit class EndToken
  inherit Token
  
  body function ToString() : string
    result "END"
  end ToString
  
end EndToken