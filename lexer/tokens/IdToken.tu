unit class IdToken
  inherit Token
  export var value, var style, Style
  
  var value : string
  
  type Style : enum (PREFIX, INFIX, POSTFIX)
  var style : Style
  
  body function ToString() : string
    case style of
      label Style.PREFIX : result "Prefix Identifier '" + value + "'"
      label Style.INFIX  : result "Infix Identifier '" + value + "'"
      label Style.POSTFIX: result "Postfix Identifier '" + value + "'"
    end case
  end ToString
  
end IdToken