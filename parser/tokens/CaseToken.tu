unit class CaseToken
  inherit Token in "../../lexer/tokens/Token.tu"
  import LinkedList in "../../util/LinkedList.tu",
         Iterator   in "../../util/Iterator.tu" 
  export var all
  
  var tMatch : ^LinkedList := nil
  var tAlts : ^LinkedList := nil
  
  body function ToString() : string
  
    if tMatch ~= nil then
      put "MATCH"
      put tMatch->ToString()
    end if
    if tAlts ~= nil then
      put "WITH"
      var iter : ^Iterator := tAlts->GetIterator()
      loop exit when not(iter->HasNext())
        put "| ", iter->Next()->ToString()
      end loop
    end if
    result "MATCH"
  end ToString
  
end CaseToken