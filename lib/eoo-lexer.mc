include "parser/lexer.mc"

lang BackslashTokenParser = TokenParser
  syn Token =
  | BackslashTok {info : Info}
  syn TokenRepr =
  | BackslashRepr ()

  sem parseToken (pos : Pos) =
  | "\\" ++ str ->
    let pos2 = advanceCol pos 1 in
    let info = makeInfo pos pos2 in
    {token = BackslashTok {info = info}, lit = "\\", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq (tokRepr : TokenRepr) =
  | BackslashTok _ -> match tokRepr with BackslashRepr _ then true else false

  sem tokInfo =
  | BackslashTok {info = info} -> info

  sem tokReprToStr =
  | BackslashRepr _ -> "<Backslash>"

  sem tokToStr =
  | BackslashTok _ -> "<Backslash>"

  sem tokToRepr =
  | BackslashTok _ -> BackslashRepr ()
end


lang SemiSemiTokenParser = TokenParser
  syn Token =
  | SemiSemiTok {info : Info}
  syn TokenRepr =
  | SemiSemiRepr ()
  sem parseToken (pos : Pos) =
  | ";;" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = SemiSemiTok {info = info}, lit = ";;", info = info, stream = {pos = pos2, str = str}}
  sem tokKindEq (tokRepr : TokenRepr) =
  | SemiSemiTok _ -> match tokRepr with SemiSemiRepr _ then true else false
  sem tokInfo =
  | SemiSemiTok {info = info} -> info
  sem tokReprToStr =
  | SemiSemiRepr _ -> "<SemiSemi>"
  sem tokToStr =
  | SemiSemiTok _ -> "<SemiSemi>"
  sem tokToRepr =
  | SemiSemiTok _ -> SemiSemiRepr ()
end

lang UnitTokenParser = TokenParser
  syn Token =
  | UnitTok {info : Info}
  syn TokenRepr =
  | UnitRepr ()
  sem parseToken (pos : Pos) =
  | "()" ++ str ->
    let pos2 = advanceCol pos 2 in
    let info = makeInfo pos pos2 in
    {token = UnitTok {info = info}, lit = "()", info = info, stream = {pos = pos2, str = str}}
  sem tokKindEq (tokRepr : TokenRepr) =
  | UnitTok _ -> match tokRepr with UnitRepr _ then true else false
  sem tokInfo =
  | UnitTok {info = info} -> info
  sem tokReprToStr =
  | UnitRepr _ -> "<Unit>"
  sem tokToStr =
  | UnitTok _ -> "<Unit>"
  sem tokToRepr =
  | UnitTok _ -> UnitRepr ()
end

lang LUIdentTokenParser = TokenParser
  syn Token =
  | LUIdentTok {info : Info, val : String}
  syn TokenRepr =
  | LUIdentRepr ()

  sem parseToken (pos : Pos) =
  | [(('_' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
      'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' |
      'x' | 'y' | 'z' )
    | ('A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
       'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' |
       'X' | 'Y' | 'Z' )) & c] ++ str ->
    match parseIdentCont (advanceCol pos 1) str with {val = val, pos = pos2, str = str}
    then
      let val = cons c val in
      let info = makeInfo pos pos2 in
      { token = LUIdentTok {info = info, val = val}
      , lit = val
      , info = info
      , stream = {pos = pos2, str = str}
      }
    else never

  sem tokKindEq (tokRepr : TokenRepr) =
  | LUIdentTok _ -> match tokRepr with LUIdentRepr _ then true else false

  sem tokInfo =
  | LUIdentTok {info = info} -> info

  sem tokReprToStr =
  | LUIdentRepr _ -> "<LUIdent>"

  sem tokToStr =
  | LUIdentTok tok -> concat "<LUIdent>" tok.val

  sem tokToRepr =
  | LUIdentTok _ -> LUIdentRepr ()
end

lang PrimeTokenParser = TokenParser
  syn Token =
  | PrimeTok {info : Info}
  syn TokenRepr =
  | PrimeRepr ()

  sem parseToken pos =
  | "'" ++ str ->
    let pos2 = advanceCol pos 1 in
    let info = makeInfo pos pos2 in
    {token = PrimeTok {info = info}, lit = "'", info = info, stream = {pos = pos2, str = str}}

  sem tokKindEq tokRepr =
  | PrimeTok _ -> match tokRepr with PrimeRepr _ then true else false

  sem tokInfo =
  | PrimeTok x -> x.info

  sem tokReprToStr =
  | PrimeRepr _ -> "'"

  sem tokToStr =
  | PrimeTok _ -> "'"

  sem tokToRepr =
  | PrimeTok _ -> PrimeRepr ()
end
