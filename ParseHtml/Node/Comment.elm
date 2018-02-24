module ParseHtml.Node.Comment exposing (comment)

import Parser exposing (..)
import Char
import ParseHtml.Node.Model exposing (..)
import ParseHtml.Utils exposing (..)


comment : Parser Node
comment =
    inContext "comment"
        <| succeed Comment
        |. openingTag
        |= keepUntilExclusive closeCommentSymbol


commentExcludingClosingTag : String -> Node
commentExcludingClosingTag string =
    String.slice 0 -3 string
        |> Comment


openingTag : Parser ()
openingTag =
    symbol "<!--"


closeCommentSymbol : String
closeCommentSymbol =
    "-->"
