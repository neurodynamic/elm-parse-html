module ParseHtml.Node.Comment exposing (comment)

{-| Parsing for HTML comments.
# Functions
@docs comment
-}

import Parser exposing (..)
import Char
import ParseHtml.Node.Model exposing (Node(..))
import ParseHtml.Utils exposing (keepUntilExclusive)


{-| Parses an HTML comment

    Parser.run comment "<!--Comment!-->" == Ok (Comment "Comment!")
-}
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
