module ParseHtml.Node.Text exposing (textNode)

{-| Parsing for HTML text nodes.

# Functions
@docs textNode
-}

import Parser exposing (..)
import Char
import Set
import ParseHtml.Node.Model exposing (Node(..))


{-| Parses an HTML text node (this will succeed on any string not starting with a "<" character, so make sure you run other parsers first).

    Parser.run textNode "some text" == Ok (textNode "some text")
    Parser.run textNode "<some text" == Err BadRepeat
-}
textNode : Parser Node
textNode =
    inContext "text"
        <| (succeed identity
                |= keep (Exactly 1) (\_ -> True)
                |> andThen textNodeOrFail
           )


textNodeOrFail : String -> Parser Node
textNodeOrFail string =
    if string == "<" then
        fail "Text nodes can't start with \"<\"."
    else
        succeed (\theRest -> TextNode (string ++ theRest))
            |= keep zeroOrMore (\char -> char /= '<')
