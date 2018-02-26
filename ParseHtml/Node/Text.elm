module ParseHtml.Node.Text exposing (textNode)

import Parser exposing (..)
import Char
import Set
import ParseHtml.Node.Model exposing (Node(..))


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
