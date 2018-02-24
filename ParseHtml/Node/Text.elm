module ParseHtml.Node.Text exposing (textNode)

import Parser exposing (..)
import Char
import ParseHtml.Node.Model exposing (..)


textNode : Parser Node
textNode =
    inContext "text"
        <| succeed TextNode
        |= keep oneOrMore (\char -> char /= '<')
