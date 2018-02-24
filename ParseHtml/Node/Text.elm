module ParseHtml.Node.Text exposing (textNode)

import Parser exposing (..)
import Parser.LanguageKit exposing (variable)
import Char
import Set
import ParseHtml.Node.Model exposing (..)


textNode : Parser Node
textNode =
    inContext "text"
        <| (succeed TextNode
                |= variable (\char -> char /= '<') (\char -> char /= '<') Set.empty
           )
