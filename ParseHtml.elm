module ParseHtml exposing (..)

import Parser exposing (..)
import ParseHtml.Node.Model exposing (..)
import ParseHtml.Node.Element exposing (..)
import ParseHtml.Node.Comment exposing (..)
import ParseHtml.Node.Text exposing (..)
import ParseHtml.Utils exposing (..)


document : Parser Node
document =
    succeed identity
        |. symbol "<!DOCTYPE html>"
        |. optionalSpaces
        |= element
        |. optionalSpaces
        |. end


parseHtml : String -> Result Error Node
parseHtml html =
    Parser.run document html


parseHtmlWithDefault : (Error -> Node) -> String -> Node
parseHtmlWithDefault errorFunc html =
    case parseHtml html of
        Ok rootNode ->
            rootNode

        Err error ->
            errorFunc error
