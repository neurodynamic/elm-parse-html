module ParseHtml.Base exposing (..)

import Parser exposing (..)
import ParseHtml.Node.Model exposing (..)
import ParseHtml.Node.Element exposing (..)
import ParseHtml.Node.Comment exposing (..)
import ParseHtml.Node.Text exposing (..)
import ParseHtml.Utils exposing (..)


rootNode : Parser Node
rootNode =
    succeed identity
        |. symbol "<!DOCTYPE html>"
        |. optionalSpaces
        |= node
        |. optionalSpaces
        |. end


node : Parser Node
node =
    lazy (\_ -> oneOf [ comment, element, textNode ])


nodeList : Parser (List Node)
nodeList =
    lazy (\_ -> repeat zeroOrMore node)


element : Parser Node
element =
    lazy (\_ -> elementWithChildrenFunc nodeList)


parseHtml : String -> Result Error Node
parseHtml html =
    Parser.run rootNode html


parseHtmlWithDefault : (Error -> Node) -> String -> Node
parseHtmlWithDefault errorFunc html =
    case parseHtml html of
        Ok rootNode ->
            rootNode

        Err error ->
            errorFunc error
