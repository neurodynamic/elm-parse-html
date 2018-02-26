module ParseHtml exposing (..)

import Parser exposing (..)
import ParseHtml.Node.Model exposing (Node)
import ParseHtml.Node.Element exposing (element)
import ParseHtml.Utils exposing (optionalSpaces)


document : Parser Node
document =
    succeed identity
        |. symbol "<!DOCTYPE html>"
        |. optionalSpaces
        |= element
        |. optionalSpaces
        |. end


parseDocument : String -> Result Error Node
parseDocument html =
    Parser.run document html
