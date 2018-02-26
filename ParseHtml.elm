module ParseHtml exposing (document, parseDocument)

import Parser exposing (..)
import ParseHtml.Node.Model exposing (Node)
import ParseHtml.Node.Element exposing (element)
import ParseHtml.Utils exposing (optionalSpaces)


{-| Parser for an HTML document. Requires a DOCTYPE declaration and at least one HTML element to succeed.

    document "<!DOCTYPE html><html></html>" == Ok (Element "html" [] [])
-}
document : Parser Node
document =
    succeed identity
        |. symbol "<!DOCTYPE html>"
        |. optionalSpaces
        |= element
        |. optionalSpaces
        |. end


{-| Runs the document parser.
-}
parseDocument : String -> Result Error Node
parseDocument html =
    Parser.run document html
