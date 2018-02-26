module ParseHtml exposing (document, parseDocument)

import Parser exposing (..)
import ParseHtml.Node.Model exposing (Node)
import ParseHtml.Node.Element exposing (element)
import ParseHtml.Utils exposing (optionalSpaces)


{-| This library is for parsing HTML documents. It uses the [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/2.0.1) library to produce parsing results or appropriate types of errors, so you'll probably want to be familiar with that library as well to use this one. ParseHtml.Node.Model has the type you'll get from a successful parse.
-}
{-| Parser for an HTML document. Requires a DOCTYPE declaration and at least one HTML element to succeed.

    Parser.run document "<!DOCTYPE html><html></html>" == Ok (Element "html" [] [])
    Parser.run document "<!DOCTYPE html><html><head></head><body><h1>I am a document!</h1></body></html>" = Element "html" [] [ Element "head" [] [], Element "body" [] [ Element "h1" [] [ TextNode "I am a document!" ]] ]
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

    parseDocument "<!DOCTYPE html><html></html>" == Ok (Element "html" [] [])
-}
parseDocument : String -> Result Error Node
parseDocument html =
    Parser.run document html
