module ParseHtml.Node.Element exposing (..)

import Parser exposing (..)
import ParseHtml.Utils exposing (..)
import ParseHtml.Utils exposing (..)
import ParseHtml.Node.Model exposing (..)
import ParseHtml.Node.Element.Attribute exposing (..)


elementWithChildrenFunc : Parser (List Node) -> Parser Node
elementWithChildrenFunc childrenParsingFunc =
    oneOf
        [ selfClosingTagElement
        , tagWrappedElement childrenParsingFunc
        ]


selfClosingTagElement : Parser Node
selfClosingTagElement =
    inContext "self-closing tag"
        <| (delayedCommitMap (\tag _ -> tag)
                (succeed Element
                    |. symbol "<"
                    |= xmlTagName
                    |= attributeList
                    |. optionalSpaces
                    |. symbol "/>"
                    |= succeed []
                )
                (succeed ())
           )


tagWrappedElement : Parser (List Node) -> Parser Node
tagWrappedElement childrenParsingFunc =
    inContext "html element"
        <| (delayedCommitMap (\result _ -> result) openingTag optionalSpaces
                |= childrenParsingFunc
                |> andThen closingTagFor
           )


openingTag : Parser (List Node -> Node)
openingTag =
    succeed Element
        |. symbol "<"
        |= xmlTagName
        |= attributeList
        |. symbol ">"


closingTagFor : Node -> Parser Node
closingTagFor node =
    case node of
        (Element name _ _) as element ->
            succeed identity
                |. symbol ("</" ++ name ++ ">")
                |= succeed element

        TextNode content ->
            fail "Tried to find a closing tag for a text node. This is a bug in the parser."

        Comment content ->
            fail "Tried to find a closing html tag for an html comment. This is a bug in the parser."



{-
   XML elements must follow these naming rules:

   Names can contain letters, numbers, and other characters
   Names cannot start with a number or punctuation character
   Names cannot start with the letters xml (or XML, or Xml, etc)
   Names cannot contain spaces
-}
