module ParseHtml.Node.Element exposing (element)

{-| Parsing for HTML elements.
# Functions
@docs element
-}

import Parser exposing (..)
import ParseHtml.Node.Model exposing (Node(..))
import ParseHtml.Node.Element.Attribute exposing (Attribute, attributeList)
import ParseHtml.Node.Comment exposing (comment)
import ParseHtml.Node.Text exposing (textNode)
import ParseHtml.Utils exposing (optionalSpaces, xmlTagName)


{-| Parses an HTML element.

    Parser.run element "<p></p>" == Ok (Element "p" [] [])
    Parser.run element "<p class="myclass"></p>" == Ok (Element "p" [("class", "myclass")] [])
    Parser.run element "<p>Hi!</p>" == Ok (Element "p" [] [ TextNode "Hi!" ])
-}
element : Parser Node
element =
    lazy
        (\_ ->
            openTagStart
                |> Parser.andThen openTagEnd
        )


openTagStart : Parser (List Node -> Node)
openTagStart =
    succeed Element
        |. symbol "<"
        |= inContext "tag name" xmlTagName
        |= attributeList
        |. optionalSpaces


openTagEnd : (List Node -> Node) -> Parser Node
openTagEnd elementFunc =
    oneOf
        [ tagSelfClose elementFunc
        , openTagEndFollowedByChildrenAndClosingTag elementFunc
        ]


tagSelfClose : (List Node -> Node) -> Parser Node
tagSelfClose elementFunc =
    case elementFunc [] of
        (Element tagName attrs []) as element ->
            succeed element
                |. optionalSpaces
                |. symbol "/>"

        _ ->
            fail "tagSelfClose got passed something that wasn't an Element. This is a bug in the parser."


openTagEndFollowedByChildrenAndClosingTag : (List Node -> Node) -> Parser Node
openTagEndFollowedByChildrenAndClosingTag elementFunc =
    case elementFunc [] of
        (Element tagName attrs []) as element ->
            succeed element
                |. symbol ">"
                |> andThen closingTagOrNextChildNodeFor

        _ ->
            fail "openTagEndFollowedByChildrenAndClosingTag got passed something that wasn't an Element. This is a bug in the parser."


closingTagOrNextChildNodeFor : Node -> Parser Node
closingTagOrNextChildNodeFor node =
    case node of
        Element name attributes children ->
            oneOf
                [ closingTagFor node
                , comment
                    |> andThen (addChildAndCheckAgain name attributes children)
                , element
                    |> andThen (addChildAndCheckAgain name attributes children)
                , textNode
                    |> andThen (addChildAndCheckAgain name attributes children)
                ]

        TextNode content ->
            fail "Tried to find a closing tag for a text node. This is a bug in the parser."

        Comment content ->
            fail "Tried to find a closing html tag for an html comment. This is a bug in the parser."


node : Parser Node
node =
    lazy
        (\_ ->
            oneOf [ comment, element, textNode ]
        )


addChildAndCheckAgain : String -> List Attribute -> List Node -> Node -> Parser Node
addChildAndCheckAgain elName elAttrs elChildren newChild =
    let
        allChildren =
            elChildren ++ [ newChild ]

        newElement =
            Element elName elAttrs allChildren
    in
        closingTagOrNextChildNodeFor newElement


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
