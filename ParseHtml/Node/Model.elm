module ParseHtml.Node.Model exposing (Node(..))

{-| This data model represents an HTML node. In most common use, it will probably represent a complete tree of HTML elements parsed from an HTML document.
# Definition
@docs Node
-}

import ParseHtml.Node.Element.Attribute exposing (Attribute)


type alias Name =
    String


type alias Content =
    String


{-| Represents an HTML node. In most common use, will probably represent a complete tree of HTML elements parsed from an HTML document.

    rootNode = Element "html" [] [ Element "head" [] [], Element "body" [] [ Element "h1" [] [ TextNode "I am a document!" ]] ]
-}
type Node
    = Element Name (List Attribute) (List Node)
    | TextNode Content
    | Comment Content
