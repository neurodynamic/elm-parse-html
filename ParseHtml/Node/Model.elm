module ParseHtml.Node.Model exposing (..)

import ParseHtml.Node.Element.Attribute exposing (Attribute)


type alias Name =
    String


type alias Content =
    String


type Node
    = Element Name (List Attribute) (List Node)
    | TextNode Content
    | Comment Content
