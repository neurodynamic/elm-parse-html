module ParseHtml.Node.Element.Attribute
    exposing
        ( Attribute
        , attributeList
        )

import ParseHtml.Utils exposing (..)
import Parser exposing (..)


type alias Attribute =
    ( String, Maybe String )


attributeList =
    repeat zeroOrMore nextAttribute


nextAttribute : Parser Attribute
nextAttribute =
    delayedCommit spaces attribute


attribute : Parser Attribute
attribute =
    succeed identity
        |= xmlTagName
        |> andThen attributeValue


attributeValue : String -> Parser ( String, Maybe String )
attributeValue attributeName =
    succeed (,)
        |= succeed attributeName
        |= Parser.oneOf
            [ valueAfterEquals attributeName
            , Parser.succeed Nothing
            ]


valueAfterEquals : String -> Parser (Maybe String)
valueAfterEquals attributeName =
    inContext attributeName
        <| (inContext "attribute"
                <| Parser.succeed Just
                |. symbol "="
                |= tagNameOrQuotedString
           )


tagNameOrQuotedString : Parser String
tagNameOrQuotedString =
    Parser.oneOf
        [ xmlTagName
        , doubleQuotedString
        , singleQuotedString
        ]


singleQuotedString : Parser String
singleQuotedString =
    succeed identity
        |. symbol "'"
        |= keep zeroOrMore (\char -> char /= '\'')
        |. symbol "'"


doubleQuotedString : Parser String
doubleQuotedString =
    succeed identity
        |. symbol "\""
        |= keep zeroOrMore (\char -> char /= '"')
        |. symbol "\""
