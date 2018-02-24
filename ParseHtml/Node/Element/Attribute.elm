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
    succeed (,)
        |= xmlTagName
        |= attributeValue


attributeValue : Parser (Maybe String)
attributeValue =
    Parser.oneOf
        [ Parser.delayedCommitMap always valueAfterEquals (Parser.succeed ())
        , Parser.succeed Nothing
        ]


valueAfterEquals : Parser (Maybe String)
valueAfterEquals =
    Parser.succeed Just
        |. symbol "="
        |= tagNameOrQuotedString


tagNameOrQuotedString : Parser String
tagNameOrQuotedString =
    Parser.oneOf
        [ xmlTagName
        , quotedString
        ]


quotedString : Parser String
quotedString =
    oneOf
        [ singleQuotedString
        , doubleQuotedString
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
