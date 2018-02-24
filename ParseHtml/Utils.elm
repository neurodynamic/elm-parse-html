module ParseHtml.Utils exposing (..)

import Parser exposing (..)
import Parser.LanguageKit exposing (variable)
import Char
import Set


xmlTagName : Parser String
xmlTagName =
    variable isXMLStartChar isVarChar (Set.fromList [])


isXMLStartChar : Char -> Bool
isXMLStartChar char =
    Char.isLower char
        || Char.isUpper char
        || char
        == '_'
        || char
        == '-'


isVarChar : Char -> Bool
isVarChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char
        || char
        == '_'
        || char
        == '-'


optionalSpaces : Parser ()
optionalSpaces =
    ignore zeroOrMore isWhitespace


spaces : Parser ()
spaces =
    ignore oneOrMore isWhitespace


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\n' || char == '\x0D' || char == '\t' || char == '\x0D'


keepUntilInclusive : String -> Parser String
keepUntilInclusive str =
    Parser.source (ignoreUntil str)


keepUntilExclusive : String -> Parser String
keepUntilExclusive str =
    str
        |> keepUntilInclusive
        |> andThen (keepUntilHelper str)


{-| Normally, calling Parser.source on ignoreUntil
    will keep the delimiting string. We don't want
    that. So this helper removes that from the parse
    result.
-}
keepUntilHelper endString sourceString =
    sourceString
        |> String.dropRight (String.length endString)
        |> succeed
