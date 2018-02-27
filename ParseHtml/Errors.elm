module ParseHtml.Errors exposing (translateError, ErrorLine)

{-| Tools for producing useful error messages.
# Functions
@docs translateError
# Data
@docs ErrorLine
-}

import Parser exposing (Error, Problem(..), Context)
import Array


{-| Converts a Parser.Error record into a human-readable description of the error, and the line on which the error occurs, broken into the columns before the error, at the error, and past the error.

    case parseDocument "some stuff" of
        {- ...other cases... -}
        Err error ->
            translateError error
    == ( "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."
    , ErrorLine "" "s" "ome stuff"
    )
-}
translateError : Error -> ( String, ErrorLine )
translateError error =
    ( errorDescription error, errorLine error )


{-| Holds the line number and the line an error occurred on in three parts: the part before the issue that caused the error, the part that caused the error, and the part after the issue that caused the error.
-}
type alias ErrorLine =
    { beforeIssue : String
    , issue : String
    , afterIssue : String
    , number : Int
    }


errorLine : Error -> ErrorLine
errorLine error =
    let
        line =
            errorLineString error

        errorColIndex =
            error.col - 1
    in
        ErrorLine (String.left errorColIndex line)
            (String.slice errorColIndex (errorColIndex + 1) line)
            (String.dropLeft (errorColIndex + 1) line)
            error.row


errorLineString : Error -> String
errorLineString error =
    error
        |> .source
        |> String.lines
        |> Array.fromList
        |> Array.get (error.row - 1)
        |> Maybe.withDefault "Couldn't find the line this error happened on."


errorDescription : Error -> String
errorDescription error =
    let
        description =
            headContextDescriptionFor error

        extraData =
            extraDataFor error
    in
        case ( error.problem, description ) of
            ( _, "DOCTYPE" ) ->
                "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."

            ( _, "tag name" ) ->
                "After opening an element with a \"<\", you need to provide a tag name. For example, \"<section>\"."

            ( _, "attribute" ) ->
                "All HTML attributes followed by an \"=\" must have a value. For example, \"" ++ extraData ++ "='some value'\"."

            ( BadOneOf [ ExpectingSymbol "/>", ExpectingSymbol ">" ], "end of tag" ) ->
                "HTML tags must end with \">\" or \"/>\". For example, \"<section>\""

            ( _, "closing tag" ) ->
                "This \"" ++ extraData ++ "\" element needs a \"</" ++ extraData ++ ">\" tag to close it."

            ( problem, desc ) ->
                "Uncategorized problem: " ++ (toString problem) ++ ", in context: \"" ++ desc ++ "\"."


headContextDescriptionFor : Error -> String
headContextDescriptionFor error =
    error
        |> headContext
        |> Maybe.map .description
        |> Maybe.withDefault "No context."


headContext : Error -> Maybe Context
headContext error =
    error
        |> .context
        |> List.head


{-| Sometimes we need extra data on an error for a good message. Like the name of the tag that the error happened in. We store this in the second-most-recent context if so. This function retrieves that context's description to get the data.
-}
extraDataFor : Error -> String
extraDataFor error =
    error
        |> .context
        |> List.tail
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.map .description
        |> Maybe.withDefault "No data."
