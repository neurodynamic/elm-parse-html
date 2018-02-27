module ParseHtml.Errors exposing (translateError, ErrorLineParts, errorLineParts)

import Parser exposing (Error, Problem(..), Context)
import Array


type alias ErrorLineParts =
    { beforeIssue : String
    , issue : String
    , afterIssue : String
    }


errorLineParts : Error -> ErrorLineParts
errorLineParts error =
    let
        line =
            errorLine error
    in
        case headContext error of
            Just { row, col, description } ->
                ErrorLineParts (String.left error.col line)
                    (String.slice error.col (error.col + 1) line)
                    (String.dropLeft (error.col + 1) line)

            Nothing ->
                ErrorLineParts (String.left error.col line)
                    (String.slice error.col (error.col + 1) line)
                    (String.dropLeft (error.col + 1) line)


errorLine : Error -> String
errorLine error =
    error
        |> .source
        |> String.lines
        |> Array.fromList
        |> Array.get (error.row - 1)
        |> Maybe.withDefault "Couldn't find the line this error happened on."


translateError : Error -> String
translateError error =
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

            ( BadOneOf _, "closing tag or next child node" ) ->
                "This \"" ++ extraData ++ "\" element needs a \"</" ++ extraData ++ ">\" tag to close it."

            ( problem, desc ) ->
                "Uncategorized problem: " ++ (toString problem) ++ ", in context: " ++ desc


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
