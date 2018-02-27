module ParseHtml.Errors exposing (translateError)

import Parser exposing (Error, Problem(..))


translateError : Error -> String
translateError error =
    let
        description =
            headContextFor error

        extraData =
            extraDataFor error
    in
        case ( error.problem, description ) of
            ( _, "DOCTYPE" ) ->
                "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."

            ( _, "tag name" ) ->
                "After opening an element with a \"<\", you need to provide a tag name. For example, \"<section>\"."

            ( _, "attribute" ) ->
                "All HTML attributes followed by an \"=\" must have a value. For example, \"<section class=\"my-class\">\""

            ( BadOneOf [ ExpectingSymbol "/>", ExpectingSymbol ">" ], "end of tag" ) ->
                "HTML tags must end with \">\" or \"/>\". For example, \"<section>\""

            ( BadOneOf _, "closing tag or next child node" ) ->
                "This \"" ++ extraData ++ "\" element needs a \"</" ++ extraData ++ ">\" tag to close it."

            ( problem, desc ) ->
                "Uncategorized problem: " ++ (toString problem) ++ ", in context: " ++ desc


headContextFor : Error -> String
headContextFor error =
    error
        |> .context
        |> List.head
        |> Maybe.map .description
        |> Maybe.withDefault "No context."


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
