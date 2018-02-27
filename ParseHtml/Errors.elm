module ParseHtml.Errors exposing (translateError)

import Parser exposing (Error)


translateError : Error -> String
translateError error =
    let
        description =
            error
                |> .context
                |> List.head
                |> Maybe.map .description
                |> Maybe.withDefault "No context."
    in
        case description of
            "DOCTYPE" ->
                "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."

            _ ->
                "???"
