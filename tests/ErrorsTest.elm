module ErrorsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Errors exposing (translateError, ErrorLineParts)
import ParseHtml exposing (parseDocument)


suite : Test
suite =
    describe "The Errors module"
        [ describe "Errors.translateError"
            [ test "Requests DOCTYPE tag."
                <| \_ ->
                    let
                        result =
                            parseDocument "some stuff"
                    in
                        expectErrorTranslation result
                            ( "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."
                            , ErrorLineParts "" "s" "ome stuff"
                            )
            , test "Explains need for element tag name"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<"
                    in
                        expectErrorTranslation result
                            ( "After opening an element with a \"<\", you need to provide a tag name. For example, \"<section>\"."
                            , ErrorLineParts "<" "" ""
                            )
            , test "Explains need for element attribute value after equals"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section attr= "
                    in
                        expectErrorTranslation result
                            ( "All HTML attributes followed by an \"=\" must have a value. For example, \"attr='some value'\"."
                            , ErrorLineParts "<section attr=" " " ""
                            )
            , test "Explains need for closing \">\" symbol"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section<"
                    in
                        expectErrorTranslation result
                            ( "HTML tags must end with \">\" or \"/>\". For example, \"<section>\""
                            , ErrorLineParts "<section" "<" ""
                            )
            , test "Explains need for closing tag"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section></xxxx>"
                    in
                        expectErrorTranslation result
                            ( "This \"section\" element needs a \"</section>\" tag to close it."
                            , ErrorLineParts "<section></" "x" "xxx>"
                            )
            ]
        ]


expectErrorTranslation : Result Parser.Error a -> ( String, ErrorLineParts ) -> Expect.Expectation
expectErrorTranslation result expectedErrorData =
    case result of
        Err error ->
            Expect.equal (translateError error) expectedErrorData

        result ->
            let
                printedResult =
                    Debug.log "Unexpected result:" result
            in
                Expect.fail "Result should have been an Err but wasn't."
