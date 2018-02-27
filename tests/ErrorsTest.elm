module ErrorsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Errors exposing (translateError, ErrorLine)
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
                            , ErrorLine "" "s" "ome stuff" 1
                            )
            , test "Explains need for element tag name"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<"
                    in
                        expectErrorTranslation result
                            ( "After opening an element with a \"<\", you need to provide a tag name. For example, \"<section>\"."
                            , ErrorLine "<" "" "" 2
                            )
            , test "Explains need for element attribute value after equals"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section attr= "
                    in
                        expectErrorTranslation result
                            ( "All HTML attributes followed by an \"=\" must have a value. For example, \"attr='some value'\"."
                            , ErrorLine "<section attr=" " " "" 2
                            )
            , test "Explains need for closing \">\" symbol"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section<"
                    in
                        expectErrorTranslation result
                            ( "HTML tags must end with \">\" or \"/>\". For example, \"<section>\""
                            , ErrorLine "<section" "<" "" 2
                            )
            , test "Explains need for closing tag"
                <| \_ ->
                    let
                        result =
                            parseDocument "<!DOCTYPE html>\n<section></xxxx>"
                    in
                        expectErrorTranslation result
                            ( "This \"section\" element needs a \"</section>\" tag to close it."
                            , ErrorLine "<section></" "x" "xxx>" 2
                            )
            ]
        ]


expectErrorTranslation : Result Parser.Error a -> ( String, ErrorLine ) -> Expect.Expectation
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
