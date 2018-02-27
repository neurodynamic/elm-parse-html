module ErrorsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Errors exposing (translateError)
import ParseHtml exposing (parseDocument)


suite : Test
suite =
    describe "The Errors module"
        [ describe "Errors.translateError"
            [ describe "document errors"
                [ test "Requires DOCTYPE tag."
                    <| \_ ->
                        let
                            result =
                                parseDocument "some stuff"
                        in
                            expectErrorTranslation result "Your HTML is missing a DOCTYPE declaration. Put \"<!DOCTYPE html>\" at the very beginning of the HTML."
                ]
              --, describe "textNode errors" []
            ]
        ]


expectErrorTranslation : Result Parser.Error a -> String -> Expect.Expectation
expectErrorTranslation result expectedErrorText =
    case result of
        Err error ->
            Expect.equal (translateError error) expectedErrorText

        result ->
            let
                printedResult =
                    Debug.log "Unexpected result:" result
            in
                Expect.fail "Result should have been an Err but wasn't."
