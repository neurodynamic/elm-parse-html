module Node.Text exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Node.Text exposing (textNode)
import ParseHtml.Node.Model exposing (Node(..))


suite : Test
suite =
    describe "The Text module"
        [ describe "Text.textNode"
            [ test "Returns the whole string if there are no < characters in it."
                <| \_ ->
                    let
                        goodTextString =
                            "Hey yo I'm a string!"
                    in
                        Expect.equal (run textNode goodTextString) (Ok (TextNode goodTextString))
            , test "Succeeds the string until the first < character if it contains one."
                <| \_ ->
                    let
                        goodTextString =
                            "Fiddlestic|<s!"
                    in
                        Expect.equal (run textNode goodTextString) (Ok (TextNode "Fiddlestic|"))
            , test "Fails if the string starts with a < character."
                <| \_ ->
                    let
                        badTextString =
                            "<Fiddlesticks!"

                        result =
                            run textNode badTextString
                    in
                        expectProblem result ExpectingVariable
            ]
        ]
