module Node.Text exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
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
                        textString =
                            "Hey yo I'm a string!"
                    in
                        Expect.equal (run textNode textString) (Ok (TextNode textString))
            , test "Succeeds the string until the first < character if it contains one."
                <| \_ ->
                    let
                        textString =
                            "Fiddlestic|<s!"
                    in
                        Expect.equal (run textNode textString) (Ok (TextNode "Fiddlestic|"))
            , test "Fails if the string starts with a < character."
                <| \_ ->
                    let
                        textString =
                            "<Fiddlesticks!"
                    in
                        Expect.equal (run textNode textString)
                            (Err
                                { row = 1
                                , col = 1
                                , source = textString
                                , problem = BadRepeat
                                , context = [ { row = 1, col = 1, description = "text" } ]
                                }
                            )
            ]
        ]
