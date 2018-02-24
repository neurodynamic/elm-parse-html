module Node.Test exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser exposing (run, Error)
import ParseHtml.Node.Text exposing (textNode)
import ParseHtml.Node.Model exposing (Node(..))


suite : Test
suite =
    describe "The Text module"
        [ describe "Text.textNode"
            [ test "Returns the string if there are no < characters in it."
                <| \_ ->
                    let
                        textString =
                            "Hey yo I'm a string!"
                    in
                        Expect.equal (run textNode textString) (Ok (TextNode textString))
            , test "Fails if there are < characters."
                <| \_ ->
                    let
                        textString =
                            "<Fiddlesticks!"
                    in
                        Expect.equal (run textNode textString) (Err (Error "Message"))
            ]
        ]
