module Node.Element exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Node.Element exposing (element)
import ParseHtml.Node.Model exposing (Node(..))


suite : Test
suite =
    describe "The Element module"
        [ describe "Element.element"
            [ describe "self-closing tags"
                [ test "Matches self-closing tags."
                    <| \_ ->
                        let
                            goodElement =
                                "<input />"
                        in
                            Expect.equal (run element goodElement) (Ok (Element "input" [] []))
                ]
            ]
        ]
