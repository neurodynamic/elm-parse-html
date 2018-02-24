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
            [ describe "element with opening and closing tags"
                [ test "Matches basic element."
                    <| \_ ->
                        let
                            goodElement =
                                "<p></p>"
                        in
                            Expect.equal (run element goodElement)
                                (Ok (Element "p" [] []))
                , test "Matches element attributes."
                    <| \_ ->
                        let
                            goodElement =
                                "<p class=\"some-class\"></p>"
                        in
                            Expect.equal (run element goodElement)
                                (Ok (Element "p" [ ( "class", Just "some-class" ) ] []))
                , test "Matches element attributes with no string."
                    <| \_ ->
                        let
                            goodElement =
                                "<button disabled></button>"
                        in
                            Expect.equal (run element goodElement)
                                (Ok (Element "button" [ ( "disabled", Nothing ) ] []))
                , test "Fails without closing tag."
                    <| \_ ->
                        let
                            badElement =
                                "<p>"

                            result =
                                run element badElement
                        in
                            case result of
                                Err error ->
                                    Expect.equal error.problem (ExpectingSymbol "</p>")

                                _ ->
                                    Expect.fail "Result wasn't an error."
                ]
            , test "Matches self-closing tags."
                <| \_ ->
                    let
                        goodElement =
                            "<input type=\"text\" />"
                    in
                        Expect.equal (run element goodElement)
                            (Ok (Element "input" [ ( "type", Just "text" ) ] []))
            ]
        ]
