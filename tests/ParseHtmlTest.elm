module ParseHtmlTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Node.Model exposing (Node(..))
import ParseHtml exposing (..)


suite : Test
suite =
    describe "The ParseHtml module"
        [ describe "ParseHtml.document"
            [ test "Returns a document."
                <| \_ ->
                    let
                        doc =
                            "<!DOCTYPE html><html></html>"

                        result =
                            run document doc
                    in
                        Expect.equal result (Ok (Element "html" [] []))
            , test "Fails without DOCTYPE."
                <| \_ ->
                    let
                        doc =
                            "<html></html>"

                        result =
                            run document doc
                    in
                        expectProblem result
                            (ExpectingSymbol "<!DOCTYPE html>")
            ]
        ]
