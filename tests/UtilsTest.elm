module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Utils exposing (xmlTagName, spaces, optionalSpaces, keepUntilExclusive)


suite : Test
suite =
    describe "The Utils module"
        [ describe "Utils.xmlTagName"
            [ test "Parses a corrent tag name."
                <| \_ ->
                    let
                        result =
                            run xmlTagName "goodTagName"
                    in
                        Expect.equal result (Ok "goodTagName")
            , test "Fails on invalid tag name."
                <| \_ ->
                    let
                        result =
                            run xmlTagName "///"
                    in
                        expectProblem result ExpectingVariable
            ]
        , describe "Utils.spaces"
            [ test "Parses whitespace."
                <| \_ ->
                    let
                        result =
                            run spaces "   \n\n\t  \n "
                    in
                        Expect.equal result (Ok ())
            , test "Fails when no whitespace."
                <| \_ ->
                    let
                        result =
                            run spaces ""
                    in
                        expectProblem result BadRepeat
            ]
        , describe "Utils.optionalSpaces"
            [ test "Parses whitespace."
                <| \_ ->
                    let
                        result =
                            run optionalSpaces "   \n\n\t  \n "
                    in
                        Expect.equal result (Ok ())
            , test "Succeeds when no whitespace."
                <| \_ ->
                    let
                        result =
                            run optionalSpaces ""
                    in
                        Expect.equal result (Ok ())
            ]
        , describe "Utils.keepUntilExclusive"
            [ test "Keeps string until meeting specified symbol, excluding symbol from results."
                <| \_ ->
                    let
                        result =
                            run (keepUntilExclusive "-->") "Hello, there!-->"
                    in
                        Expect.equal result (Ok "Hello, there!")
            , test "Fails when symbol not found"
                <| \_ ->
                    let
                        result =
                            run (keepUntilExclusive "-->") "Hello, there!"
                    in
                        expectProblem result (ExpectingClosing "-->")
            ]
        ]
