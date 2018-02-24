module Node.Comment exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestHelp exposing (expectProblem)
import Parser exposing (run, Error, Problem(..))
import ParseHtml.Node.Comment exposing (comment)
import ParseHtml.Node.Model exposing (Node(..))


suite : Test
suite =
    let
        commentContent =
            "Hey yo I'm a comment!"
    in
        describe "The Comment module"
            [ describe "Comment.comment"
                [ test "Returns the whole string if there are no < characters in it."
                    <| \_ ->
                        let
                            result =
                                run comment ("<!--" ++ commentContent ++ "-->")
                        in
                            Expect.equal result (Ok (Comment commentContent))
                , test "Fails if the comment has no opening tag."
                    <| \_ ->
                        let
                            result =
                                run comment (commentContent ++ "-->")
                        in
                            expectProblem result (ExpectingSymbol "<!--")
                , test "Fails if the comment has no closing tag."
                    <| \_ ->
                        let
                            result =
                                run comment ("<!--" ++ commentContent)
                        in
                            expectProblem result (ExpectingClosing "-->")
                ]
            ]
