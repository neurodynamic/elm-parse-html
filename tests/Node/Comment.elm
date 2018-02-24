module Node.Comment exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
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
                            goodComment =
                                "<!--" ++ commentContent ++ "-->"
                        in
                            Expect.equal (run comment goodComment) (Ok (Comment commentContent))
                , test "Fails if the comment has no opening tag."
                    <| \_ ->
                        let
                            badComment =
                                commentContent ++ "-->"
                        in
                            Expect.equal (run comment badComment)
                                (Err
                                    { row = 1
                                    , col = 1
                                    , source = badComment
                                    , problem = ExpectingSymbol "<!--"
                                    , context = [ { row = 1, col = 1, description = "comment" } ]
                                    }
                                )
                , test "Fails if the comment has no closing tag."
                    <| \_ ->
                        let
                            badComment =
                                "<!--" ++ commentContent
                        in
                            Expect.equal (run comment badComment)
                                (Err
                                    { row = 1
                                    , col = 5
                                    , source = badComment
                                    , problem = ExpectingClosing "-->"
                                    , context = [ { row = 1, col = 1, description = "comment" } ]
                                    }
                                )
                ]
            ]
