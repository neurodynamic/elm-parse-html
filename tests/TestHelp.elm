module TestHelp exposing (..)

import ParseHtml.Node.Model exposing (Node)
import Parser
import Expect


expectProblem : Result Parser.Error a -> Parser.Problem -> Expect.Expectation
expectProblem result expectedProblem =
    case result of
        Err error ->
            Expect.equal error.problem expectedProblem

        result ->
            let
                printedResult =
                    Debug.log "Unexpected result:" result
            in
                Expect.fail "Result should have been an Err but wasn't."
