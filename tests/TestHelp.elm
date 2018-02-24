module TestHelp exposing (..)

import ParseHtml.Node.Model exposing (Node)
import Parser
import Expect


expectProblem : Result Parser.Error Node -> Parser.Problem -> Expect.Expectation
expectProblem result expectedProblem =
    case result of
        Err error ->
            Expect.equal error.problem expectedProblem

        _ ->
            Expect.fail "Result should have been an error but wasn't."
