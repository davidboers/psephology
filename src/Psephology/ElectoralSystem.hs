module Psephology.ElectoralSystem (
    ElectoralSystem,
    systems,
) where

import Psephology.Candidate
import Psephology.Voter

import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Condorcet
import Psephology.ElectoralSystems.Plurality
import Psephology.ElectoralSystems.Runoff

type ElectoralSystem a = [Candidate] -> [a] -> Int

systems :: (Voter a) => [(String, ElectoralSystem a)]
systems =
    [ ("FPTP", firstPastThePost)
    , ("Anti-plurality", antiPlurality)
    , ("TRS", twoRound)
    , ("IRV", instantRunoffVoting)
    , ("Coombs", coombsMethod)
    , ("Borda", bordaCount)
    , ("Dowdall", dowdallSystem)
    , ("Nanson", nansonsMethod)
    , ("Copeland-Llull", copelandLlull)
    , ("Baldwin", baldwinsMethod)
    , ("Ranked pairs", rankedPairs)
    ]
