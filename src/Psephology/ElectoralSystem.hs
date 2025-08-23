module Psephology.ElectoralSystem
    ( ElectoralSystem
    , systems
    ) where

import Psephology.Candidate
import Psephology.Voter

import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Condorcet
import Psephology.ElectoralSystems.Plurality
import Psephology.ElectoralSystems.Rated
import Psephology.ElectoralSystems.Runoff

type ElectoralSystem a = [Candidate] -> [a] -> Int

systems :: Voter a => [(String, ElectoralSystem a)]
systems =
    [ ("FPTP", firstPastThePost)
    , ("Anti-plurality", antiPlurality)
    , ("TRS", twoRound)
    , ("IRV", instantRunoffVoting)
    , ("Coombs", coombsMethod)
    , ("Borda", bordaCount)
    , ("Dowdall", dowdallSystem)
    , ("Icelandic Borda", icelandicBorda)
    , ("Nanson", nansonsMethod)
    , ("Baldwin", baldwinsMethod)
    , ("Tideman alternative", tidemanAlternative)
    , ("Minimax", minimax)
    , ("Copeland-Llull", copelandLlull)
    , ("Black", black traditionalBordaWeight)
    , ("Ranked pairs", rankedPairs)
    , ("Approval", approvalVoting)
    , ("Highest median", highestMedian 0 10)
    , ("Score", scoreVoting 0 10)
    , ("STAR", starVoting 0 10)
    ]
