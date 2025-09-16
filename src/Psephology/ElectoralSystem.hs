-- | This module contains helpers, and nothing substantive.
module Psephology.ElectoralSystem
    ( ElectoralSystem
    , systems
    , turnoutRequirement
    ) where

import Psephology.Candidate
import Psephology.Voter

import Psephology.ElectoralSystems.Borda
import Psephology.ElectoralSystems.Condorcet
import Psephology.ElectoralSystems.Plurality
import Psephology.ElectoralSystems.Rated
import Psephology.ElectoralSystems.Runoff
import Psephology.ElectoralSystems.Sortition

import System.Random (mkStdGen)

-- | All single-winner electoral systems in this library have the following type signature.
type ElectoralSystem a = [Candidate] -> [a] -> Int

-- | List of all single-winner electoral systems provided by the library. Excludes Dodgson due to runtime.
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
    , ("Kemeny", kemeny)
    , -- , ("Dodgson", dodgson)
      ("Ranked pairs", rankedPairs)
    , ("Schulze", schulze)
    , ("Approval", approvalVoting)
    , ("Highest median", highestMedian 0 10)
    , ("Score", scoreVoting 0 10)
    , ("STAR", starVoting 0 10)
    , ("Sortition", sortition g)
    , ("Random ballot", randomBallot g)
    ]
    where
        g = mkStdGen 0

-- | @'turnoutRequirement' threshold registered@ returns the winner if at least a certain portion @threshold@ of @registered@ voters cast ballots.
turnoutRequirement :: Voter a => Double -> Int -> ElectoralSystem a -> [Candidate] -> [a] -> Maybe Int
turnoutRequirement threshold registered es candidates voters
    | no_voters >= threshold * fromIntegral registered = return $ es candidates voters
    | otherwise = Nothing
    where no_voters = fromIntegral (length voters)