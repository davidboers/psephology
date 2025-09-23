-- | Tools for analyzing and calculating proportional representation.
module Psephology.ProportionalRepresentation
    ( -- * General
      gallagherIndex
    , aboveThreshold
    , apparentment
    , level
    , levelStartingWith
    , listSeats
    , listSeatsWOOverhang
    , preserveMajority
    , preserveMajorityProportional
    , seatAssignmentByCompetitor

      -- * Entitlement
    , entitlement
    , upperQuota
    , lowerQuota

      -- * House monotonicity
    , isHouseMonotoneAt
    , alabamas

      -- * Rules
    , checkRelative
    , correctNumSeats
    )
where

import Data.List (findIndices, intersect, nub, singleton, sortOn, findIndex)

import Psephology.Utils (incrementAt, indices)

-- | @'gallagherIndex' votes seats@ returns the index of relative disproportionality between
-- @votes@ received and @seats@ won.
--
-- \[ G = \sqrt{\frac{1}{2}\sum_{i=1}^{n}(V_i-S_i)^2} \]
--
-- Both the @votes@ and @seats@ arguments should represent the integer number of votes or seats
-- won. However, Gallagher gives an index whose values range between 0 and 100. The argument lists
-- are converted to lists of the percentages of votes/seats, multiplied by 100.
--
-- \[   \sigma_v = \sum_{i=1}^{n}V_i\\
--      \sigma_s = \sum_{i=1}^{n}S_i\\
--      V_i := \frac{V_i}{\sigma_v} \times 100\\
--      S_i := \frac{S_i}{\sigma_s} \times 100\\
-- \]
gallagherIndex :: [Int] -> [Int] -> Double
gallagherIndex votes seats =
    let totalVotes = sum votes
        totalSeats = sum seats

        votesP = map (\vi -> fromIntegral vi / fromIntegral totalVotes * 100) votes
        seatsP = map (\si -> fromIntegral si / fromIntegral totalSeats * 100) seats

        sumOfSquares = sum $ zipWith (\vpi spi -> (vpi - spi) ** 2) votesP seatsP
     in sqrt $ sumOfSquares / 2

-- | Returns the list of indexes of competitors that achieved the threshold.
--
-- Given the threshold \(t\), and the tally list for \(k\) competitors \(V=\{v_0,\ldots,v_{k-1}\}\):
--
-- \[   \{ i | i\in\{0,\ldots,k-1\}, V_i \geq t^\prime \}\\
--      t^\prime = t \times \sigma\\
--      \sigma = \sum{V}\\
-- \]
aboveThreshold :: Double -> [Int] -> [Int]
aboveThreshold threshold votes =
    let totalVotes = sum votes
        thresholdVotes = fromIntegral totalVotes * threshold
     in findIndices (\vi -> fromIntegral vi >= thresholdVotes) votes

-- | @'apparentment' m apparentments votes x@ allocates @x@ seats between @apparentments@, and then 
-- between members of each apparentment. Each apparentment is a list of competitor indexes. 
-- Individual competitors may not be members of multiple apparentments, otherwise the apparentment 
-- list will be disregarded.
apparentment :: ([Int] -> Int -> [Int]) -> [[Int]] -> [Int] -> Int -> [Int]
apparentment m apparentments votes x
    | not $ validateApparentments apparentments = m votes x
    | otherwise = apparentmentValidated m apparentments votes x

apparentmentValidated :: ([Int] -> Int -> [Int]) -> [[Int]] -> [Int] -> Int -> [Int]
apparentmentValidated m apparentments votes x =
    let apparentments' = apparentments ++ map singleton (notInApparentments (length votes) apparentments)
        seatsByApparentment = m (tallyByApparentment votes apparentments') x
        seatsByCompetitor = [ m (map (votes !!) (apparentments' !! i)) (seatsByApparentment !! i) | i <- indices apparentments' ]
     in
    map snd $ sortOn fst $ zip (concat apparentments') (concat seatsByCompetitor)

validateApparentments :: [[Int]] -> Bool
validateApparentments apparentments =
    all null
        [ x `intersect` y
        | x <- nub apparentments
        , y <- nub apparentments
        , x /= y
        ]

notInApparentments :: Int -> [[Int]] -> [Int]
notInApparentments ck apparentments =
    let inApparentments = concat apparentments in
    filter (`notElem` inApparentments) [0 .. ck - 1]

tallyByApparentment :: [Int] -> [[Int]] -> [Int]
tallyByApparentment votes apparentments =
    [ sum $ map (votes !!) a
    | a <- apparentments
    ]

-- | @'level' m seatsWithoutLeveling votes@ adds additional seats until every competitor has at 
-- least as many seats as in @seatsWithoutLeveling@. 
level :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> [Int]
level m seatsWithoutLeveling votes =
    leveled m seatsWithoutLeveling votes (sum seatsWithoutLeveling)

-- | @'levelStartingWith' m seatsWithoutLeveling votes k@ is the same as 'level', ensuring at
-- least @k@ seats are allocated.
levelStartingWith :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> Int -> [Int]
levelStartingWith = leveled

leveled :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> Int -> [Int]
leveled m seatsWithoutLeveling votes k
    | isLevel seatsWithoutLeveling seats = seats
    | otherwise = leveled m seatsWithoutLeveling votes (k + 1)
    where seats = m votes k

isLevel :: [Int] -> [Int] -> Bool
isLevel lhs rhs =
    all (uncurry (<=)) $ zip lhs rhs

-- | @'listSeats' m lowerSeats votes xl@ returns the number of list seats for each competitor, 
-- which may be higher than @xl@ if there are overhang seats. New Zealand-style MMP.
listSeats :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> Int -> [Int]
listSeats m lowerSeats votes xl =
    let xc = sum lowerSeats in
    zipWith (\ci ti -> max 0 (ti - ci))
        lowerSeats
        (m votes (xc + xl))

-- | Iterate 'listSeats', removing a list seat until no more overhang is present. This is not the
-- recommended method, 'highestAveragesWithInit' is preferred. This method should be used only in
-- conjunction with a largest remainder method, where there is no other way to award compensatory
-- seats. Bolivian-style MMP.
listSeatsWOOverhang :: ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> Int -> [Int]
listSeatsWOOverhang m lowerSeats votes xl
    | overhang == 0 = aln
    | otherwise     = listSeats m lowerSeats votes (xl - overhang)
    where 
        aln = listSeats m lowerSeats votes xl
        overhang = sum aln - xl

-- | @'preserveMajority' m votes x@ ensures that a party that has won a majority of votes will win 
-- at least half of the seats, by adding seats to that party until the condition is satisfied. This 
-- is unnecessary for the 'dhondt' method.
preserveMajority :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> [Int]
preserveMajority m votes x =
    case withMajority votes of
        Just w  -> withSeatMajority w a
        Nothing -> a
    where
        a = m votes x
        withSeatMajority w ae
            | ae !! w >= halfSeats (sum ae) = ae
            | otherwise                     = withSeatMajority w $ incrementAt ae w

preserveMajorityProportional :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> [Int]
preserveMajorityProportional m votes x =
    case withMajority votes of
        Nothing -> a
        Just w  | a !! w >= halfSeats x -> a
                | otherwise             -> preserveMajorityProportional m votes (x + 1)
    where
        a = m votes x
    
withMajority :: [Int] -> Maybe Int    
withMajority votes = 
    let voteMajorityMark = sum votes `div` 2 in
    findIndex (> voteMajorityMark) votes

halfSeats :: Int -> Int
halfSeats x | even x    =  x `div` 2
            | otherwise = (x `div` 2) + 1

-- | @'seatAssignmentByCompetitor' m votes x@ returns a list of length @x@. The list element at
-- each index @n@ is the competitor('s index) that wins the final seat if @x@ were set to @n + 1@.
--
-- Assumes that @m@ 'isHouseMonotoneAt' the interval @[1 .. x]@
seatAssignmentByCompetitor :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> [Int]
seatAssignmentByCompetitor m votes x =
    mapWithLastArg whoGained (replicate (length votes) 0) $ map (m votes) [1 .. x]

whoGained :: [Int] -> [Int] -> Int
whoGained al an =
    head $ findIndices (uncurry (>)) $ zip an al

mapWithLastArg :: (a -> a -> b) -> a -> [a] -> [b]
mapWithLastArg _ _  []       = []
mapWithLastArg f fl (x : xs) = let fn = f fl x in fn : mapWithLastArg f x xs

-- * Entitlement

-- | @'entitlement' votes i x@ returns the [entitlement](https://en.wikipedia.org/wiki/Entitlement_(fair_division))
-- of competitor @i@, normalized by the number of seats @x@. In other words, the proportion of
-- @votes@ for @i@ multiplied by the number of seats.
--
-- \[ t_i = \frac{v_i}{\sum_{j=1}^{n} v_j} \cdot x \]
entitlement :: [Int] -> Int -> Int -> Double
entitlement votes i x =
    fromIntegral (votes !! i) / fromIntegral (sum votes) * fromIntegral x

-- | A competitor's 'entitlement' rounded up to the nearest integer.
--
-- @ upperQuota votes i x = ceiling $ entitlement votes i x @
--
-- \[ U_i = \lceil t_i \rceil \]
upperQuota :: [Int] -> Int -> Int -> Int
upperQuota votes i x =
    ceiling $ entitlement votes i x

-- | A competitor's 'entitlement' rounded down to the nearest integer.
--
-- @ lowerQuota votes i x = floor $ entitlement votes i x @
--
-- \[ L_i = \lfloor t_i \rfloor \]
lowerQuota :: [Int] -> Int -> Int -> Int
lowerQuota votes i x =
    floor $ entitlement votes i x

-- * House monotonicity

-- | @'isHouseMonotoneAt' m votes x@ returns @True@ if there are no 'alabamas' at @x@.
isHouseMonotoneAt :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> Bool
isHouseMonotoneAt m votes x =
    null $ alabamas m votes x

-- | @'alabamas' m votes x@ indicates which competitors would lose a seat if an additional seat was
-- added, given allocation method @m@, @votes@, and original number of seats @x@.
alabamas :: ([Int] -> Int -> [Int]) -> [Int] -> Int -> [Int]
alabamas m votes x =
    let a = m votes x
        ap1 = m votes (x + 1)
     in findIndices (uncurry (>)) $ zip a ap1

-- * Properties

matchups :: [a] -> [(Int, Int)]
matchups l = 
    [ (i, j)
    | i <- indices l
    , j <- indices l
    , i > j
    ]

none :: Foldable t => (a -> Bool) -> t a -> Bool
none f = not . any f

improperOrder :: [Int] -> [Int] -> Int -> Int -> Bool
improperOrder votes seats i j =
    case compare (votes !! i) (votes !! j) of
        GT -> seats !! i <  seats !! j
        LT -> seats !! i >  seats !! j
        EQ -> abs ((seats !! i) - (seats !! j)) > 1

-- | @'checkRelative' votes seats@ determines whether every competitor obeys the requirement that 
-- the proper relative position, with respect to votes, is also obeyed with respect to seats. In 
-- other words, if party x receives less votes than party y, x should get a number of seats less 
-- than or equal to y, and if x receives more votes than y, the opposite should be true. Stated 
-- formally (for each @i@, @j@):
--
-- @
--  case compare (votes !! i) (votes !! j) of
--      GT -> seats !! i <  seats !! j
--      LT -> seats !! i >  seats !! j
--      EQ -> abs ((seats !! i) - (seats !! j)) > 1
-- @
checkRelative :: [Int] -> [Int] -> Bool
checkRelative votes seats =
    none (uncurry (improperOrder votes seats)) $ matchups votes

-- | @'correctNumSeats' x seats@ determines whether the correct number of seats @x@ has been 
-- allocated.
correctNumSeats :: Int -> [Int] -> Bool
correctNumSeats x seats =
    sum seats == x