-- Use -fno-full-laziness compiler flag to optimise list comprehension performance.

import Prelude
import Data.List

type Cod = (Int, Int, Int)
type Seq = [Cod]
type Unit = [Seq]
type Tree = [Unit]

type CodFormat = (Int, Int, Int)

gaps :: [Int] -> [Int]
gaps [] = []
gaps xs = concat [[succ v1..pred v2] | (v1, v2) <- zip (0:xs) xs, succ v1 < v2]

compareCod :: Cod -> Cod -> Ordering
compareCod (u1, s1, e1) (u2, s2, e2) = 
  case compare u1 u2 of
    EQ -> case compare s1 s2 of
            EQ -> compare e1 e2
            sOrd -> sOrd
    uOrd -> uOrd

sortCods :: [Cod] -> [Cod]
sortCods = sortBy compareCod

compareSeq :: Seq -> Seq -> Ordering
compareSeq [] [] = EQ
compareSeq [] _ = LT
compareSeq _ [] = GT
compareSeq seq1 seq2 = compare s1 s2
  where
    (_, s1, _) = head seq1
    (_, s2, _) = head seq2

sortSeqs :: Unit -> Unit
sortSeqs = sortBy compareSeq

gapsFromCods :: [Cod] -> [Cod]
gapsFromCods [] = []
gapsFromCods cods = treeToCods $ mapTreeToGaps $ codsToTree cods

codsToTree :: [Cod] -> Tree
codsToTree [] = []
codsToTree cods = [[seq' | seq' <- groupBySequence unit] | unit <- groupByUnit cods]

treeToCods :: Tree -> [Cod]
treeToCods tree = concat $ concat tree

mapTreeToGaps :: Tree -> Tree
mapTreeToGaps = mapTreeToSeqGaps.mapTreeToExtGaps

mapTreeToSeqGaps :: Tree -> Tree
mapTreeToSeqGaps = map (\unit -> mergeUnits unit [[(u, s, 0)] | s <- gaps $ listSeq unit, let (u, _, _) = head $ head unit])

mergeUnits :: Unit -> Unit -> Unit
mergeUnits u1 u2 = sortSeqs $ u1 ++ u2

mapTreeToExtGaps :: Tree -> Tree
mapTreeToExtGaps = map (\unit -> mapUnitToExtGaps unit)

mapUnitToExtGaps :: Unit -> Unit
mapUnitToExtGaps = map (\seq' -> [(u, s, e) | e <- gaps $ listExt seq', let (u, s, _) = head seq'])

groupByUnit :: [Cod] -> [[Cod]]
groupByUnit = groupBy (\(u1, _, _) (u2, _, _) -> u1 == u2)

groupBySequence :: [Cod] -> [[Cod]]
groupBySequence = groupBy (\(_, s1, _) (_, s2, _) -> s1 == s2)

listSeq :: Unit -> [Int]
listSeq [] = []
listSeq unit = map (\((_, s, _):_) -> s) [seq' | seq' <- unit, not $ null seq']

listExt :: Seq -> [Int]
listExt [] = []
listExt seq' = [ e | (_, _, e) <- seq']

leadingZero :: Int -> Int -> String
leadingZero len num = reverse $ take len' (reverse (show num) ++ replicate leadingLen '0')
                      where 
                        numLen = length (show num)
                        leadingLen = len - numLen
                        len' = max len numLen

formatCod :: CodFormat -> Cod -> String
formatCod (l1, l2, l3) (unit, seq', ext) =  leadingZero l1 unit ++ "/" ++ 
                                            leadingZero l2 seq' ++ 
                                            case ext of 
                                              0 -> []
                                              _ -> "-" ++ leadingZero l3 ext

formatCods :: CodFormat -> [Cod] -> [String]
formatCods fmt = map (formatCod fmt)
