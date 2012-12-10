{-# OPTIONS_GHC -Wall #-}

import Control.Applicative ((<$>))
import System.Random (mkStdGen, StdGen, randomR)
import Control.Monad.State
import Text.Printf (printf)
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import Data.Foldable (foldlM)
import Debug.Trace (trace)

data Parameters = Parameters {
                      w :: Int,
                      h :: Int,
                      t :: Int,
                      s :: Int,
                      n :: Int
                  }
                  deriving Show

type Pack = V.Vector (V.Vector Int)
type Stage = V.Vector (V.Vector Int)
type Point = (Int, Int)
type Block = Int

data OutOfStageError = X | Y

main :: IO ()
main =
    --mainRelease
    mainEmu
    --mainRotate

mainRelease :: IO ()
mainRelease =
    do
        p <- readParameters
        packs <- readPacks p
        --print p
        --mapM_ putStrLn $ map show $ head packs
        mapM_ putStrLn $ map outputString $ fst $ runState (randomOutputs p) (mkStdGen 33)

readParameters :: IO Parameters
readParameters =
    do
        [w', h', t', s', n'] <- map read . words <$> getLine
        return $ Parameters w' h' t' s' n'

readPacks :: Parameters -> IO [Pack]
readPacks p =
    replicateM (n p) $ do
        ls <- V.replicateM (t p) $ V.fromList . (map read . words) <$> getLine
        _ <- getLine
        return ls

randomOutputs :: Parameters -> State StdGen [(Int, Int)]
randomOutputs p =
    do
        replicateM (n p) $ randomOutput (w p) (t p)
    where
        randomOutput :: Int -> Int -> State StdGen (Int, Int)
        randomOutput w' t' =
            do
                x <- state $ randomR (0, w'-t')
                r <- state $ randomR (0, 3)
                return (x, r)

outputString :: (Int, Int) -> String
outputString (x, r) = (show x) ++ " " ++ (show r)


-- | エミュレーションテスト
mainEmu :: IO ()
mainEmu =
    do
        -- とりあえず、あふれたときはエラーで止まる
        p <- readParameters
        packs <- readPacks p
        let
            st = emptyStage (w p) (h p)
        mapM_ (putStrLn . showStage) $ bfs p packs [st]

mainRotate :: IO ()
mainRotate = do
    let
        p = Parameters {
                w = 10,
                h = 16,
                t = 4,
                s = 10,
                n = 1000
        }
        pack = V.fromList $ map V.fromList [[1..4],[5..8],[9..12],[13..16]]
    putStrLn $ showStage pack
    mapM_ (putStrLn . showStage . (rotatePack p) pack) [0..3]

emptyStage :: Int -> Int -> Stage
emptyStage w' h' = V.replicate h' $ V.replicate w' 0

showStage :: Stage -> String
showStage = unlines . map (unwords . (map (printf "%2d")) . V.toList) . V.toList

dropPack :: Parameters -> Int -> Pack -> Stage -> Either OutOfStageError Stage
dropPack p x pack stage =
    let
        t' = (t p)
    in
        foldlM dropPack' stage [(bx, by) | bx <- [0..(t'-1)], by <- [(t'-1),(t'-2)..0]]
        where
            dropPack' :: Stage -> Point -> Either OutOfStageError Stage
            dropPack' stage' bp@(bx, _) = dropBlock p (x+bx) (blockAt bp pack) stage'

dropBlock :: Parameters -> Int -> Block -> Stage -> Either OutOfStageError Stage
dropBlock p x b stage = putBlock p (x, emptyBottom x stage) b stage

putBlock :: Parameters -> Point -> Block -> Stage -> Either OutOfStageError Stage
putBlock p (x, y) b st
    | x < 0      = Left X
    | (w p) <= x = Left X
    | y < 0      = Left Y
    | (h p) <= y = Left Y
    | otherwise  = Right $ st // [(y, (st ! y) // [(x, b)])]

-- | ブロックを落とすと止まる場所（y座標）
emptyBottom :: Int -> Stage -> Int
emptyBottom x st = (V.length $ V.takeWhile (== 0) $ V.map (!x) st) - 1

vconcat :: V.Vector (V.Vector a) -> V.Vector a
vconcat = V.concat . V.toList

blockAt :: Point -> Pack -> Block
blockAt (x, y) p = p ! y ! x

rotatePack :: Parameters -> Pack -> Int -> Pack
rotatePack _ pack 0 = pack
rotatePack p pack 1 = V.fromList $ map (V.reverse . column pack) [0..(t p)-1]
rotatePack p pack 2 = V.fromList $ map (V.reverse . (pack!)) [(t p)-1,(t p)-2..0]
rotatePack p pack 3 = V.fromList $ map (column pack) [(t p)-1,(t p)-2..0]
rotatePack _ _ r = error $ (show r) ++ " is not a rotation number (in rotatePack)"

column :: Pack -> Int -> V.Vector Block
column pack x = V.map (!x) pack

bfs :: Parameters -> [Pack] -> [Stage] -> [Stage]
bfs _ [] stages = stages
bfs p packs stages = do
    let t' = (t p)
        w' = (w p)
        pk = head packs
        pks = tail packs
    r <- [0..3]
    x <- [1-t'..w'-1]
    st <- stages
    let next = case dropPack p x (rotatePack p pk r) st of
                   Right st' -> [st']
                   Left X    -> []
                   Left Y    -> [] -- TODO 終了
    bfs p pks next
