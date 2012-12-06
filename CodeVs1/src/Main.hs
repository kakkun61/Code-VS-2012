import Control.Applicative ((<$>), (<*>))
import System.Random
import Control.Monad.State
import Text.Printf (printf)
import qualified Data.Vector as V
import Data.Vector ((!), (//))
import Debug.Trace

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
        [w, h, t, s, n] <- map read . words <$> getLine
        return $ Parameters w h t s n

readPacks :: Parameters -> IO [Pack]
readPacks p =
    replicateM (n p) $ readPack (t p)
    where
        readPack :: Int -> IO Pack
        readPack t =
            do
                ls <- V.replicateM t $ V.fromList . (map read . words) <$> getLine
                getLine
                return ls

randomOutputs :: Parameters -> State StdGen [(Int, Int)]
randomOutputs p =
    do
        replicateM (n p) $ randomOutput (w p) (t p)
    where
        randomOutput :: Int -> Int -> State StdGen (Int, Int)
        randomOutput w t =
            do
                x <- state $ randomR (0, w-t)
                r <- state $ randomR (0, 3)
                return (x, r)

outputString :: (Int, Int) -> String
outputString (x, r) = (show x) ++ " " ++ (show r)


-- | エミュレーションテスト
mainEmu :: IO ()
mainEmu =
    do
        let
            p = Parameters {
                    w = 10,
                    h = 16,
                    t = 4,
                    s = 10,
                    n = 1000
                }
            --st = putBlock (0, 15) 1 $ putBlock (1, 14) 2 $ putBlock (2, 13) 3 $ putBlock (3, 12) 4 $ emptyStage (w p) (h p)
            st = emptyStage (w p) (h p)
            pk = V.fromList $ map V.fromList [[1,0,0,3],
                                              [0,0,0,0],
                                              [3,3,0,0],
                                              [0,0,0,0]]
        mapM_ (putStrLn . showStage) $ bfs p [pk] [st]

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
emptyStage w h = V.replicate h $ V.replicate w 0

showStage :: Stage -> String
showStage = unlines . map (unwords . (map (printf "%2d")) . V.toList) . V.toList

dropPack :: Parameters -> Int -> Pack -> Stage -> Maybe Stage
dropPack p x pack stage =
    let
        overL = not $ V.null $ V.filter (0<) $ vconcat $ V.map (V.take (-x)) pack
        overR = not $ V.null $ V.filter (0<) $ vconcat $ V.map (V.drop ((t p)-(x+(t p)-(w p)))) pack
        t' = (t p)
    in
        if overL || overR
            then Nothing
            else Just $ foldl dropPack' stage [(x, y) | x <- [0..(t'-1)], y <- [(t'-1),(t'-2)..0]]
        where
            dropPack' :: Stage -> Point -> Stage
            dropPack' stage bp@(bx, by) = dropBlock p (x+bx) (blockAt bp pack) stage

dropBlock :: Parameters -> Int -> Block -> Stage -> Stage
dropBlock p x b stage
    | x < 0      = stage
    | (w p) <= x = stage
    | otherwise  = putBlock (x, emptyBottom x stage) b stage


putBlock :: Point -> Block -> Stage -> Stage
putBlock (x, y) b s = s // [(y, (s ! y) // [(x, b)])]

-- | ブロックを落とすと止まる場所（y座標）
emptyBottom :: Int -> Stage -> Int
emptyBottom x s = (V.length $ V.takeWhile (== 0) $ V.map (!x) s) - 1

vconcat :: V.Vector (V.Vector a) -> V.Vector a
vconcat = V.concat . V.toList

blockAt :: Point -> Pack -> Block
blockAt (x, y) p = p ! y ! x

rotatePack :: Parameters -> Pack -> Int -> Pack
rotatePack p pack 0 = pack
rotatePack p pack 1 = V.fromList $ map (V.reverse . column pack) [0..(t p)-1]
rotatePack p pack 2 = V.fromList $ map (V.reverse . (pack!)) [(t p)-1,(t p)-2..0]
rotatePack p pack 3 = V.fromList $ map (column pack) [(t p)-1,(t p)-2..0]

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
                   Just st -> [st]
                   Nothing -> []
    bfs p pks next
