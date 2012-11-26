import Control.Applicative ((<$>))
import System.Random
import Control.Monad.State
import Text.Printf (printf)

data Parameters = Parameters {
                      w :: Int,
                      h :: Int,
                      t :: Int,
                      s :: Int,
                      n :: Int
                  }
                  deriving Show

type Pack = [[Int]]
type Stage = [[Int]]
type Point = (Int, Int)
type Block = Int

main :: IO ()
main =
    mainRelease
    --mainEmu

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
                ls <- replicateM t $ map read . words <$> getLine
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
                x <- state $ randomR (0, w-t-1)
                r <- state $ randomR (0, 3)
                return (x, r)

outputString :: (Int, Int) -> String
outputString (x, r) = (show x) ++ " " ++ (show r)

{-
    エミュレーションテスト
-}
mainEmu :: IO ()
mainEmu =
    do
        let w = 10
            h = 16
            t = 4
            s = 10
            n = 1000
        putStrLn $ showStage $ emptyStage w h

emptyStage :: Int -> Int -> Stage
emptyStage w h = replicate h $ replicate w 0

showStage :: Stage -> String
showStage = unlines . map (unwords . (map (printf "%2d")))
{-
putPack :: Parameters -> Int -> Pack -> Stage -> Maybe Stage
putPack p x pack stage =
    let
        overL = not $ null $ filter (0<) $ concat $ map (take (-x)) pack
        overR = not $ null $ filter (0<) $ concat $ map (drop ((t p)-(x+(t p)-(w p)))) pack
    in
        if overL && overR
            then Nothing
            else dropBlock

dropBlock :: Point -> Block -> Stage
dropBlock = undefined
-}
