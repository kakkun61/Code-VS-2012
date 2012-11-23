import Control.Applicative ((<$>))
import System.Random
import Control.Monad.State
import Text.Printf (printf)

type Pack = [[Int]]
type Stage = [[Int]]

main :: IO ()
main =
    --mainRelease
    mainEmu

mainRelease :: IO ()
mainRelease =
    do
        [w, h, t, s, n] <- map read . words <$> getLine
        packs <- readPacks n t
        mapM_ putStrLn $ map outputString $ fst $ runState (randomOutputs w t) (mkStdGen 33)


readPacks :: Int -> Int -> IO [Pack]
readPacks 0 _ = return []
readPacks n t =
    do
        p <- readPack t
        ps <- readPacks (n-1) t
        return $ p:ps
    where
        readPack :: Int -> IO Pack
        readPack 0 = getLine >> return []
        readPack t =
            do
                l <- map read . words <$> getLine
                ls <- readPack (t-1)
                return $ l:ls

randomOutputs :: Int -> Int -> State StdGen [(Int, Int)]
randomOutputs w t =
    do
        o <- randomOutput w t
        os <- randomOutputs w t
        return $ o:os
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

putPack :: Int -> Pack -> Stage -> Maybe Stage
putPack x pack stage =
    let
        overL = not $ null $ filter (0<) $ concat $ map (take (-x)) pack
        overR = not $ null $ filter (0<) $ concat $ map (drop ((length pack)-(x+(length pack)-(length $ stage !! 0)))) pack
