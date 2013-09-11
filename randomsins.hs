module Main where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Random
import System.Random
import qualified Data.Vector.Unboxed as U

compCount = 1000
sinusLength = 4000
period = (2, 500)
amplitude = (0.1, 2)

main = do
  g <- newStdGen
  res <- return $ (flip evalRand) g $ do
      com <- genSins . U.fromList <$> (replicateM compCount $ genSinComponent)
      spec1 <- genSins . U.fromList <$> (replicateM compCount $ genSinComponent)
      spec2 <- genSins . U.fromList <$> (replicateM compCount $ genSinComponent)
      spec3 <- genSins . U.fromList <$> (replicateM compCount $ genSinComponent)
      spec4 <- genSins . U.fromList <$> (replicateM compCount $ genSinComponent)
      return $ force (U.zipWith (+) com spec1,
                      U.zipWith (+) com spec2,
                      U.zipWith (+) com spec3,
                      U.zipWith (+) com spec4)
  _ <- evaluate $!! res
  return ()

  where
    genSinComponent :: (RandomGen g) => Rand g (Double, Double, Double)
    genSinComponent = do
      per <- getRandomR period
      amp <- getRandomR amplitude
      ph <- getRandomR (0, per)
      return (ph, per, amp)

    genSins :: U.Vector (Double, Double, Double) -> U.Vector Double
    genSins comps = U.fromList $ map (genSin comps) [0..fromIntegral sinusLength]

    genSin :: U.Vector (Double, Double, Double) -> Double -> Double
    genSin comps x = U.foldl' (\res (ph, per, amp) -> res + (amp * (sin $ (x/per*2*pi)+ph ))) 0 comps
