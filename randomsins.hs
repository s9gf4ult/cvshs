module Main where


import Control.DeepSeq
import Control.Monad.Random
import System.Random
import qualified Data.Vector.Unboxed as U

compCount = 1000
sinusLength = 4000
period = (2, 500)
amplitude = (0.1, 2)

main = do
  g <- newStdGen
  res <- evalRand g $ do
      com <- U.fromList <$> replicateM compCount $ genSinComponent
      spec1 <- U.fromList <$> replicateM compCount $ genSinComponent
      spec2 <- U.fromList <$> replicateM compCount $ genSinComponent
      spec3 <- U.fromList <$> replicateM compCount $ genSinComponent
      spec4 <- U.fromList <$> replicateM compCount $ genSinComponent
      return $ force (mergeComps com spec1,
                      mergeComps com spec2,
                      mergeComps com spec3,
                      mergeComps com spec4)
  _ <- evaluate $!! res
  return ()

  where
    genSinComponent :: Rand (U.Vector (Double, Double, Double))
    genSinComponent = do
      per <- getRandomR period
      amp <- getRandomR amplitude
      ph <- getRandomR (0, per)
      return (ph, per, amp)
    mergeComps :: U.Vector (Double, Double, Double) -> U.Vector (Double, Double, Double) -> U.Vector Double
    mergeComps com spec = U.zipWith (\a b -> a + b + hi) (genSins com) (genSins spec)
  
    genSins :: U.Vector (Double, Double, Double) -> U.Vector Double
    genSins comps = U.fromList $ map (genSin comps) [0,2..fromIntegral len]

    genSin :: U.Vector (Double, Double, Double) -> Double -> Double
    genSin comps x = U.foldl' (\res (ph, (per, amp)) -> res + (amp * (sin $ (x/per*2*pi)+ph ))) 0 comps
  
