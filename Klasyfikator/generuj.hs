import System.Random (randomRIO) -- biblioteka do generowania liczb pseudolosowych 21 dzwiekow
-- funkcje generujace listy 
randomFloat :: Float -> IO([Float])
randomFloat 0 = return []
randomFloat n = do
  r  <- randomRIO (20,108) -- zakres midi (https://pl.wikipedia.org/wiki/MIDI klawiatura)
  rs <- randomFloat (n-1) -- generuje liste dlugosci n-1 i przypisuje ja do rs
  return (r:rs)
  
randomBin :: Int -> IO([Int])
randomBin 0 = return []
randomBin n = do
    r <- randomRIO (1,0) -- zakres
    rs <- randomBin (n-1)
    return (r:rs)