import Euterpea
-- konwersja z Pitch na AbsPitch
toAbsPitch :: [Pitch] -> [AbsPitch]
toAbsPitch [] = []
toAbsPitch (x:xs) =  absPitch x : toAbsPitch xs

-- konwersja z AbsPitch na Pitch
toPitch :: [AbsPitch] -> [Pitch]
toPitch [] = []
toPitch (x:xs) = pitch x : toPitch xs

--dodanie nut

-- Definicja nuty (rownorzedny zapis)
 
--Prim (Note qn (C, 4))
--note qn (C, 4)
--c 4 qn

pcToSN :: PitchClass -> Music Pitch
pcToSN pc = note sn (pc, 4)

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

pcToHN :: PitchClass -> Music Pitch
pcToHN pc = note hn (pc, 4) 
