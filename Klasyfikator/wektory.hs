import Euterpea

toAbsPitches :: [Pitch] -> [AbsPitch]
toAbsPitches ps = map absPitch ps
    
    
toPitches :: [AbsPitch] -> [Pitch]
toPitches as = map pitch as

-- Od Osterode
osterode :: [Pitch]
osterode = [(A, 4), (C, 4), (B, 4), (A, 4), (Gs, 4), (A, 4), (C, 4), (B, 4), (A, 4), (G, 4), (C, 4), (C, 4)]

osterodeAbs :: [AbsPitch]
osterodeAbs = [69,60,71,69,68,69,60,71,69,67,60,60]
osterodeRhytm :: [Int]
osterodeRhytm = [1,0,1,1,1,1,1,0,1,1,1,1,1,1]

-- Mazowsze
mazowsze :: [Pitch]
mazowsze = [(A, 4), (C, 4), (B, 4), (A, 4), (Gs, 4), (A, 4), (C, 4), (B, 4), (A, 4), (Gs, 4), (A, 4), (B, 4)]
mazowszeAbs :: [AbsPitch]
mazowszeAbs = [69,60,71,69,68,69,60,71,69,68,69,71]
mazowszeRhytm :: [Int]
mazowszeRhytm = [1,0,1,1,1,1,1,0,1,1,1,1,1,1]

-- Od Lwowa i Tarnowa

lwow :: [Pitch]
lwow = [(A, 4), (A, 4), (B, 4), (C, 4), (B, 4), (C, 4), (A, 4), (A, 4), (B, 4), (C, 4), (B, 4), (C, 4)]
lwowAbs :: [AbsPitch]
lwowAbs = [69,69,71,60,71,60,69,69,71,60,71,60]
lwowRhytm :: [Int]
lwowRhytm = [1,0,1,1,1,1,1,1,0,1,1,1,1,1]

-- Ostroleka

ostroleka :: [Pitch]
ostroleka = [(A, 4), (C, 4), (B, 4), (A, 4), (Gs, 4), (A, 4), (C, 4), (B, 4), (A, 4), (Gs, 4), (A, 4), (B, 4)]
ostrolekaAbs :: [AbsPitch]
ostrolekaAbs = [69,60,71,69,68,69,60,71,69,68,69,71]
ostrolekaRhytm :: [Int]
ostrolekaRhytm = [1,0,1,1,1,1,1,0,1,1,1,1,1,1]

plock :: [Pitch]
plock = [(A, 4), (A, 4), (C, 4), (B, 4), (Gs, 4), (A, 4),  (A, 4), (C, 4), (B, 4), (Gs, 4), (A, 4), (B, 4)]
plockAbs :: [AbsPitch]
plockAbs = [69,69,60,71,68,69,69,60,71,68,69,71]
plockRhytm :: [Int]
plockRhytm = [1,0,1,1,1,1,1,0,1,1,1,1,1,1]


-- JAZZZ

-- Fly to the moon 

moon :: [Pitch]
moon = [(Cs, 4), (D, 4), (A, 4), (A, 4), (A, 4), (A, 4), (A, 4), (B, 4), (C, 4), (B, 4), (G, 4), (G, 4), (G, 4)]
moonAbs :: [AbsPitch]
moonAbs = [61,62,69,69,69,69,69,71,60,71,67,67,67]
moonRhytm = [1,1,1,0,1,1,0,1,1,1,1,1,1,0]

-- Moon River

river :: [Pitch]
river = [(C, 4), (E, 4), (C, 4), (D, 4), (E, 4), (G, 4), (E, 4), (C, 4), (C, 4), (C, 4), (E, 4), (G, 4), (G, 4), (E, 4)]
riverAbs :: [AbsPitch]
riverAbs = [60,64,60,62,64,67,64,60,60,60,64,67,67,64]
riverRhytm :: [Int]
riverRhytm = [1,1,1,1,1,1,1,1,1,1,1,1,1,0]

-- My Way

way :: [Pitch]
way = [(G, 4), (E, 4), (E, 4), (G, 4), (E, 4), (D, 4), (E, 4), (E, 4), (G, 4), (E, 4), (D, 4), (E, 4), (E, 4), (G, 4)]
wayAbs :: [AbsPitch]
wayAbs = [67,64,64,67,64,62,64,64,67,64,62,64,64,67]
wayRhytm :: [Int]
wayRhytm = [0,0,1,1,1,1,1,1,1,1,1,1,1,1]

-- Misty
misty :: [Pitch]
misty = [(B, 4), (C, 4), (G, 4), (C, 4), (C, 4), (C, 4), (C, 4), (C, 5), (D, 5), (C, 5), (C, 5), (B, 5), (C, 5), (B, 5)]
mistyAbs :: [AbsPitch]
mistyAbs = [71,60,67,60,60,60,60,72,74,72,72,83,72,83]
mistyRhytm :: [Int]
mistyRhytm = [1,1,1,1,0,1,1,1,1,1,1,1,1,1]

hit :: [Pitch]
hit = [(G, 4), (E, 4), (C, 4), (G, 4), (E, 4), (C, 4), (G, 4), (E, 4), (C, 4), (G, 4), (E, 4), (C, 4), (G, 4), (E, 4)]
hitAbs :: [AbsPitch]
hitAbs = [67,64,60,67,64,60,67,64,60,67,64,60,67,64]
hitRhytm :: [Int]
hitRhytm = [0,1,0,1,0,1,0,1,0,1,0,1,0,1]



