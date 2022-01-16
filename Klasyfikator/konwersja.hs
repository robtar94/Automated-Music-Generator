import Euterpea

-- ludowa

takt1 = line [a 4 qn, rest qn, a 4 en, a 4 en, gs 4 en,  e 4 en]
takt2 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, b 4 sn, gs 4 sn]
takt3 = line [a 4 sn, a 4 sn, c 4 qn, d 4 qn, e 4 hn]
takt4 = line [e 4 sn, e 4 sn, e 4 sn, e 4 sn,  d 4 sn, c 4 sn]
takt5 = line [b 4 sn,  d 4 sn,  d 4 sn, d 4 sn, d 4 sn, c 4 sn]
takt6 = line [a 4 sn, b 4 sn, c 4 qn, rest qn, c 4 qn, rest qn, a 4 hn]

-- jazz

jazz = line [a 4 qn, f 4 qn, g 4 qn, f 4 en, a 4 qn, f 5 qn, a 5 qn, f 5 qn, g 5 qn, a 5 qn, f 5 qn, g 5 qn, f 5 qn]


v :: [AbsPitch]
v = [69,69,69,68,64,69,69,69,71,68,69,69]
u :: [AbsPitch] 
u = [69,65,67,65,69,65,69,65,67,71,65,67]
x :: [AbsPitch]
x = [60,60,60,60,60,60,62,63,60,62,64,64]





toAbsPitches :: [Pitch] -> [AbsPitch]
toAbsPitches ps = map absPitch ps
    
    
toPitches :: [AbsPitch] -> [Pitch]
toPitches as = map pitch as


