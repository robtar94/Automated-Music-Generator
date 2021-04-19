-- http://oskarkolberg.pl/en-US;q=0.7/MusicDb/Details/cafbb535-ed69-4b99-9002-f65fe40ed8fe
-- "Od Pilicy i Wolbroma"
import Euterpea
takt1 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, gs 4 sn,  e 4 sn]
takt2 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, b 4 sn, gs 4 sn]
takt3 = line [a 4 sn, a 4 sn, c 4 qn, d 4 qn, e 4 hn]
takt4 = line [e 4 sn, e 4 sn, e 4 sn, e 4 sn,  d 4 sn, c 4 sn]
takt5 = line [b 4 sn,  d 4 sn,  d 4 sn, d 4 sn, d 4 sn, c 4 sn]
takt6 = line [a 4 sn, b 4 sn, c 4 qn, rest qn, c 4 qn, rest qn, a 4 hn]

test = takt1 :+: takt2 :+: takt3 :+: takt4 :+: takt5 :+: takt6

-- konwersja na AbsolutePitch:

linia1 = [69,69,69,68,64]
lina2 = [69,69,69,71,68]
linia3 = [69, 69, 60,62, 64]
linia4 = [64, 64,64,64,62,60]
linia5 = [71,62,62,62,62,60]
linia6 = [69,71,60,60,69]


--klasyfikator:

-- definicje funkcji klasyfikujacych:
g1 :: [Int] -> [Int] -> [Int]
g1 xs xs =
    return  xs[0] + xs[1]