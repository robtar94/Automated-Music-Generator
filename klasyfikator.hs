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


