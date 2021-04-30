-- od lwowa do tarnaowa http://oskarkolberg.pl/en-US;q=0.7/MusicDb/Details/e366af87-f8bf-4f0f-8d62-6fb8c9674781
-- C 6 == 84, A 5 == 81 B 5 == 83
import Euterpea
lwow = line [a 5 en, a 5 en, b 5 en, c 6 en, b 5 en,  c 6 en, a 5 en, a 5 en, b 5 en, c 6 en, b 5 en, c 6 en, e 6 en, d 6 qn, f 6 qn, d 6 qn, e 6 hn]

-- jazz (https://musescore.com/user/27009977/scores/5429135)
jazz = line [a 5 en, f 5 en, g 5 en, f 5 en, a 5 qn, f 5 qn, a 5 qn, f 5 qn, g 5 qn, a 5 qn, f 5 qn, g 5 qn, f 5 qn]


-- konwersja na Absolute Pitch


lwow2 = (81, 81, 83, 84, 83, 84, 81, 81, 83, 84, 83, 84, 84)
jazz2 = (81, 77, 79, 77, 81, 77, 81, 77, 79, 81, 77, 89, 77)


-- listy

list1 = [81, 81, 83, 84, 83, 84, 81, 81, 83, 84, 83, 84, 84]
list2 = [81, 77, 79, 77, 81, 77, 81, 77, 79, 81, 77, 89, 77]

dot = zipWith(*) list1 list2 


