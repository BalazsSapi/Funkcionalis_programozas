import Data.Char
import Data.List
import System.Posix.Internals (lstat)

-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
-- Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
-- ("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
-- ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”
-- valogat varosok n = sort [nev | (nev, ertek) <- varosok, ertek > n]

-- valogat2 varosok n = sort $ map fst $ filter (\(nev, ertek) -> ertek > n) varosok

fel1 = do
  let ls =
        [ ("sepsiszentgyorgy", 54000),
          ("kolozsvar", 330000),
          ("marosvasarhely", 130000),
          ("temesvar", 310000),
          ("arad", 160000),
          ("gyergyoszentmiklos", 18000),
          ("nagyvarad", 196000)
        ]
      n = 150000
      ls1 = filter (\(_, nepesseg) -> nepesseg > n) ls
      ls2 = sort (map(fst) ls1)
  if ls2 == []
    then
        putStrLn ("Nincs " ++ show n ++ " nepessegnel nagyobbal rendelkezo varos")
        else do
            mapM_ putStrLn ls2
  --print ls1

    

--       nepesseg = map snd varosok
--       varosNevek = valogat varosok n
--   if null varosNevek
--     then putStrLn ("Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos.")
--     else do
--       mapM_ (\(v, n) -> putStrLn (v ++ " " ++ show n ++ ", ")) varosok
--       putStrLn "A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
--       mapM_ (\v -> putStrLn ("- " ++ v)) varosNevek
--       putStrLn $ intercalate ", " varosNevek
--       putStrLn $ unwords varosNevek
--       putStrLn $ intercalate ", " $ map show nepesseg

-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

nincsNulla n
    | mod n 10 == 0 = False
    | n < 10 = True
    | otherwise = nincsNulla (div n 10)

fel2 = do 
    let ls =  [17603, 4005, 32203, 8106252, 70, 235061, 9018007, 807, 601, 300]
        ls1 = filter nincsNulla ls
    if ls1 == []
        then 
            putStrLn ("Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.")
    else do
        putStrLn ("A 0 szamjegyet nem tartalmazo szamok a kovetkezok: " ++ unwords (map show ls1))

-- nincsNulla n = notElem '0' (show n)

-- nincsNulla2 n
--   | n < 10 && n == 0 = False
--   | n < 10 && n /= 0 = True
--   | mod n 10 == 0 = False
--   | otherwise = nincsNulla2 (div n 10)

-- szamjegyek 0 = []
-- szamjegyek x = szamjegyek (div x 10) ++ [mod x 10]

-- nemTartNulla ls = [i | i <- ls, notElem 0 (szamjegyek i)]

-- nemTartNulla2 ls = [i | i <- ls, any (/= 0) (szamjegyek i)]

-- printNemTartNulla = do
--   let szamok = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
--       nemNullaSzamok = nemTartNulla szamok
--   if null nemNullaSzamok
--     then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
--     else do
--       putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
--       mapM_ (\elem -> putStr (show elem ++ " ")) nemNullaSzamok

-- fel2 = do
--   let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
--       ls1 = filter nincsNulla ls
--       ls2 = filter (\n -> '0' `notElem` show n) ls
--       ls3 = filter nincsNulla2 ls
--   if null ls3
--     then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
--     else do
--       putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok:"
--       mapM_ (\n -> putStr (show n ++ " ")) ls1

-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.
-- Például:
-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
-- class
-- float
-- function
-- higher-order
-- monad
-- ● Amennyiben nincsenek ilyen karakterláncok, a kimenet a következő:
-- -- “Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.”

fel3 = do
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
        ls2 = filter (\i -> not (any isDigit i)) ls
    if ls2 == []
        then 
            putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
    else do
        putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
        mapM_ putStrLn (sort ls2)
-- nincsSzam n = not (any isDigit n)

-- nincsSzam2 n = all isAlpha n

-- nincsSzam3 n = if filter isDigit n == "" then True else False

-- karlancokNotNull ls = filter (\n -> not (any isDigit n)) ls

-- nemSzamjegy ls = [i | i <- ls, not (any isDigit i)]

-- fel3 = do
--   let ls = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
--       ls1 = filter nincsSzam ls
--       ls2 = sort [szo | szo <- ls, not (any isDigit szo)]
--       ls3 = sort . filter (\szo -> not (any isDigit szo)) $ ls
--   if null ls3
--     then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
--     else do
--       putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
--       mapM_ putStrLn ls3

-- -- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- -- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- -- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- -- Például:
-- -- ● Bemenet:
-- -- s = feri
-- -- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- -- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata
-- ketoldaliSzomszedok s lsS = aux (sort lsS)
--   where
--     aux (x : y : z : ve)
--       | x == s = [y]
--       | y == s = [x, z]
--       | z == s = [y]
--       | otherwise = aux ve

-- ketoldaliSzomszedok2 s lsS
--   | keresettIndex == 0 = [lsS !! (keresettIndex + 1)]
--   | keresettIndex == length lsS - 1 = [lsS !! (keresettIndex - 1)]
--   | otherwise = [lsS !! (keresettIndex - 1), lsS !! (keresettIndex + 1)]
--   where
--     keresettIndex = aux (zip [0 ..] lsS)
--     aux ((idx, nev) : ve)
--       | nev == s = idx
--       | otherwise = aux ve

-- szomszedok n ls =
--   putStrLn
--     ( n
--         ++ " baloldali szomszedja "
--         ++ snd baloldali
--         ++ ", jobboldali szomszedja pedig "
--         ++ snd jobboldali
--     )
--   where
--     indexelt = zip [0 ..] (sort ls)
--     talalat = foldr (\(index, nev) acc -> if nev == n then (index, nev) else acc) (0, "") indexelt
--     baloldali = indexelt !! (fst talalat - 1)
--     jobboldali = indexelt !! (fst talalat + 1)

-- findNeighbors :: String -> [String] -> IO ()
-- findNeighbors s xs = case dropWhile (/= s) (sort xs) of
--   (_ : r : _) -> putStrLn $ s ++ " baloldali szomszedja " ++ last (takeWhile (/= s) (sort xs)) ++ ", jobboldali szomszedja pedig " ++ r
--   _ -> putStrLn "Nincs ket szomszed."

-- jobbOldaliSzomszed nev nevek = case dropWhile (/= nev) (sort nevek) of
--   (elem : k : ve) -> k
--   _ -> "nincs"

-- balOldaliSzomszed nev nevek = case reverse (takeWhile (/= nev) (sort nevek)) of
--   (k : ve) -> k
--   _ -> "nincs"

-- szomszed nev nevek
--   | notElem nev nevek = putStrLn "nincs benne a listaban"
--   | otherwise = putStrLn $ nev ++ " baloldali szomszedja " ++ bSz ++ ", jobboldali szomszedja pedig " ++ jSz
--   where
--     bSz = balOldaliSzomszed nev nevek
--     jSz = jobbOldaliSzomszed nev nevek

-- fel4 = do
--   let s = "feri"
--       lsS = ["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
--       szomszedok = ketoldaliSzomszedok s lsS
--       szomszedok2 = ketoldaliSzomszedok2 s (sort lsS)
--       sz1 = head szomszedok
--       sz2 = last szomszedok
--   if sz1 == sz2
--     then putStrLn (s ++ " 1 szomszedja van, " ++ sz1)
--     else putStrLn (s ++ " baloldali szomszedja " ++ sz1 ++ ", jobboldali szomszedja " ++ sz2)

-- -- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- -- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- -- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- -- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- -- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- -- értéket.
-- -- Például:
-- -- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- -- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- -- eladasi erteke a kovetkezok:
-- -- - iphone3
-- -- - huaweiS1
-- -- - samsungA1
-- thrd (_, _, x) = x

-- sndThrd (_, x, _) = x

-- telefonok ls = sort [nev | (nev, eladEr, ar) <- ls, eladEr == maxEladEr]
--   where
--     maxEladEr = maximum (map (\(_, ee, _) -> ee) ls)
--     maxEladEr2 = maximum (map sndThrd ls)

-- printTelefonok ls = mapM_ (\t -> putStrLn ("- " ++ t)) ls

-- telefonokLs = [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25, 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200), ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]

-- filterMaxEladas ls = filter (\(telefon, eladas, ar) -> eladas == maxEladas) ls -- kivesszuk filterrel azokat az elemeket, amiknek az eladasi erteke maximalis
--   where
--     eladasok = map (\(_, eladas, _) -> eladas) ls -- kivesszuk csak az eladasokat a bemeneti listabol
--     maxEladas = maximum eladasok -- a kapott eladasokbol kivesszuk a maximalisat

-- tuple2 (e1, e2, e3) = e2 -- tuple ertek masodik eleme

-- printTelefonok2 = do
--   let maxEladott = filterMaxEladas telefonokLs
--       maxEladas = tuple2 (head maxEladott)
--   putStrLn ("A maximalis eladasi ertek" ++ show maxEladas ++ "A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:")
--   mapM_ (\(nev, _, _) -> putStrLn ("- " ++ show nev)) maxEladott

-- fel5 = do
--   let ls =
--         [ ("iphoneS1", 20, 2500),
--           ("huaweiS1", 30, 1700),
--           ("huaweiS2", 25, 3100),
--           ("samsungA1", 30, 2000),
--           ("nokia", 10, 1900),
--           ("iphoneS2", 10, 2200),
--           ("samsungA2", 15, 1650),
--           ("iphone3", 30, 1800)
--         ]
--       maxElement = maximum $ map (\(_, eladErtek, _) -> eladErtek) ls
--       ls1 = filter (\(nev, eladErtek, _) -> eladErtek == maxElement) ls
--       ls2 = telefonok ls
--   putStrLn ("A maximalis eladasi ertek " ++ show maxElement ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:")
--   mapM_ (\(nev, _, _) -> putStrLn nev) ls1
--   printTelefonok ls2

-- -- 6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- -- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- -- példában szereplő formában, előfordulási érték szerint rendezve.
-- -- Például:
-- -- ● Bemenet: [7]
-- -- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- -- ● Bemenet: [1, 1, 2]
-- -- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- -- ● Bemenet: [1, 1]
-- -- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- -- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- -- ● Kimenet:
-- -- Elofordulas: 1 -> Ertek: 3
-- -- Elofordulas: 1 -> Ertek: 7
-- -- Elofordulas: 3 -> Ertek: 4
-- -- Elofordulas: 5 -> Ertek: 2
-- ls1 = [7]

-- ls2 = [1, 1, 2]

-- ls3 = [1, 1]

-- ls4 = [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]

-- elof ls = sort [(hossz, ertek) | (hossz, ertek) <- zip hosszok ertekek, odd hossz]
--   where
--     ertekek = map head (group . sort $ ls)
--     hosszok = map length (group . sort $ ls)

-- printElof = do
--   let elofSzamok = elof ls3
--   if null elofSzamok
--     then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
--     else mapM_ (\(hossz, ertek) -> putStrLn $ "Elofordulas: " ++ show hossz ++ " -> Ertek: " ++ show ertek) elofSzamok

-- paratlanElof ls = sort [(length i, head i) | i <- group (sort ls), odd (length i)]

-- elofordulas ls = filter (\(hossz, elem) -> odd hossz) zippelt -- kivesszuk azokat, ahol az elofordulasi szam, vagyis a blokkok hossza paratlan
--   where
--     csoportositott = (group . sort $ ls) -- rendezzuk a szamokat, majd csoportositjuk az azonos ertekueket egy blokkba
--     lengthek = map length csoportositott -- kivesszuk minden blokknak a hosszat
--     elemek = map head csoportositott -- kivesszuk minden blokkbol az elso elemet
--     zippelt = zip lengthek elemek -- tarsitjuk a hosszt az ertekkel

-- fel6 = do
--   let ls1 = [7]
--       ls2 = [1, 1, 2]
--       ls3 = [1, 1]
--       ls4 = [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
--       megoldas = map (\bLs -> (head bLs, length bLs)) $ filter (\bLs -> odd (length bLs)) $ group $ sort ls4
--   if null megoldas
--     then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
--     else
--       mapM_ (\(szam, elof) -> putStrLn ("Elofordulas: " ++ show elof ++ " -> Ertek: " ++ show szam)) megoldas

-- main = do
--   putStrLn "fel1"
--   fel1
--   putStrLn "\nfel2"
--   fel2
--   putStrLn "\nfel3"
--   fel3
--   putStrLn "\nfel4"
--   fel4
--   putStrLn "\nfel5"
--   fel5
--   putStrLn "\nfel6"
--   fel6