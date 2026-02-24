import System.Posix.Internals (lstat)
-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulonbseg :: Double -> Double -> Double
kulonbseg a b = a - b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a,b -> x = (-b) / a
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut a
  | a < 0 = -a
  | otherwise = a

abszolut2 a = if a < 0 then -a else a

-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
  | n < 0 = "negativ"
  | n > 0 = "pozitiv"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max_ a b = if a > b then a else b

max1 a b
  | a > b = a
  | otherwise = b

-- - két argumentuma közül a minimumot,
min_ a b
  | a < b = a
  | otherwise = b

-- - egy másodfokú egyenlet gyökeit,
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyokN n = [sqrt i | i <- [1..n]]

-- - az első n négyzetszámot,
negyzetN n = [i*i | i <- [1..n]]

-- - az első n természetes szám köbét,
kobN n = [i^3 | i <- [1..n]]

-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzetN n = [ i | i <- [1..n], (sqrt i * sqrt i)/= i]

-- - x hatványait adott n-ig,
xHatvanyN x n = [ x^i | i <- [1..n]]

-- - egy szám páros osztóinak listáját,
osztokN n = [i | i <- [1 .. n], n `mod` i == 0, i `mod` 2 == 0]

osztokN2 n = [i | i <- [2, 4 .. n], n `mod` i == 0]

-- - n-ig a prímszámok listáját,
osztok n = [i | i <- [1..n], n `mod` i == 0]

primszam n = osztok n == [1, n]

primszamokN n = [ i | i <- [2 .. n], primszam i]

primszamokN2 n =[ i | i <- [2..n], primszamL i]
  where
    primszamL n = osztokL n == [1, n]
    osztokL n = [i | i <- [1..n], n `mod` i == 0]


-- - n-ig az összetett számok listáját,
osszetettN n = [i | i <- [0..n], not (primszam i)]

-- - n-ig a páratlan összetett számok listáját,
paratlanOsszetettN n = [i | i <- [0..n], mod i 2 == 1, not (primszam i)]

-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a,b,c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a*a + b*b == c*c]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'][0 .. 25]
--fontos ZIP

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok1 = zip [0 .. 5][5, 4 .. 0]

szamok2 n = zip [0 .. n][n, n-1 .. 0]

-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tfLs n = take n ls
  where
    ls = [True, False] ++ ls
--fontos TAKE, Végtelen lista


main :: IO()
main = do
  putStrLn "x hatvany n"
  print (xHatvanyN 5 3)