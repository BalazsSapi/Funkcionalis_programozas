import System.Posix.Internals (lstat)
import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Data.List (maximumBy, sort)
import Data.Ord (comparing)

-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
parosNegyzet n = [x ** 2 | x <- [2, 4 .. n*2]]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,
fel2 n = take n (ls 1)
    where
        ls i = replicate i i : ls (i+1)

fel22 1 = [1]
fel22 n = fel22 (n-1) ++ replicate n n

-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,
fel3 1 = [2]
fel3 n = fel3 (n-1) ++ replicate n (n * 2)

-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,
fel4 n = [n, n-1 .. 1] ++ [1 .. n]

-- - váltakozva tartalmazzon True és False értékeket,
fel5 n = take n ls
    where
        ls = [True, False] ++ ls

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.
fel6 n = take n ls
    where
        ls = [0, 1, -1] ++ ls


-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 1 [1 ..  div n 2]

osztok2 n = length [x | x <- [1..n], mod n x == 0]

-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOsztok n = maximum [i | i <- [1 .. n], mod n i == 0, odd i]

maxParatlanOsztok2 n = last [i | i <- [1 .. n], mod n i == 0, odd i]

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
decP x p
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

decPSzam x p = myLength(decP x p)
    where
        myLength [] = 0
        myLength (_:ls) = 1 + myLength ls

decPSzam2 x p = foldl (\res i -> res +1) 0 (decP x p)

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
decPMax x p = maximum (decP x p)

-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.
fibo a b = filter (\x -> x > a && x < b)(fibo2 0 1 0)
    where
        fibo2 a1 b1 res
            | res < b = res : fibo2 b1 res (res + b1)
            | otherwise = [res]

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
atlag ls = (sum ls) / fromIntegral (length ls)

pozitivAtlag ls = atlag [x | x <- ls, x > 0]

pozitivAtlag2 ls = (atlag . filter (> 0)) ls

pozitivAtlag3 ls = (sum ls1) / fromIntegral (length ls1)
    where
        ls1 = filter (>0) ls


-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
listaN ls n = [i | (idx, i) <- zip [1..] ls, mod i n == 0]

listaN2 ls n i
    | i-1>= length ls = []
    | otherwise = ls !! (i-1) : listaN2 ls n (i+n)


-- - tükrözi egy lista elemeit,
tukroz ls = reverse ls

-- - tükrözi egy lista elemeit, egyesével
tukrozEgyesevel ls = map (reverse . show) ls

-- - tükrözi egy lista elemeit, egyesével és legyen int a végén1
tukrozEgyesevel2 ls = map (\x -> read x :: Int) $ map (reverse . show) ls

-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
maxElemPoz ls = [idx | (idx,i) <- zip [0..] ls, i == myMax]
    where
        myMax = maximum ls

maxElemPoz2 (x :ls) = foldl aux (x, [0]) (zip ls [1..])
    where
        aux (currentMax, positions) (elem, i)
            | elem > currentMax = (elem, [i])
            | elem == currentMax = (elem, i:positions)
            | otherwise = (currentMax, positions)

-- - meghatározza egy lista leggyakrabban előforduló elemét.
--elof ls = maxElofElem
    --where
       -- maxElofSzam = maximum $ map length $ (group . sort) ls
       -- ls2 = map (\x -> (head x, lenght x)) $ (group . sort) ls
       -- maxElofElem = filter (\x -> snd x == maxElofSzam) ls2


leggyakoribb2[] = error "ures lista"
leggyakoribb2 ls = head $ maximumBy (comparing length) group $ sort ls