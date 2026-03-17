import Distribution.Simple.Utils (xargs)
import System.Posix.Internals (lstat)
import Distribution.Types.Lens (PackageDescription)
import Data.Foldable(minimumBy)
import Data.Function(on)

-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myLength2 ls = foldr(\_ db -> 1+ db) 0 ls

myLength3 xs = foldr (\x -> (+) 1) 0 xs


-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myProduct2 ls = foldr (*) 1 ls


-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [x] = x
myMinimum (x1 : x2 : xs) = if x1 < x2 then myMinimum (x1 : xs) else myMinimum (x2 :xs)

myMinimum2 ls = foldr1 min ls

myMinimum3 ls = minimum ls



-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [x] = x
myMaximum (x1 : x2 : xs) = if x1 > x2 then myMaximum (x1 : xs) else myMaximum (x2 :xs)


myMaximum3 ls = foldr1 max ls


-- - meghatározza egy lista n-ik elemét (!!),
listaN ls n = ls !! n


-- - egymásután fűzi a paraméterként megadott két listát (++),
listaFuz ls1 ls2 = ls1 ++ ls2


-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
palindrom ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom2 [] = True
palindrom2 [x] = True
palindrom2 ls = (head ls == last ls) && palindrom2 (init $ tail ls)

-- - meghatározza egy egész szám számjegyeinek listáját,
szjLs x 
    | x < 10 = [x]
    | otherwise = szjLs (div x 10) ++ [mod x 10] 

szjLs2 x
    | x < 10 = [x]
    | otherwise = (mod x 10) : szjLs2 (div x 10)

szjLs2sg x = reverse (szjLs2 x)

-- - a lista első elemét elköltözteti a lista végére,
elsoUtolso (x:xs) = xs ++ [x]

elsoUtolso2 xs = tail xs ++ [head xs]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
lsAtlag ls = osszeg / hossz
    where
        osszeg = sum ls
        hossz = fromIntegral (length ls)

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.
pDec ls p = foldl (\sg x -> sg * p + x) 0 ls

pDec2 x p = [i + (p ^ hatvany) | (i, hatvany) <- zip (szamjegyek x p) [0..]]
    where
        szamjegyek x p
            | x < 10 = [x]
            |otherwise = mod x 10 : szamjegyek (div x 10) p

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.

aLs = [3, -2, 5, -7]

x0 = 2

poli [] x0 = 0
poli (a:aLs) x0 = a + x0 * (poli aLs x0)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.

type Pont = (Double, Double)

lsP = [(2.3, 5.6), (1.2, 4.5), (6, 7)]

p = (3.6, 8.9)

tavolsag (x1,y1) (x2,y2) = sqrt ((x1-x2) ** 2 + (y1-y2) ** 2)

minPont lsP p = foldl1 aux lsP
    where
        aux p1 p2 =if tavolsag p1 p < tavolsag p2 p then p1 else p2


minPont2 lsP p = minimumBy (compare `on` tavolsag p) lsP

