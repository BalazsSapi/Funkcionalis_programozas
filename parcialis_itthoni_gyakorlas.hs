import Data.Foldable (minimumBy)
import Data.Function(on)
import Data.List (sort, group, sortBy)
faktLs n hanyadik utolso
    | n == 0 = [1]
    | n == hanyadik = [hanyadik*utolso]
    | hanyadik == 0 = [1] ++  faktLs n 1 1
    | otherwise = [hanyadik*utolso] ++ faktLs n (hanyadik+1) (hanyadik*utolso)

fiboLs n hanyadik k1 k2
    | n == 0 = [0]
    | n == 1 = [0,1]
    | n == hanyadik = [k1+k2]
    | hanyadik == 0 = [0,1] ++ fiboLs n 2 0 1
    | otherwise = [k1+k2] ++ fiboLs n (hanyadik+1) k2 (k2+k1)

--Első n természetes szám gyöke
func1 n = map (sqrt . fromIntegral) [1..n]

--Első n prímszám
primN n = take n (filter prim ([2]++[3,5..]))
    where
        prim m
            | m == 2 = True
            | mod m 2 == 0 = False
            | otherwise = foldl (\res i -> (mod m i /= 0) && res) True [3..(floor (sqrt (fromIntegral m)))]

szamjegyMax n
    | n == 0 = 0
    | otherwise = max (szamjegyMax (div n 10)) (mod n 10)

--10-es számrendszerből x számrendszerbe alakítás
szamrendszerAlakito n x
    | n == 0 = [0]
    | div n x == 0 = [mod n x]
    | otherwise = szamrendszerAlakito (div n x) x ++ [mod n x]

--Lista pont elemei közül a legközelebbi a paraméter ponthoz
closestPoint p ls = foldl1 (\res i -> if tavolsag res p > tavolsag i p then i else res) ls
    where
        tavolsag (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

closestPointMinimumBy p ls = minimumBy (compare `on` tavolsag p) ls
    where
        tavolsag (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

--első n páros szám négyzete
parosNegyzetN n = map (**2) [2,4..n*2]

--szám osztóinak száma
osztokSzama n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = foldl (\res i -> if mod n i == 0 then res + 1 else res) 2 [2..(div n 2)]

--lista minden n. eleme
mindenNElem n ls = map fst (filter (\(_,k2) -> mod k2 n == 0) (zip ls [1..]))

--lista legnagyobb elemének pozícióját egyszeri bejárással
legnagyobbElemPoz ls = snd (foldl1 (\res i -> if i > res then i else res) (zip ls [0..]))

--lista leggyakoribb eleme
leggyakoribbElem ls = (head . head . reverse . sortBy (compare `on` length)) ((group . sort) ls)

--mindenkinek az átlag jegyét kiszámolja
atlagJegySzemelyenkent ls = map (\(nev,jegyek) -> (nev, atlag jegyek)) ls
    where
        atlag ls1 = (sum ls1) / (fromIntegral $ length ls1)

main = do
    --let ls = faktLs 10 0 0
    --let ls = fiboLs 1000 0 0 0
    --let ls = func1 10
    --let ls = primN 10
    -- maxSzj = szamjegyMax 150
    --let ls = szamrendszerAlakito 18 8
    --let points = [(1.8,2.0),(1.0,1.0),(155.0,55.0)]
    --let ls = parosNegyzetN 10
    --let ls = mindenNElem 2 [15,8,5,56]
    --let ls = legnagyobbElemPoz [15,8,5,56,8]
    --let ls = leggyakoribbElem [15, 20, 4, 2, 20]
    --let ls = atlagJegySzemelyenkent [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]

    n <- readLn :: IO Int
    --let nInt = read (n) :: Int
    --m <- getLine
    print (n)



    --print (ls !! 0)
    --print (closestPointMinimumBy (2.0,2.0) points)
    --print ls
    --mapM_ (\i -> putStrLn (show i ++ " Alma")) ls2
    --mapM_ (putStrLn.show) ls
    --mapM_ (putStr.show) ls
    --putStrLn ""
    --mapM_ (\(nev, atlag) -> putStrLn (nev ++ " " ++ show atlag)) ls