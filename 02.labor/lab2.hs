import Text.Parsec.Token (GenLanguageDef(reservedNames))
-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat x
  | x < 0 = error "negativ szam"
  |otherwise = mod x 10 * szjSzorzat (div x 10)

szjSzorzat1 x
  | x < 10 = x
  | x < 0 = error "negativ szam"
  |otherwise = mod x 10 * szjSzorzat1 (div x 10)


ls1 = [324, 56, 98, 72, 0]
szjSzorzat1Ls ls = map szjSzorzat1 ls

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg 0 = 0
szjOsszeg x = mod x 10 + szjOsszeg (div x 10)

szjOsszeg1 x
  | x == 0 = 0 
  | x < 0 = error "negativ szam"
  |otherwise = mod x 10 + szjOsszeg1 (div x 10)


-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam 0 = 0
szjSzam x = 1 + szjSzam (div x 10)

szjSzam1 x
  | x == 0 = 0 
  | x < 0 = error "negativ szam"
  |otherwise = 1 + szjSzam1 (div x 10)

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```

fugv4 x y
  | y >= 10 =error "nem szamjegy"
  | x == 0 = 0 
  | x < 0 = error "negativ szam"
  | mod x 10 == y = mod x 10 + fugv4 (div x 10) y
  | otherwise = fugv4(div x 10) y

-- - egy szám páros számjegyeinek számát,

fugv5 x
  | x == 0 = 0 
  | x < 0 = error "negativ szam"
  | mod x 2 == 0 = 1 + fugv5 (div x 10)
  | otherwise = fugv5(div x 10)

-- - egy szám legnagyobb számjegyét,

maxSzj x res
  | x==0 = res
  | x < 0 = error "negativ szam"
  | mod x 10 > res = maxSzj (div x 10) (mod x 10)
  | otherwise = maxSzj(div x 10) res

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```



fugv6 x y z
  | x==0 = 0
  | x<0 = error "negativ szam"
  | mod x y == z = 1 + fugv6 (div x y) y z
  | otherwise = fugv6 (div x y) y z


-- - az 1000-ik Fibonacci számot.

fibo x
  | x<0 = error "negativ szam"
  | x==1 = 1
  | x==0 = 0
  | otherwise = fibo (x-1) + fibo (x-2)

fiboN n = fibo2 0 1 0 n
  where
    fibo2 _ _ res 0 = res
    fibo2 a b res n1 = fibo2 b res (b+res) (n1-1)

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```
