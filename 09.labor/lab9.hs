import Data.List
import Data.Char (isAlpha, isUpper, isDigit, ord)

-- # 9. labor

-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.
fugv1 szoveg n
    | length szoveg <= n = szoveg
    | otherwise = fugv1 (marJo ++ ujEleje ++ ujVege) (n + (length ujEleje) + 2)
        where
            marJo = take n szoveg
            ezenDolgozunk = drop n szoveg
            ujEleje = takeWhile (\i -> not (elem i ".,!?;")) ezenDolgozunk
            ujVegeSeged = dropWhile (\i -> not (elem i ".,!?;")) ezenDolgozunk
            ujVege = if ujVegeSeged /= [] then (ujVegeSeged !! 0) : (" " ++ (dropWhile (==' ') (drop 1 ujVegeSeged))) else ""

mainI = do
    szoveg <- readFile "allomany.txt"
    let eredmeny = fugv1 szoveg 0
    putStrLn eredmeny

-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- - beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
-- - bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,
-- - átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$

binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs =
    let mid = length xs `div` 2
        pivot = xs !! mid
    in case compare x pivot of
        EQ -> True
        LT -> binarySearch x (take mid xs)
        GT -> binarySearch x (drop (mid + 1) xs)
megfeleloFormatum hosszList iban =
    not (any (\c -> not (isUpper c) && not (isDigit c)) iban) &&
    (length iban == hossz) &&
    (mod atcsoportositottHelyettesitettSzam 97 == 1)
    where
        atcsoportositott = drop 4 iban ++ take 4 iban
        helyettesitett = concatMap (\c -> if isDigit c then [c] else show (ord c - ord 'A' + 10)) atcsoportositott
        atcsoportositottHelyettesitettSzam = read helyettesitett :: Integer
        hossz = case find (\(orszag, _) -> orszag == take 2 iban) hosszList of
            Just (_, h) -> h
            Nothing -> 0



mainII = do
    szoveg <- readFile "iban.txt"
    let sorokLista = lines szoveg
    let sorokListaTrimed = map (filter (/='\r')) sorokLista
    let rendezettLista = sort sorokListaTrimed
    -- print rendezettLista
    -- print (binarySearch "HU421177301611101800000000" rendezettLista)
    ibanHosszString <- readFile "ibanLength.txt"
    let ibanHosszLista = map (filter (/='\r')) (lines ibanHosszString)
    let hosszList = map (\sor -> words sor) ibanHosszLista
    let hosszListTuple = map (\sor -> (head sor, read (last sor) :: Int)) hosszList
    --print hosszListTuple
    let helyesIbanok = filter (megfeleloFormatum hosszListTuple) rendezettLista
    writeFile "helyesIbanok.txt" (unlines helyesIbanok)


-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum.
-- Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről,
-- hogy a hét milyen napján született és mikor van a névnapja. A névnapok megállapításához használhatjuk a
-- [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.

-- ```haskell
-- data Datum = Datum {
--   nap :: Int,
--   honap:: Int,
--   ev :: Int
-- } deriving (Show)

-- data Szemely = Szemely {
--   vnev :: [Char],
--   knev :: [Char],
--   szdatum :: Datum
-- } deriving (Show)
-- ```


data Datum = Datum {
  nap :: Int,
  honap:: Int,
  ev :: Int
} deriving (Show)

data Szemely = Szemely {
  vnev :: [Char],
  knev :: [Char],
  szdatum :: Datum
} deriving (Show)

-- ??? ....
datumToDayOfWeek = \ev honap nap ->
    let k = ev `mod` 100
        j = ev `div` 100
        m = if honap <= 2 then honap + 12 else honap
        d = nap
    in (d + (13 * (m + 1)) `div` 5 + k + (k `div` 4) + (j `div` 4) - (2 * j)) `mod` 7
mainIII = do
    szoveg <- readFile "szemelyek.txt"
    let sorokLista = lines szoveg
    let sorokListaTrimed = map (filter (/='\r')) sorokLista
    let szemelyek = map (\sor -> let adatok = words sor in
            Szemely (head adatok) (adatok !! 1)
            (Datum (read (adatok !! 4) :: Int) (read (adatok !! 3) :: Int) (read (adatok !! 2) :: Int))) sorokListaTrimed
    --print szemelyek

    let milyenNapokonSzulettek = map (\sz  -> let datum = szdatum sz in
            let napSzam = datumToDayOfWeek (ev datum) (honap datum) (nap datum) in
            (vnev sz, knev sz, napSzam)) szemelyek
    print milyenNapokonSzulettek
