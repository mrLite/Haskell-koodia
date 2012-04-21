module W5 where

import System.Random
import Data.List

-- Tehtävä 1: Määrittele funktio allEqual :: Eq a => [a] -> Bool, joka
-- katsoo ovatko listan kaikki alkiot samoja.
--
-- Esimerkkejä:
-- allEqual [] ==> True
-- allEqual [1,2,3] ==> False
-- allEqual [1,1,1] ==> True

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (y:x:xs)
	| y == x = allEqual (x:xs)
	| otherwise = False

-- Tehtävä 2: Määrittele funktio secondSmallest, joka palauttaa listan
-- toisiksi pienimmän alkion (käärittynä Justiin). Jos toisiksi
-- pienintä alkiota ei ole, palauta Nothing.
--
-- Esimerkkejä:
--
-- secondSmallest [1]   ==>  Nothing
-- secondSmallest [1,1] ==>  Just 1
-- secondSmallest [5,3,7,2,3,1]  ==>  Just 2

secondSmallest :: Ord a => [a] -> Maybe a
secondSmallest [] = Nothing
secondSmallest [x] = Nothing
secondSmallest xs = Just $ head . drop 1 $ sort xs

-- Tehtävä 3: Määrittele funktio findDifference joka löytää kahden
-- listan eron. Jos listat ovat eripituisia, tuotetaan arvo
-- Just "listanpituus /= toisenlistanpituus".

-- Jos listat ovat samanpituiset, etsitään ensimmäinen indeksi jossa
-- ne eroavat. Jos tällaista ei löydy, palautetaan Nothing, muuten
-- palautetaan Just s, jossa s on merkkijono muotoa "elementti /=
-- elementti".
--
-- HUOM! Kirjoita itse findDifferencen tyyppiannotaatio. Mitä
-- tyyppiluokkia tarvitset?
--
-- Esimerkkejä: 
--  findDifference [True,False] [True,True]
--    ==> Just "False /= True"
--  findDifference [0,0,0] [0,0,0,0]
--    ==> Just "3 /= 4"

findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference xs ys = let
	lxs = length xs
	lys = length ys
	in case lxs == lys of
		False -> Just $ show lxs ++ " /= " ++ show lys
		True -> mfindIndex xs ys

mfindIndex :: (Eq a, Show a) => [a] -> [a] -> Maybe String
mfindIndex [] [] = Nothing
mfindIndex (x:xs) (y:ys)
	| x == y = mfindIndex xs ys
	| otherwise = Just $ show x ++ " /= " ++ show y

-- Tehtävä 4: Määrittele funktio average, joka laskee annettujen
-- lukujen keskiarvon. Muista että luokka Fractional on luokan Num
-- aliluokka, joten sinulla on käytössäsi kaikki laskutoimitukset.
--
-- Vihje! Saat muutettua listan pituuden tyypistä Int tyypiksi a funktiolla fromIntegral

average :: Fractional a => [a] -> a
average xs = let
	l = fromIntegral $ length xs
	s = sum xs
	in s/l

-- Tehtävä 5: Määrittele allaolevalle tyypille Foo Eq-instanssi.
-- Konstruktorien pitäisi olla yhtäsuuria vain itsensä kanssa.
--
-- Älä käytä derivingiä.

data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
	x == y = show x == show y
	-- Bar == Bar = True
	-- Quux == Quux = True
	-- Xyzzy == Xyzzy = True
	-- _ == _ = False
  
-- Tehtävä 6: Määrittele tyypille Foo sellainen Ord-instanssi että Quux < Bar < Xyzzy
--
-- Älä käytä derivingiä.
  
instance Ord Foo where
	compare x y
		| x == y = EQ		
	compare Quux _ = LT
	compare Xyzzy _ = GT
	compare Bar Quux = GT
	compare Bar Xyzzy = LT
  
-- Tehtävä 7: Tässä on 3d-vektorityyppi Vector. Määrittele sille Eq-instanssi.
--
-- Älä käytä derivingiä.

data Vector = Vector Integer Integer Integer
  deriving Show
           
instance Eq Vector where
	Vector a b c == Vector x y z = a == x && b == y && c == z

-- Tehtävä 8: Määrittele tyypille Vector Num-instanssi siten, että
-- kaikki operaatiot toimivat jokaiselle komponentille.
--
-- Vilkaise vaikka dokumentaatiosta mitkä Num-luokan metodit olivatkaan!
--
-- Esimerkkejä:
--
-- Vector 1 2 3 + Vector 0 1 1 ==> Vector 1 3 4
-- Vector 1 2 3 * Vector 0 1 2 ==> Vector 0 2 6
-- abs (Vector (-1) 2 (-3))    ==> Vector 1 2 3
-- signum (Vector (-1) 2 (-3)) ==> Vector (-1) 1 (-1)

instance Num Vector where
	Vector a b c + Vector x y z = Vector (a+x) (b+y) (c+z)
	Vector a b c * Vector x y z = Vector (a*x) (b*y) (c*z)
	negate (Vector a b c) = Vector (negate a) (negate b) (negate c)
	abs (Vector a b c) = Vector (abs a) (abs b) (abs c)
	signum (Vector a b c) = Vector (signum a) (signum b) (signum c)
	fromInteger a = Vector (fromInteger a) (fromInteger a) (fromInteger a)

-- Tehtävä 9: Määrittele funktio freqs, joka laskee kuinka monta
-- kertaa kukin alkio esiintyy listassa.
--
-- Esimerkkejä:
-- freqs [False,False,False]
--   ==> [(3,False)]

freqs :: (Eq a, Ord a) => [a] -> [(Int,a)]
-- freqs xs = undefined
freqs = map (\ys -> (length ys, head ys)) . group . sort

-- Tehtävä 10: Määrittele allaolevalle kokonaislukuja sisältävän
-- binääripuun tyypille Eq-instanssi.
--
-- Älä käytä derivingiä.

data ITree = ILeaf | INode Int ITree ITree
  deriving Show

instance Eq ITree where
	ILeaf == ILeaf = True
	INode x l1 r1 == INode y l2 r2 = x == y && l1 == l2 && r1 == r2
	_ == _ = False

-- Tehtävä 11: Tässä on edelliseltä viikolta tuttu listatyyppimme
-- List. Toteuta instanssi "Eq a => Eq (List a)" joka vertailee
-- listojen elementtejä.
--
-- Älä käytä derivingiä.

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
	Empty == Empty = True
	LNode x xs == LNode y ys = x == y && xs == ys
	_ == _ = False

-- Tehtävä 12: Määrittele funktio incrementAll, joka lisää kaikkia
-- funktorin sisällä olevia arvoja yhdellä.
--
-- Esimerkkejä:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
incrementAll = fmap (+1)

-- Tehtävä 13: Alla on määritelty tyyppi Result, joka toimii hieman
-- kuten Maybe, mutta virhetiloja on kaksi erilaista: toinen sisältää
-- virheviestin. Määrittele instanssi Functor Result.

data Result a = MkResult a | NoResult | Failure String
  deriving (Show,Eq)

instance Functor Result where
	fmap f (MkResult a) = MkResult (f a)
	fmap f NoResult = NoResult
	fmap f (Failure str) = Failure str

-- Tehtävä 14: Määrittele instanssi Functor List.

instance Functor List where
	fmap f Empty = Empty
	fmap f (LNode x xs) = LNode (f x) $ fmap f xs

-- Tehtävä 15: Tässä tyyppi Fun a, joka on yksinkertainen kääre
-- funktiolle tyyppiä Int -> a. Tehtävänäsi on kirjoittaa instanssi
-- Functor Fun.
--
-- Jos saat instanssisi menemään tyyppitarkastimesta läpi, on se hyvin
-- varmasti oikein.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
	fmap f (Fun a) = Fun (f . a)

-- Tehtävä 16: Määrittele operaattori ||| joka toimii kuten ||, mutta
-- pakottaa _oikeanpuoleisen_ argumenttinsa.
-- 
-- Esimerkkejä:
--   False ||| False     ==> False
--   True ||| False      ==> True
--   undefined ||| True  ==> True

(|||) :: Bool -> Bool -> Bool
x ||| y = case y of
	True -> y
	_ -> x

-- Tehtävä 17: Määrittele funktio boolLength joka palauttaa
-- Bool-listan pituuden ja pakottaa kaikki listan alkiot.
-- 
-- Esimerkkejä:
--   boolLength [False,True,False] ==> 3
--   boolLength [False,undefined]  ==> Virhe
-- Huom! length [False,undefined] ==> 2

boolLength :: [Bool] -> Int
boolLength xs = boolLength' xs 0

boolLength' [] l = l
boolLength' (True:xs) l = boolLength' xs (l+1)
boolLength' (False:xs) l = boolLength' xs (l+1)

-- Tehtävä 18: Tämä ja seuraava tehtävä ovat pohjustusta ensi viikon
-- materiaaliin.
--
-- Modulissa System.Random on määritelty tyyppiluokka RandomGen joka
-- esittää satunnaislukugeneraattorin operaatioita. Luokka Random taas
-- on niitä tyyppejä varten, joitten arvoja osaamme generoida
-- RandomGenin avulla.
-- 
-- Oleellista tästä modulista on funktio
--   random :: (Random a, RandomGen g) => g -> (a, g)
-- joka ottaa satunnaisgeneraattorin, ja palauttaa satunnaisen arvon
-- ja generaattorin uuden tilan (muista, puhtaus!)
--
-- Toteuta funktio threeRandom, joka generoi kolmikon satunnaisia
-- arvoja käyttäen funktiota random.
--
-- Huomio! Älä käytä samaa generaattoria uudestaan, muuten saat kolme
-- samaa lukua!
--
-- Huomio! Generaattorin lopullista tilaa ei tarvitse palauttaa (kuten
-- tyypistäkin näet).
--
-- Esimerkkejä:
--  *W5> threeRandom (mkStdGen 1) :: (Int,Int,Int)
--  (7917908265643496962,-1017158127812413512,-1196564839808993555)
--  *W5> threeRandom (mkStdGen 2) :: (Bool,Bool,Bool)
--  (True,True,False)

threeRandom :: (Random a, RandomGen g) => g -> (a,a,a)
threeRandom g = let
	(f, g2) = random g
	(s, g3) = random g2
	(t, g4) = random g3
	in (f,s,t)

-- Tehtävä 19: Toteuta funktio randomizeTree joka ottaa puun ja
-- palauttaa samanmuotoisen puun jossa jokaisessa Nodessa on
-- satunnainen arvo. Tuota arvot jälleen funktiolla random.
--
-- Tällä kertaa sinun tulee palauttaa generaattorin lopullinen tila
-- että randomizeTreen kutsuja voi jatkaa sen käyttämistä.
--
-- Vihje: rekursiivinen ratkaisu on suoraviivainen, mutta muista olla
-- käyttämättä samaa generaattoria kahdesti!
--
-- Esimerkkejä:
--  *W5> randomizeTree (Node 0 (Node 0 Leaf Leaf) Leaf) (mkStdGen 1)  :: (Tree Char, StdGen)
--  (Node '\603808' (Node '\629073' Leaf Leaf) Leaf,1054756829 1655838864)
--  *W5> randomizeTree (Node True Leaf Leaf) (mkStdGen 2)  :: (Tree Int, StdGen)
--  (Node (-2493721835987381530) Leaf Leaf,1891679732 2103410263)


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

randomizeTree :: (Random a, RandomGen g) => Tree b -> g -> (Tree a,g)
randomizeTree Leaf g = (Leaf, g)
randomizeTree (Node a left right) g = let
	(root, g2) = random g
	(leftr, g3) = randomizeTree left g2
	(rightr, g4) = randomizeTree right g3
	in ((Node root leftr rightr), g4)
