module F1 where

  import Data.Char

  -- FIBONACCI
  fibs :: [Integer]
  fibs = 0:1:zipWith (+) fibs (tail fibs)

  fib :: Integer -> Integer
  fib n = fibs !! fromInteger(n)

  -- RÖVARSPRÅKET
  isConsonant = not . (`elem` "aoeuiy")

  rovarsprak :: [Char] -> [Char]
  rovarsprak "" = ""
  rovarsprak (c:s)
    | isConsonant(c) = c:'o':c:rovarsprak s
    | otherwise      = c:rovarsprak s

  karpsravor :: [Char] -> [Char]
  karpsravor "" = ""
  karpsravor (c:s)
    | isConsonant(c) = c:(karpsravor . tail . tail) s
    | otherwise      = c:karpsravor s

  -- MEDELLÄNGD
  -- Antalet ord är alltså hälften så många som antalet brytpunkter mellan
  -- alfanumeriska och icke-alfanumeriska. Detta för att alla ord börjar och
  -- slutar vid en brytpunkt.

  xor :: [Bool] -> Bool
  xor x = and [or x, not (and x)]

  wordBoundary :: (Char, Char) -> Bool
  wordBoundary (a, b) = (xor . (map isAlpha)) [a, b]

  --wordBoundaries :: String -> [(Char, Char)]
  charTransitions s = zip (' ':s) (s ++ " ")

  numWords :: String -> Int
  numWords s = (div . length . (filter wordBoundary) . charTransitions) s 2

  medellangd :: String -> Double
  medellangd s = (fromIntegral . length . (filter isAlpha)) s / (fromIntegral . numWords) s

  -- SKYFFLA
  -- Givet en lista abcdefghijkl så ska vi skapa en ny lista genom att sätta
  -- siffror på vartannat element, och sedan börja på samma vis med de element
  -- som inte ännu har en siffra. I och med att listan halveras för varje
  -- iteration kommer transformationen ske i logaritmisk tid. Ta ett exempel:
  -- 
  --   abcdefghijklmnopqrstuvwxyz123456
  --   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
  --    2   2   2   2   2   2   2   2  
  --      3       3       3       3    
  --          4               4        
  --                  5                
  -- 
  -- Här ser vi tydligt att den första iterationen tar var annan    plus noll,
  --                        den andra  iterationen tar var fjärde   plus ett,
  --                        den tredje iterationen tar var åttonde  plus två,
  --                        den fjärde iterationen tar var sextonde plus fyra,
  --                        den femte  iterationen tar var 32:a     plus sexton,
  --                        den n:te   iterationne tar var 2^n      plus 2^(n - 1).
  -- 
  -- Vi ser även en intressant trädstruktur: ju längre listan är desto större
  -- är trädet, upp till log n i djup. Här inser vi nu att det enda elementet
  -- i femte iterationen ovan råkar ligga på platsen 2^4, och elementen i fjärde
  -- råkar ligga på b*2^3 för alla heltal b : 0 < b < n.
  --
  -- Med detta i åtanke finner vi en möjlig implementation: börja på iteration
  -- fem ovan, som hittas genom att ta taket av log_2 n. Fortsätt sedan uppåt.
  --
  -- Det visar sig dock senare vara enkelt att definiera problemet utifrån
  -- rekursion av en funktion som tar varannat element, och sedan applicerar
  -- sig själv över varannat element (plus ett).

  everyNth :: Int -> [a] -> [a]
  everyNth _ [] = []
  everyNth n s  = head s : ((everyNth n) . (drop n)) s

  everyOther :: [a] -> [a]
  everyOther = everyNth 2

  skyffla :: [a] -> [a]
  skyffla [] = []
  skyffla s  = everyOther s ++ (skyffla . everyOther . tail) s
