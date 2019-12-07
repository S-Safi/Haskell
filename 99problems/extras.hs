factorial :: Integer -> Integer
factorial a = foldr (*) 1 [1..a]

isEven :: Integer -> Bool
isEven x = mod x 2 == 0

filterEven :: [Integer] -> [Integer]
-- filterEven xs = filter isEven xs
filterEven = filter isEven

addEven :: [Integer] -> Integer
addEven xs = foldr (+) 0 (filterEven xs)

greet :: String -> String -> String
greet greeting name = greeting ++ ", " ++ name

greetEnglish :: String -> String
greetEnglish = greet "Hello"

greetSpanish :: String -> String
greetSpanish = greet "Hola"
