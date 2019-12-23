
main :: IO ()
main = putStrLn $ create_svg $ scale 30 $ generateStar 0.6 5

data Vec2 = Vec2 {x :: Float, y :: Float}

instance Num Vec2 where 
  (+) v1 v2 = Vec2 (x v1 + x v2) (y v1 + y v2)
  (-) v1 v2 = Vec2 (x v1 - x v2) (y v1 - y v2)
  (*) v1 v2 = Vec2 (x v1 * x v2) (y v1 * y v2)
  abs v     = Vec2 (abs $ x v) (abs $ y v)
  signum v  = Vec2 (signum $ x v) (signum $ y v)
  fromInteger i = Vec2 (fromIntegral i) (fromIntegral i)

sum' :: Vec2 -> Float
sum' v = (x v) + (y v)

type Point = Vec2

rotationMatrix f = (Vec2 (cos f) (-sin f), Vec2 (sin f) (cos f))

generatePolygon :: Int -> [Point]
generatePolygon n
    | n > 1 = map f [angle i | i <- [0..n-1]]
    | True  = []
      where angle i = pi * (fromIntegral i) * 2 / (fromIntegral n)
            f a     = Vec2 (cos a) (sin a)

generateStar :: Float -> Int -> [Point]
generateStar ratio n = blend (generatePolygon n) (scale' ratio $ rotate angle $ generatePolygon n)
    where angle = pi / (fromIntegral n)

translate :: Vec2 -> [Point] -> [Point]
translate v = map (+v)

scale :: Vec2 -> [Point] -> [Point]
scale v = map (*v)

scale' :: Float -> [Point] -> [Point]
scale' f = map (* (Vec2 f f))

rotate :: Float -> [Point] -> [Point]
rotate a ps = map (\p -> Vec2 (sum' $ v1*p) (sum' $ v2*p)) ps
    where v1 = fst r
          v2 = snd r
          r  = rotationMatrix a

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

create_svg :: [Point] -> String
create_svg ps = "<path d=" ++ (show $ make_path_string  $ map (round'') ps) ++ " fill=\"yellow\"/>"

make_path_string :: [Point] -> String
make_path_string (p:ps) = "M" ++ (show $ x p) ++ " " ++ (show $ y p) ++ (make_path_string' ps)
make_path_string _ = ""

make_path_string' :: [Point] -> String
make_path_string' (p:ps) = " L" ++ (show $ x p) ++ " " ++ (show $ y p) ++ (make_path_string' ps)
make_path_string' _ = ""

round' n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
round'' v = Vec2 (round' 2 $ x v) (round' 2 $ y v)
