

main :: IO ()
main = putStrLn $ create_svg $ translate 100 -200 $ generate_polygon 100 8

data Point = Point Double Double

x :: Point -> Double
x (Point x _) = round' 2 x

y :: Point -> Double
y (Point _ y) = round' 2 y


infixl 6 |+|

Point a b |+| Point c d = Point (a + c) (a + d)

translate :: Double -> Double -> [Point] -> [Point]
translate x y = map (|+| (Point x y))

generate_polygon = generate_polygon' 0


generate_polygon' :: Double -> Double -> Int -> [Point]
generate_polygon' s r n -- start angle, radius, number of points
    | n > 0 = map (\a -> Point (r * cos a) (r * sin a)) [angle i | i <- [0..n-1]]
    | True  = []
        where angle i = pi * (fromIntegral i) * 2 / (fromIntegral n) + s


blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

generate_star :: Double -> Double -> Int -> [Point]
generate_star r1 r2 n -- inner radius, outer radius, number of points
    = blend (generate_polygon r2 n) (generate_polygon' s r1 n)
    where s = pi / (fromIntegral n)


create_svg :: [Point] -> String
create_svg ps = "<path d=" ++ (show $ make_path_string ps) ++ " fill=\"black\"/>"

make_path_string :: [Point] -> String
make_path_string (p:ps) = "M" ++ (show $ x p) ++ " " ++ (show $ y p) ++ (make_path_string' ps)
make_path_string _ = ""

make_path_string' :: [Point] -> String
make_path_string' (p:ps) = " L" ++ (show $ x p) ++ " " ++ (show $ y p) ++ (make_path_string' ps)
make_path_string' _ = ""

round' n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
