{- This looks like a lot of code. But
 - 1. it has a parser for command line arguments
 - 2. it does not use any third-party hackage packages
 - 3. it only has 3(!) imports from Base Library
 - 4. it is written in a manner so that it's easily extensible.
 -
 - Display help with -h flag!
 -
 - GHC version used: 8.6.5
 -}

import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Applicative ((<|>))

main :: IO ()
main = getArgs >>= putStrLn . renderCommand . parseCommand

-- ############################################################################
-- Formalities
-- ############################################################################

data Vec2 a = Vec2 {x :: a, y :: a}

instance Functor Vec2 where
  fmap f v = Vec2 (f (x v)) (f (y v))

type Point = Vec2 Float

(|+|) :: Point -> Point -> Point
(|+|) (Vec2 a b) (Vec2 c d) = Vec2 (a+c) (b+d)

(|*|) :: Float -> Point -> Point
(|*|) s (Vec2 a b) = Vec2 (s*a) (s*b)


-- ############################################################################
-- Model
-- ############################################################################

data Command = Help | Generator SVG

data SVG = SVG { svgFillColor :: String
               , svgPosition :: Point
               , svgScale :: Float
               , svgType :: SVGType
               , mAnimation :: Maybe Animation
               }

data SVGType = Polygon { polygonNumberEdges :: Int }
             | Star    { starNumberEdges :: Int
                       , starRatio :: Float }

data Animation = Rotation { duration :: Float }


-- ############################################################################
-- Parser
-- ############################################################################

parseColor         = valueParser "-c" "yellow"
parseTranslationX  = valueParser "-x" 0
parseTranslationY  = valueParser "-y" 0
parseScale         = valueParser "-s" 50
parseNumber        = valueParser "-n" 5
parseRatio         = valueParser "-r" 0.5
parseDuration      = valueParser "-d" 4
parseAnimationFlag = elem        "-a"
parseHelpFlag      = elem        "-h"

valueParser :: Read a => String -> a -> [String] -> a
valueParser flag defaultVal args
    | (f : val : _) <- args =
        if f == flag
            then getVal val defaultVal
            else valueParser flag defaultVal (tail args)
    | (_ : xs) <- args = valueParser flag defaultVal xs
    | otherwise = defaultVal

parseCommand :: [String] -> Command
parseCommand ss = if ss == [] || parseHelpFlag ss
    then Help
    else Generator $ parseSVG ss

parseSVG :: [String] -> SVG
parseSVG ss = SVG c p s t a
  where c = parseColor ss
        p = Vec2 (parseTranslationX ss) (parseTranslationY ss)
        s = parseScale ss
        t = parseType ss
        a = parseAnimation ss

parseType :: [String] -> SVGType
parseType ("polygon" : ss) = Polygon n
  where n = parseNumber ss
parseType ("star" : ss) = Star n r
  where n = parseNumber ss
        r = parseRatio ss
parseType _ = error "You need to specify either 'polygon' or 'star'."

parseAnimation :: [String] -> Maybe Animation
parseAnimation ss = if parseAnimationFlag ss
    then Just $ Rotation $ parseDuration ss
    else Nothing

-- ############################################################################
-- Generator
-- ############################################################################

generateSVG :: SVG -> [Point]
generateSVG (SVG _ p s t _)  = (translate p) $ (scale s) $ generateType t

generateType :: SVGType -> [Point]
generateType (Polygon n) = generatePolygon n
generateType (Star n r) = generateStar n r

generateStar :: Int -> Float -> [Point]
generateStar n r = blend (generatePolygon n) (scale r $ generatePolygon' angle n)
    where angle = pi / (fromIntegral n)

generatePolygon' :: Float -> Int -> [Point]
generatePolygon' s n
    | n > 1 = map f [angle i | i <- [0..n-1]]
    | True  = []
      where angle i = pi * (fromIntegral i) * 2 / (fromIntegral n) + s
            f a     = Vec2 (cos a) (sin a)

generatePolygon = generatePolygon' 0

-- ############################################################################
-- Transformation
-- ############################################################################

translate :: Point -> [Point] -> [Point]
translate t = fmap $ (|+|) t

scale :: Float -> [Point] -> [Point]
scale s = fmap $ (|*|) s

-- ############################################################################
-- Rendering
-- ############################################################################

renderCommand :: Command -> String
renderCommand Help = "\nSynopsis:\n\
\svg_generator [-h]\n\
\svg_generator polygon [-c COLOR] [-x X_TRANSLATION] [-y Y_TRANSLATION] [-s SCALE] [-n NUMBER_EDGES] [-a [-d DURATION]]\n\
\svg_generator star [-c COLOR] [-x X_TRANSLATION] [-y Y_TRANSLATION] [-s SCALE] [-n NUMBER_EDGES] [-r RATIO] [-a [-d DURATION]]\n\n\
\- h-flag or no subcommand shows this help\n\
\- COLOR: String, default yellow, fill color\n\
\- X_TRANSLATION: Float, default 0, translation in x direction\n\
\- Y_TRANSLATION: Float, default 0, translation in y direction\n\
\- SCALE: Float, default 50, factor the figure is scaled by; scale of polygon and star is equivalent to the radius of the unit circle the figure is constructed on\n\
\- NUMBER_EDGES: Int, default 5, number of edges of a polygon or number of pronks of a star\n\
\- RATIO: Float, default 0.5, ratio between inner and outer radius of star\n\
\- DURATION: Float, default 4, only applies when -a flag is specified; duration of one turn\n\
\- a-flag creates Animation\n"


renderCommand (Generator s) = renderSVG s

renderSVG :: SVG -> String
renderSVG s@(SVG c p _ _ a) = 
    "\n<path d=" ++ (show $ renderPoints roundedPoints)
    ++ " fill=" ++ (show c) ++ ">" ++ renderAnimation p a
    ++ "</path>\n"
    where roundedPoints = fmap roundPoint points
          points = generateSVG s

renderAnimation :: Point -> Maybe Animation -> String
renderAnimation p Nothing = ""
renderAnimation p (Just (Rotation d)) = 
    "<animateTransform attributeName=\"transform\" type=\"rotate\" "
    ++ "from=\"0 " ++ show (x p) ++ " " ++ show (y p) ++ "\" "
    ++ "to=\"360 " ++ show (x p) ++ " " ++ show (y p) ++ "\" "
    ++ "repeatCount=\"indefinite\" dur=\""
    ++ show (round' 2 d) ++ "s\"/>"

renderPoints :: [Point] -> String
renderPoints (p:ps) = "M" ++ (show $ x p) ++ " " ++ (show $ y p)
                      ++ (renderPoints' ps) ++ " Z"
renderPoints _ = ""

renderPoints' :: [Point] -> String
renderPoints' (p:ps) = " L" ++ (show $ x p) ++ " " ++ (show $ y p)
                       ++ (renderPoints' ps)
renderPoints' _ = ""

-- ############################################################################
-- Utils
-- ############################################################################

round' :: Int -> Float -> Float
round' n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

roundPoint :: Point -> Point
roundPoint = fmap $ round' 2

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

getVal :: Read a => String -> a -> a
getVal s defaultVal =
    let maybeVal = (readMaybe s) <|> (readMaybe $ show s) in
    case maybeVal of
        Nothing  -> defaultVal
        Just val -> val

