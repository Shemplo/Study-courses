module Civil where

import Data.List.NonEmpty (NonEmpty(..), toList, (<|))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
           deriving (Eq, Show, Enum, Bounded)

nextDay :: Day -> Day
nextDay day | day == maxBound = minBound 
            | otherwise = succ day 


afterDays :: Int -> Day -> Day
afterDays n day | n `mod` 7 == 0 = day
                | otherwise = afterDays (n - 1) (nextDay day) 

isWeekend :: Day -> Bool
isWeekend day = elem day [Sat, Sun]


daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty day = 1 + (daysToParty $ nextDay day)




data City = City {
    cityCastle :: Maybe Castle,
    cityHall   :: Maybe Hall,
    cityHouses :: NonEmpty House
} deriving (Show)

data Castle = Castle {
    castleWalls :: Maybe Walls,
    castleLord  :: Maybe Lord
} deriving (Show)

data Hall = Church | Library
            deriving (Show)

data House = One | Two | Three | Four
             deriving (Show, Enum)

data Walls = Walls 
             deriving (Show)

data Lord = Lord 
            deriving (Show)

buildCastle :: City -> (Bool, City)
buildCastle city@City {cityCastle = Nothing} = (True, city{cityCastle = castle})
                                                where castle = Just $ Castle Nothing Nothing
buildCastle city = (False, city)


buildHall :: City -> Hall -> (Bool, City)
buildHall city@City {cityHall = Nothing} hall = (True, city {cityHall = Just hall})
buildHall city _ = (False, city)


buildChurch :: City -> (Bool, City)
buildChurch city = buildHall city Church


buildLibrary :: City -> (Bool, City)
buildLibrary city = buildHall city Library


buildHouse :: City -> Int -> City
buildHouse city@City {cityHouses = hs} members 
           | elem members [1 .. 4] = city {cityHouses = (toEnum (members - 1)) <| hs}
           | otherwise = city

castleHasLord :: Castle -> Bool
castleHasLord Castle {castleLord = Nothing} = False
castleHasLord _                             = True

castleHasWalls :: Castle -> Bool
castleHasWalls Castle {castleWalls = Nothing} = False
castleHasWalls _                              = True

inviteLord :: City -> Lord -> City
inviteLord city@City {cityCastle = (Just castle)} lord
           | castleHasLord castle = error "Another lord is living in the castle"
           | otherwise      = city {cityCastle = Just (castle {castleLord = Just lord})}
inviteLord City {cityCastle = Nothing} _ = error "No castle for lord in city"

cityPopulation :: City -> Int
cityPopulation City {cityHouses = chouses} = sum $ map (+1) 
                                                 $ map fromEnum 
                                                 $ Data.List.NonEmpty.toList chouses

buildWalls :: City -> City
buildWalls city@City {cityCastle = (Just castle)}
           | not (castleHasLord castle)   = error "City doesn't has a lord"
           | (cityPopulation city) < 10   = error "City is too small"
           | castleHasWalls castle        = error "There are walls in the city already"
           | otherwise = city {cityCastle = Just (castle {castleWalls = Just Walls})}
buildWalls City {cityCastle = Nothing} = error "No castle in the city"