{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Data.Units where
    import Control.Monad

    import Data.List (intercalate, sort, sortBy, find, findIndex, minimumBy)
    import Data.List.Utils (replace, split)
    import qualified Data.Map as Map
    import Data.Maybe (isJust, fromJust, mapMaybe)
    import Data.Ord (comparing)

    import System.IO.Unsafe

    import Library.List (remove, countDuplicates)

    -----------------------------------------------------------------------------
    -------------------------- GENERAL LIBRARY FUNCTIONS
    -----------------------------------------------------------------------------
    -- For each item that is present in both lists, removes it from both lists
    -- Returns both lists
    cancel :: Eq a => [a] -> [a] -> ([a], [a])
    cancel [] [] = ([], [])
    cancel [] ys = ([], ys)
    cancel xs [] = (xs, [])
    cancel (x:xs) ys
        | x `elem` ys = (resx, resy)
        | otherwise = (x : resx, resy)
        where nextYs = remove ys x
              (resx, resy) = cancel xs nextYs

    isNumeric :: Char -> Bool
    isNumeric c = c `elem` (['0'..'9'] ++ ",.")

    isAlpha :: Char -> Bool
    isAlpha c = c `elem` (['a'..'z'] ++ ['A'..'Z'])

    -- Removes \n, \t, \r, and ' ' from the left side (the beginning) of a string
    lTrim :: String -> String
    lTrim = dropWhile (`elem` " \n\r\t")

    -- Removes \n, \t, \r, and ' ' from the right side (the end) of a string
    rTrim :: String -> String
    rTrim = reverse . lTrim . reverse

    -- Removes \n, \t, \r, and ' ' from the left and right sides of a string
    trim :: String -> String
    trim = rTrim . lTrim

    groupBy :: Ord b => (a -> b) -> [a] -> [[a]]
    groupBy f xs = map snd $ Map.toList $ foldl (\cur v -> Map.insertWith (++) (f v) [v] cur) Map.empty xs

    -----------------------------------------------------------------------------
    -------------------------- DATA TYPE DEFINITIONS
    -----------------------------------------------------------------------------
    data Unit = Base String String String (Map.Map String (Double -> Double)) |
                Inv Unit |
                Mult Unit Unit | -- Could be N m (energy) or ms**-1 (velocity)
                -- Newtons and such, for example. The double is how many there are in that unit.s
                -- For example, an acre is 43560 square feet.
                Derived String String String Double Unit
                --deriving Show

    data Quantity = Quantity Unit Double

    -----------------------------------------------------------------------------
    -------------------------- UNIT DEFINITIONS
    -----------------------------------------------------------------------------
    noUnit = Base "" "" "1" Map.empty

    siPrefixes = [e, p, t, g, m, k, h, da, d, c, mi, mu, n, pi, f, a]
        where e = ("exa", 10**18)
              p = ("peta", 10**15)
              t = ("tera", 10**12)
              g = ("giga", 10**9)
              m = ("mega", 10**6)
              k = ("kilo", 10**3)
              h = ("hecto", 10**2)
              da = ("deca", 10**1)
              d = ("deci", 10**(-1))
              c = ("centi", 10**(-2))
              mi = ("milli", 10**(-3))
              mu = ("micro", 10**(-6))
              n = ("nano", 10**(-9))
              pi = ("pico", 10**(-12))
              f = ("femto", 10**(-15))
              a = ("atto", 10**(-18))

    addSIPrefixes :: Unit -> Unit
    addSIPrefixes (Base uType name abb conv) = Base uType name abb newConv
        where newConv = foldl (\cur (pref, mult) -> Map.insert (pref ++ name) (\v -> 1 / mult) cur) conv siPrefixes
    addSIPrefixes a@(Derived uType name abb val base) = Derived uType name abb val (Mult aMultFinal (Inv aDivFinal))
        where (m:ms, d) = getBaseUnitTuple a
              newM = addSIPrefixes m
              aMultFinal = case filter (/= noUnit) (newM:ms) of
                            [] -> noUnit
                            xs -> foldl1 Mult xs
              aDivFinal = case filter (/= noUnit) d of
                            [] -> noUnit
                            xs -> foldl1 Mult xs

    ampere = addSIPrefixes $ Base "current" "ampere" "A" aConversions
        where aConversions = Map.fromList []

    kelvin = addSIPrefixes $ Base "temperature" "kelvin" "K" kConversions
        where kConversions = Map.fromList []

    candela = addSIPrefixes $ Base "luminosity" "candela" "cd" cdConversions
        where cdConversions = Map.fromList []

    second = addSIPrefixes $ Base "time" "second" "s" sConversions
        where sConversions = Map.fromList [("minute", \v -> v * 1 / 60)]

    minute = addSIPrefixes $ Base "time" "minute" "min" minConversions
        where minConversions = Map.fromList [("second", \v -> v * 60), ("hour", \v -> v * 1/60)]

    hour = addSIPrefixes $ Base "time" "hour" "hr" hrConversions
        where hrConversions = Map.fromList [("minute", \v -> v * 60), ("day", \v -> v * 1/24)]

    day = Base "time" "day" "day" dayConversions
        where dayConversions = Map.fromList [("hour", \v -> v * 24), ("week", \v -> v * 1/7), ("year", \v -> v * 1/365)]

    week = Base "time" "week" "wk" wkConversions
        where wkConversions = Map.fromList [("day", \v -> v * 7)]

    year = Base "time" "year" "yr" yrConversions
        where yrConversions = Map.fromList [("day", \v -> v * 365)]

    gram = addSIPrefixes $ Base "mass" "gram" "g" gConversions
        where gConversions = Map.fromList [("pound", \v -> v * 0.0022046226)]

    pound = addSIPrefixes $ Base "mass" "pound" "lbs" lbsConversions
        where lbsConversions = Map.fromList [("gram", \v -> v * 453.59237)]

    au = addSIPrefixes $ Base "length" "au" "AU" auConversions
        where auConversions = Map.fromList [("meter", \v -> v * 149597870700)]

    meter = addSIPrefixes $ Base "length" "meter" "m" mConversions
        where mConversions = Map.fromList [("foot", \v -> v * 3.28084), ("au", \v -> v * 6.68459*10**(-12))]

    inch = Base "length" "inch" "in" inConversions
        where inConversions = Map.fromList [("foot", \v -> v * 1/12)]
    foot = Base "length" "foot" "ft" ftConversions
        where ftConversions = Map.fromList [("meter", \v -> v * 0.3048), ("yard", \v -> v * 1 / 3), ("inch", \v -> v * 12), ("chain", \v -> v * 1/66), ("link", \v -> v * 50/33)]
    yard = Base "length" "yard" "yd" ydConversions
        where ydConversions = Map.fromList [("foot", \v -> v * 3), ("fathom", \v -> v * 1/2), ("mile", \v -> v * 1 / 1760)]
    fathom = Base "length" "fathom" "ftm" ftmConversions
        where ftmConversions = Map.fromList [("yard", \v -> v * 2)]
    furlong = Base "length" "furlong" "fur" furConversions
        where furConversions = Map.fromList [("chain", \v -> v * 10)]
    chain = Base "length" "chain" "ch" chConversions
        where chConversions = Map.fromList [("foot", \v -> v * 66)]
    link = Base "length" "link" "li" liConversions
        where liConversions = Map.fromList [("foot", \v -> v * 33/50), ("rod", \v -> v * 1/25)]
    rod = Base "length" "rod" "rd" rdConversions
        where rdConversions = Map.fromList [("link", \v -> v * 25)]
    league = Base "length" "league" "lea" leaConversions
        where leaConversions = Map.fromList [("mile", \v -> v * 3)]
    nauticalMile = Base "length" "nautical mile" "NM" nmConversions
        where nmConversions = Map.fromList [("mile", \v -> v * 1.151)]
    mile = Base "length" "mile" "mi" miConversions
        where miConversions = Map.fromList [("nauticalMile", \v -> v * 1 / 1.151), ("league", \v -> v * 1 / 3), ("yard", \v -> v * 1760)]

    newton = addSIPrefixes $ Derived "force" "Newton" "N" 1 (Mult (Mult (kilo gram) meter) (Inv (Mult second second)))
    coulomb = addSIPrefixes $ Derived "charge" "coulomb" "C" 1 (Mult second ampere)
    joule = addSIPrefixes $ Derived "energy" "joule" "K" 1 (Mult newton meter)
    watt = addSIPrefixes $ Derived "power" "watt" "W" 1 (Mult joule (Inv second))
    hertz = addSIPrefixes $ Derived "frequency" "hertz" "Hz" 1 (Mult noUnit (Inv second))
    volt = addSIPrefixes $ Derived "potential" "volt" "V" 1 (Mult joule (Inv coulomb))
    farad = addSIPrefixes $ Derived "capacitance" "farad" "F" 1 (Mult coulomb (Inv volt))
    ohm = addSIPrefixes $ Derived "resistance" "ohm" "Ω" 1 (Mult volt (Inv ampere))
    tesla = addSIPrefixes $ Derived "magnetic field" "tesla" "T" 1 (Mult newton (Inv (Mult ampere meter)))

    acre = Derived "area" "acre" "ac" 1 (Mult chain furlong)

    baseUnits = [second, minute, hour, day, week, year, gram, pound, au, meter, inch, foot,
                 yard, fathom, chain, link, rod, league, nauticalMile, mile, ampere,
                 kelvin, candela]
    derivedUnits = [newton, acre, coulomb, joule, watt, hertz, volt, farad, ohm, tesla]
    allUnits = baseUnits ++ derivedUnits ++ [u | base <- baseUnits ++ derivedUnits, pref <- siPrefFunc, let u = pref base] ++ [noUnit]

    siPrefFunc :: [Unit -> Unit]
    siPrefFunc = [exa, peta, tera, giga, mega, kilo, hecto, deca, deci, centi, milli, micro, nano, pico, femto, atto]

    -- SI Prefixes are functions that convert a normal unit to its prefixed version (generates a new base unit)
    -- Also generates a conversion to the original unit
    exa = makePrefix "exa" "E" (10**18)
    peta = makePrefix "peta" "P" (10**15)
    tera = makePrefix "tera" "T" (10**12)
    giga = makePrefix "giga" "G" (10**9)
    mega = makePrefix "mega" "M" (10**6)
    kilo = makePrefix "kilo" "k" (10**3)
    hecto = makePrefix "hecto" "h" (10**2)
    deca = makePrefix "deca" "da" (10**1)
    deci = makePrefix "deci" "d" (10**(-1))
    centi = makePrefix "centi" "c" (10**(-2))
    milli = makePrefix "milli" "m" (10**(-3))
    micro = makePrefix "micro" "μ" (10**(-6))
    nano = makePrefix "nano" "n" (10**(-9))
    pico = makePrefix "pico" "p" (10**(-12))
    femto = makePrefix "femto" "f" (10**(-15))
    atto = makePrefix "atto" "a" (10**(-18))

    makePrefix :: String -> String -> Double -> Unit -> Unit
    makePrefix prefixName prefixAbb val = f
        where f (Base uType name abb conv) = Base uType (prefixName ++ name) (prefixAbb ++ abb) conversions
                where conversions = Map.insert name (\v -> v * val) $ Map.map (\f -> \v -> f v * val) conv
              f (Derived uType name abb val base) = Derived uType (prefixName ++ name) (prefixAbb ++ abb) val base

    -----------------------------------------------------------------------------
    -------------------------- UTILITY FUNCTIONS
    -----------------------------------------------------------------------------
    collectMultUnits :: Unit -> [Unit]
    collectMultUnits a@(Base _ _ _ _) = [a]
    collectMultUnits (Inv a) = collectDivUnits a
    collectMultUnits (Mult a b) = collectMultUnits a ++ collectMultUnits b
    collectMultUnits (Derived _ _ _ _ base) = collectMultUnits base

    collectDivUnits :: Unit -> [Unit]
    collectDivUnits (Base _ _ _ _) = []
    collectDivUnits a@(Inv b) = collectMultUnits b -- This should be mult, as there should never be another inv inside an inv
    collectDivUnits (Mult a b) = collectDivUnits a ++ collectDivUnits b
    collectDivUnits (Derived _ _ _ _ base) = collectDivUnits base

    getBaseUnitTuple :: Unit -> ([Unit], [Unit])
    getBaseUnitTuple a = (sort $ collectMultUnits a, sort $ collectDivUnits a)

    getUnitType :: Unit -> String
    getUnitType (Base uType _ _ _) = uType
    getUnitType (Derived uType _ _ _ _) = uType
    getUnitType a = mpart ++ dpart
            where mpart = case sort $ map getUnitType $ collectMultUnits a of
                            [] -> "1"
                            mult -> intercalate "*" $ map show mult
                  dpart = case sort $ map getUnitType $ collectDivUnits a of
                            [] -> ""
                            (d:[]) -> "/" ++ show d
                            div -> "/(" ++ intercalate "*" (map show div) ++ ")"

    -- Most units don't have a name
    getUnitName :: Unit -> String
    getUnitName (Base _ name _ _) = name
    getUnitName (Derived _ name _ _ _) = name
    getUnitName _ = ""

    getUnitByName :: String -> Maybe Unit
    getUnitByName name = find ((== name) . getUnitName) allUnits

    -- Finds the derived unit, if any. If none, then just returns the input
    getDerivedUnit :: Unit -> Unit
    getDerivedUnit a@(Base _ _ _ _) = a
    getDerivedUnit a = case find ((== aBase) . getBaseUnitTuple) derivedUnits of
                            Nothing -> a
                            Just dUnit -> dUnit
        where aBase = getBaseUnitTuple a

    -- getConversion :: Unit -> [String] -> Unit -> Maybe Double
    -- Convert units of the same type to the same unit (for example, cm*m/(s*hr) will become m^2/s^2)
    simplifyMult :: [Unit] -> (Double, [Unit])
    simplifyMult units = (product multipliers, foldl1 (++) finalUnits)
        where unitGroups = groupBy getUnitType units
              (multipliers, finalUnits) = unzip $ map getMultiplier unitGroups
              -- Get all of the conversions and multiply them together.
              -- Afterwards, all units are the preferred unit, so just replicate that.
              getMultiplier group@(x:_) = case (mapM (\unit -> getConversion preferredUnit [] unit) group) >>= (return . product) of
                                            Nothing -> (1, replicate (length group) preferredUnit)
                                            Just v -> (v, replicate (length group) preferredUnit)
                where -- Find the unit in the group that comes first in the list of units, that's the preferred unit.
                      -- This will cause base units to be favored over units with prefixes, for example, but also meters over feet.
                      preferredUnit = fst $ minimumBy (comparing (fromJust . snd)) $ filter (isJust . snd) $ map (\v -> (v, findIndex (== v) allUnits)) group

    simplifyUnitMult :: Unit -> Unit -> (Double, Unit)
    simplifyUnitMult unitA unitB = (mMult / dMult, builtUnitFromBase mUnits dUnits)
        where units = unitA * unitB
              (multUnits, divUnits) = getBaseUnitTuple units
              (mMult, mUnits) = simplifyMult multUnits
              (dMult, dUnits) = simplifyMult divUnits

    simplifyUnitDiv :: Unit -> Unit -> (Double, Unit)
    simplifyUnitDiv unitA unitB = (1 / multiplier, units)
        where (multiplier, units) = simplifyUnitMult unitA unitB

    -- Multiplies together all the units in the first list and divides them by the units in the second
    builtUnitFromBase :: [Unit] -> [Unit] -> Unit
    builtUnitFromBase mUnits dUnits = Mult aMultFinal (Inv aDivFinal)
        where aMultFinal = case filter (/= noUnit) mUnits of
                                [] -> noUnit
                                xs -> foldl1 Mult xs
              aDivFinal = case filter (/= noUnit) dUnits of
                                [] -> noUnit
                                xs -> foldl1 Mult xs

    simplifyUnits :: Unit -> Unit
    simplifyUnits a@(Base _ _ _ _) = a
    simplifyUnits a@(Inv b) = getDerivedUnit $ Inv $ simplifyUnits b
    simplifyUnits a@(Derived _ _ _ _ _) = a
    simplifyUnits a = getDerivedUnit $ builtUnitFromBase aMultRes aDivRes
        where (aMult, aDiv) = getBaseUnitTuple a
              (aMultRes, aDivRes) = cancel aMult aDiv

    getVal :: Quantity -> Double
    getVal (Quantity _ val) = val

    -----------------------------------------------------------------------------
    -------------------------- INSTANCES
    -----------------------------------------------------------------------------
    instance Show Unit where
        show (Base _ _ abb _) = abb
        show (Derived _ _ abb _ _) = abb
        show a = mpart ++ dpart
            where mpart = case sort $ countDuplicates $ collectMultUnits a of
                            [] -> "1"
                            mult -> intercalate "*" $ map showPower mult
                  dpart = case sort $ countDuplicates $ collectDivUnits a of
                            [] -> ""
                            ((n, d):[]) -> case d == noUnit of
                                            False -> "/" ++ showPower (n,d)
                                            True -> ""
                            div -> "/(" ++ intercalate "*" (map showPower div) ++ ")"
                  showPower (n, d) = case n > 1 of
                                        False -> show d
                                        True -> show d ++ "^" ++ show n

    instance Ord Unit where
        a <= b = show a <= show b

    instance Eq Unit where
        (Base uType1 name1 abb1 _) == (Base uType2 name2 abb2 _) = (uType1 == uType2) && (name1 == name2) && (abb1 == abb2)
        (Inv a) == (Inv b) = a == b
        (Derived type1 name1 abb1 val1 base1) == (Derived type2 name2 abb2 val2 base2) = (type1 == type2) && (name1 == name2) && (abb1 == abb2) && (val1 == val2) && (base1 == base2)
        (Derived _ _ _ _ base) == b = base == b
        a == (Derived _ _ _ _ base) = a == base
        a == b = (aMult == bMult) && (aDiv == bDiv)
            where (aMult, aDiv) = getBaseUnitTuple a
                  (bMult, bDiv) = getBaseUnitTuple b

    instance Num Unit where
        a + b
            | a == b = a
            | otherwise = error ("Unit mismatch! First is " ++ show a ++ ", but second is " ++ show b ++ ".")

        a - b
            | a == b = a
            | otherwise = error ("Unit mismatch! First is " ++ show a ++ ", but second is " ++ show b ++ ".")

        (Derived _ _ _ _ a) * (Derived _ _ _ _ b) = simplifyUnits $ a * b
        (Derived _ _ _ _ a) * b = simplifyUnits $ a * b
        a * (Derived  _ _ _ _ b) = simplifyUnits $ a * b
        a * b = simplifyUnits $ Mult a b

        abs = id
        signum = id
        fromInteger _ = noUnit

    instance Fractional Unit where
        a / b = simplifyUnits $ a * (Inv b)

        fromRational v = noUnit

    instance Num Quantity where
        a@(Quantity unitA valA) + (Quantity unitB valB)
            | unitA == unitB = Quantity unitA (valA + valB)
            | otherwise = case convert a unitB of
                                Nothing -> error ("Cannot convert! " ++ errMsg)
                                Just converted -> Quantity unitB (getVal converted + valB)
            where errMsg = "Value " ++ show valA ++ " has units of " ++ show unitA ++ " and value " ++ show valB ++ " has units of " ++ show unitB ++ "."

        a@(Quantity unitA valA) - (Quantity unitB valB)
            | unitA == unitB = Quantity unitA (valA + valB)
            | otherwise = case convert a unitB of
                                Nothing -> error ("Cannot convert! " ++ errMsg)
                                Just converted -> Quantity unitB (getVal converted - valB)
            where errMsg = "Value " ++ show valA ++ " has units of " ++ show unitA ++ " and value " ++ show valB ++ " has units of " ++ show unitB ++ "."

        (Quantity (Derived _ _ _ conv unitA) valA) * b = (Quantity unitA (conv * valA)) * b
        a * (Quantity (Derived _ _ _ conv unitB) valB) = a * (Quantity unitB (valB / conv))
        (Quantity unitA valA) * (Quantity unitB valB) = Quantity units (multiplier * valA * valB)
            where (multiplier, units) = simplifyUnitMult unitA unitB

        abs (Quantity unitA valA) = Quantity unitA (abs valA)
        signum (Quantity unitA valA) = Quantity unitA (signum valA)
        fromInteger v = Quantity noUnit (fromIntegral v)

    instance Fractional Quantity where
        (Quantity (Derived _ _ _ conv unitA) valA) / b = (Quantity unitA (conv * valA)) / b
        a / (Quantity (Derived _ _ _ conv unitB) valB) = a / (Quantity unitB (valB / conv))
        (Quantity unitA valA) / (Quantity unitB valB) = Quantity units (multiplier * valA / valB)
            where (multiplier, units) = simplifyUnitDiv unitA unitB

        fromRational v = Quantity noUnit (fromRational v)

    instance Show Quantity where
        show (Quantity unit val) = show val ++ " " ++ show unit

    instance Num a => Num (Maybe a) where
        (+) = liftM2 (+)
        (-) = liftM2 (-)
        (*) = liftM2 (*)

        abs = liftM abs
        signum = liftM signum
        fromInteger = Just . fromInteger

    instance Fractional a => Fractional (Maybe a) where
        (/) = liftM2 (/)
        fromRational = Just . fromRational

    -----------------------------------------------------------------------------
    -------------------------- MAIN FUNCTIONS
    -----------------------------------------------------------------------------

    canConvert :: Unit -> Unit -> Bool
    canConvert a@(Base typeA _ _ _) b@(Base typeB _ _ _) = typeA == typeB
    canConvert (Inv a) (Inv b) = canConvert a b
    canConvert a b = ((sort aMult) == (sort bMult)) && ((sort aDiv) == (sort bDiv))
        where aMult = filter (not . null) $ map getUnitType $ collectMultUnits a
              aDiv = filter (not . null) $ map getUnitType $ collectDivUnits a
              bMult = filter (not . null) $ map getUnitType $ collectMultUnits b
              bDiv = filter (not . null) $ map getUnitType $ collectDivUnits b

    -- Returns the total conversion factor for those two units if possible
    -- Otherwise returns Nothing
    -- Converts from a to b (arguments are in the other order for convenience in recursion)
    -- The second argument is the list of the names of the units that have been tried already, so it doesn't get into a loop
    getConversion :: Unit -> [String] -> Unit -> Maybe (Double -> Double)
    getConversion b@(Derived _ _ _ valB baseB) history a = case baseConv of
                                                        Nothing -> Nothing
                                                        Just conv -> Just $ conv / valB
        where baseConv = getConversion baseB history a
    getConversion b history a@(Derived _ _ _ valA baseA) = case baseConv of
                                                        Nothing -> Nothing
                                                        Just conv -> Just $ valA * conv
        where baseConv = getConversion b history baseA
    getConversion b history a@(Base _ _ _ conv) = case Map.lookup (getUnitName b) conv of
                                            -- check the other possible paths
                                            Nothing -> case find isJust $ map getNextConv $ filter (not . (`elem` history) . fst) $ Map.toList conv of
                                                        Nothing -> Nothing
                                                        Just v -> v
                                            Just v -> Just $ v 1 -- We found it
        where getNextConv (name, conv) = case (getUnitByName name) >>= (getConversion b (name : history)) of
                                            Nothing -> Nothing
                                            Just v -> Just $ conv v
    getConversion b history a = case convert (Quantity a 1) b of
                                    Nothing -> Nothing
                                    Just (Quantity _ v) -> Just v

    -- Converts a quantity in one unit to another if possible, otherwise returns Nothing
    convert :: Quantity -> Unit -> Maybe Quantity
    convert a@(Quantity unit val) b
        | not (canConvert unit b) = Nothing
        | unit == b = Just a
        | otherwise = case convFactor of
                            Nothing -> Nothing
                            Just v -> Just $ Quantity b $ fromConvFactor * toConvFactor * v * val
        where (ma, da) = getBaseUnitTuple unit
              (mb, db) = getBaseUnitTuple b
              multConvFactor = (mapM (\(from, to) -> getConversion to [] from) multMatched) >>= (Just . product)
              divConvFactor = (mapM (\(from, to) -> getConversion to [] from) divMatched) >>= (Just . product)
              fromConvFactor = case unit of
                                (Derived _ _ _ val _) -> val
                                _ -> 1
              toConvFactor = case b of
                                (Derived _ _ _ val _) -> 1 / val
                                _ -> 1
              convFactor = case multConvFactor of
                                Nothing -> Nothing
                                Just v1 -> case divConvFactor of
                                            Nothing -> Nothing
                                            Just v2 -> Just $ v1 / v2
              multMatched = zip (sortBy (comparing getUnitType) ma) (sortBy (comparing getUnitType) mb)
              divMatched = zip (sortBy (comparing getUnitType) da) (sortBy (comparing getUnitType) db)

    -- Takes a string and returns the base or derived units associated with those abbreviations (if possible)
    parseUnit :: String -> Maybe Unit
    parseUnit "1" = Just noUnit
    parseUnit unitAbb = case findIndex (== '^') unitAbb of
                            Nothing -> find ((== unitAbb) . show) allUnits
                            Just i -> let (base, exp) = splitAt i unitAbb in
                                        case parseUnit base of
                                            Nothing -> Nothing
                                            Just v -> Just $ v^(read $ tail exp)

    -- Takes a string and returns full units associated with that abbreviation (if possible)
    -- Opposite of "show" for Units
    parseUnits :: String -> Maybe Unit
    parseUnits [] = Just noUnit
    parseUnits inXs = case multUnits of
                        Nothing -> Nothing
                        Just multRes -> case divUnits of
                                            Nothing -> Just $ simplifyUnits $ foldl1 Mult multRes
                                            -- Reconstruct the unit and simplify
                                            Just divRes -> Just $ simplifyUnits $ Mult (foldl1 Mult multRes) (Inv $ foldl1 Mult divRes)
        where xs = replace " " "" $ trim inXs
              (mult, div) = case findIndex (== '/') xs of
                                -- There is no division, so the whole thing is the mult
                                Nothing -> (xs, "")
                                Just i -> splitAt i xs
              multUnits = mapM parseUnit $ split "*" $ replace ")" "" $ replace "(" "" mult
              divUnits = case div of
                            [] -> Nothing
                            (_:ds) -> mapM parseUnit $ split "*" $ replace "(" ""  $ replace ")" "" ds

    -- Takes a string and returns a quantity representing that string (if possible)
    parseQuantity :: String -> Maybe Quantity
    parseQuantity inXs = case findIndex (not . isNumeric) xs of
                                Nothing -> Nothing
                                Just i -> let (val, units) = splitAt i xs in
                                            case parseUnits units of
                                                Nothing -> Nothing
                                                Just u -> Just $ Quantity u $ read val
        where xs = trim inXs

    q = parseQuantity
