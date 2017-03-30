{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Data.Units where
    import Data.List (intercalate, sort)
    import qualified Data.Map as Map
    import Data.Maybe
    
    import Library.List
    
    -----------------------------------------------------------------------------
    -------------------------- GENERAL LIBRARY FUNCTIONS
    -----------------------------------------------------------------------------
    -- For each item that is present in both lists, removes it from both lists
    -- Returns both lists
    cancel :: Eq a => [a] -> [a] -> ([a], [a])
    cancel (x:xs) ys = (resx, resy)
        where nextYs = case find (== x) ys of
                            Nothing -> ys
                            _ -> remove x ys
              (resx, resy) = cancel xs nextYs
              
    takeAlpha :: String -> String
    takeAlpha = takeWhile (`elem` ['a'..'z'])
    
    takeAlphaNumeric :: String -> String
    takeAlphaNumeric = takeWhile (\c -> c `elem` ('.' : (['a'..'z'] ++ ['1'..'9'])))

    -----------------------------------------------------------------------------
    -------------------------- DATA TYPE DEFINITIONS
    -----------------------------------------------------------------------------
    data Unit a where
        Base :: Floating a => String -> String -> String -> Map.Map String a -> Unit a
        Inv :: Floating a => Unit a -> Unit a -- For example, s^-1
        Mult :: Floating a => Unit a -> Unit a -> Unit a -- Could be N m (energy) or ms^-1 (velocity)
        Derived :: Floating a => String -> String -> String -> Unit a -> Unit a -- Newtons and such, for example.
                
    data Quantity a where
        Quantity :: Floating a => Unit a -> a -> Quantity a
    
    -----------------------------------------------------------------------------
    -------------------------- UNIT DEFINITIONS
    -----------------------------------------------------------------------------
    noUnit = Base "" "" "1" Map.empty
    
    second = Base "time" "second" "s" sConversions
        where sConversions = Map.fromList []
        
    gram = Base "mass" "gram" "g" gConversions
        where gConversions = Map.fromList []
    
    au = Base "length" "au" "AU" auConversions
        where auConversions = Map.fromList [("meter", 149597870700)]
    
    meter = Base "length" "meter" "m" mConversions
        where mConversions = Map.fromList [("foot", 3.28084), ("au", 6.68459*10**(-12))]
    
    foot = Base "length" "foot" "ft" ftConversions
        where ftConversions = Map.fromList [("meter", 0.3048)]
        
    newton = Derived "force" "Newton" "N" (kilo gram * meter / (second * second))
        
    baseUnits = [au, meter, foot, second, gram, noUnit]
    derivedUnits = [newton]
    allUnits = baseUnits ++ derivedUnits
    
    -- SI Prefixes are functions that convert a normal unit to its prefixed version (generates a new base unit)
    -- Also generates a conversion to the original unit
    exa :: Floating a => Unit a -> Unit a
    exa (Unit uType name abb conv) = Unit uType ("exa" ++ name) ("E" ++ abb) exaConversions
        where exaConversions = Map.insert name (10^(-18)) $ Map.map (* (10^(-18))) conv
        
    peta :: Floating a => Unit a -> Unit a
    peta (Unit uType name abb conv) = Unit uType ("peta" ++ name) ("P" ++ abb) petaConversions
        where petaConversions = Map.insert name (10^(-15)) $ Map.map (* (10^(-15))) conv
        
    tera :: Floating a => Unit a -> Unit a
    tera (Unit uType name abb conv) = Unit uType ("tera" ++ name) ("T" ++ abb) teraConversions
        where teraConversions = Map.insert name (10^(-12)) $ Map.map (* (10^(-12))) conv
        
    giga :: Floating a => Unit a -> Unit a
    giga (Unit uType name abb conv) = Unit uType ("giga" ++ name) ("G" ++ abb) gigaConversions
        where gigaConversions = Map.insert name (10^(-9)) $ Map.map (* (10^(-9))) conv
        
    mega :: Floating a => Unit a -> Unit a
    mega (Unit uType name abb conv) = Unit uType ("mega" ++ name) ("M" ++ abb) megaConversions
        where megaConversions = Map.insert name (10^(-6)) $ Map.map (* (10^(-6))) conv
        
    kilo :: Floating a => Unit a -> Unit a
    kilo (Unit uType name abb conv) = Unit uType ("kilo" ++ name) ("k" ++ abb) kiloConversions
        where kiloConversions = Map.insert name (10^(-3)) $ Map.map (* (10^(-3))) conv
        
    hecto :: Floating a => Unit a -> Unit a
    hecto (Unit uType name abb conv) = Unit uType ("hecto" ++ name) ("h" ++ abb) hectoConversions
        where hectoConversions = Map.insert name (10^(-2)) $ Map.map (* (10^(-2))) conv
        
    deca :: Floating a => Unit a -> Unit a
    deca (Unit uType name abb conv) = Unit uType ("deca" ++ name) ("da" ++ abb) decaConversions
        where decaConversions = Map.insert name (10^(-1)) $ Map.map (* (10^(-1))) conv
        
    deci :: Floating a => Unit a -> Unit a
    deci (Unit uType name abb conv) = Unit uType ("deci" ++ name) ("d" ++ abb) conversions
        where conversions = Map.insert name (10^1) $ Map.map (* (10^1)) conv
        
    centi :: Floating a => Unit a -> Unit a
    centi (Unit uType name abb conv) = Unit uType ("centi" ++ name) ("c" ++ abb) conversions
        where conversions = Map.insert name (10^2) $ Map.map (* (10^2)) conv
        
    milli :: Floating a => Unit a -> Unit a
    milli (Unit uType name abb conv) = Unit uType ("milli" ++ name) ("m" ++ abb) conversions
        where conversions = Map.insert name (10^3) $ Map.map (* (10^3)) conv
        
    micro :: Floating a => Unit a -> Unit a
    micro (Unit uType name abb conv) = Unit uType ("micro" ++ name) ("μ" ++ abb) conversions
        where conversions = Map.insert name (10^6) $ Map.map (* (10^6)) conv
        
    nano :: Floating a => Unit a -> Unit a
    nano (Unit uType name abb conv) = Unit uType ("nano" ++ name) ("n" ++ abb) conversions
        where conversions = Map.insert name (10^9) $ Map.map (* (10^9)) conv
        
    pico :: Floating a => Unit a -> Unit a
    pico (Unit uType name abb conv) = Unit uType ("pico" ++ name) ("p" ++ abb) conversions
        where conversions = Map.insert name (10^12) $ Map.map (* (10^12)) conv
        
    femto :: Floating a => Unit a -> Unit a
    femto (Unit uType name abb conv) = Unit uType ("femto" ++ name) ("f" ++ abb) conversions
        where conversions = Map.insert name (10^15) $ Map.map (* (10^15)) conv
        
    atto :: Floating a => Unit a -> Unit a
    atto (Unit uType name abb conv) = Unit uType ("atto" ++ name) ("a" ++ abb) conversions
        where conversions = Map.insert name (10^18) $ Map.map (* (10^18)) conv
    
    -----------------------------------------------------------------------------
    -------------------------- UTILITY FUNCTIONS
    -----------------------------------------------------------------------------
    collectMultUnits :: Floating a => Unit a -> [Unit a]
    collectMultUnits a@(Base _ _ _ _) = [a]
    collectMultUnits (Inv _) = []
    collectMultUnits (Mult a b) = collectMultUnits a ++ collectMultUnits b
    collectMultUnits (Derived _ _ _ base) = collectMultUnits base
    
    collectDivUnits :: Floating a => Unit a -> [Unit a]
    collectDivUnits (Base _ _ _ _) = []
    collectDivUnits a@(Inv b) = collectMultUnits b -- This should be mult, as there should never be another inv inside an inv
    collectDivUnits (Mult a b) = collectDivUnits a ++ collectDivUnits b
    collectDivUnits (Derived _ _ _ base) = collectDivUnits base
    
    getBaseUnitTuple :: Floating a => Unit a -> (Unit a, Unit b)
    getBaseUnitTuple a = (sort $ collectMultUnits a, sort $ collectDivUnits a)
    
    getUnitType :: Floating a => Unit a -> String
    getUnitType (Base uType _ _ _) = uType
    getUnitType (Derived uType _ _ _) = uType
    getUnitType a = mpart ++ dpart
            where mpart = case sort $ map getUnitType $ collectMultUnits a of
                            [] -> "1"
                            mult -> intercalate "*" $ map show mult
                  dpart = case sort $ map getUnitType $ collectDivUnits a of
                            [] -> ""
                            (d:[]) -> "/" ++ show d
                            div -> "/(" ++ intercalate "*" (map show div) ++ ")"
                      
    -- Most units don't have a name
    getUnitName :: Floating a => Unit a -> String
    getUnitName (Base _ name _ _) = name
    getUnitName (Derived _ name _ _) = name
    getUnitName _ = ""
    
    getUnitByName :: Floating a => String -> Unit a
    getUnitByName name = case find ((== name) . ) allUnits of
    
    -- Finds the derived unit, if any. If none, then just returns the input
    getDerivedUnit :: Floating a => Unit a -> Unit a
    getDerivedUnit a@(Base _ _ _ _) = a
    getDerivedUnit a = case find ((== aBase) . getBaseUnitTuple) derivedUnits of
                            Nothing -> a
                            Just dUnit -> dUnit
        where aBase = getBaseUnitTuple a
    
    simplifyUnits :: Floating a => Unit a -> Unit a
    simplifyUnits a@(Base _ _ _ _) = a
    simplifyUnits a@(Inv b) = getDerivedUnit $ Inv $ simplifyUnits b
    simplifyUnits a = getDerivedUnit m
        where m = Mult (foldl1 (Mult) aMultRes) (Inv (foldl1 (Mult) aDivRes))
              (aMult, aDiv) = getBaseUnitTuple a
              (aMultRes, aDivRes) = cancel aMult aDiv
                            
    -----------------------------------------------------------------------------
    -------------------------- INSTANCES
    -----------------------------------------------------------------------------
    instance Floating a => Show (Unit a) where
        show (Base _ _ abb _) = abb
        show (Derived _ _ abb _) = abb
        show a = mpart ++ dpart
            where mpart = case sort $ collectMultUnits a of
                            [] -> "1"
                            mult -> intercalate "*" $ map show mult
                  dpart = case sort $ collectDivUnits a of
                            [] -> ""
                            (d:[]) -> "/" ++ show d
                            div -> "/(" ++ intercalate "*" (map show div) ++ ")"

    instance Floating a => Ord (Unit a) where
        a <= b = show a <= show b
    
    instance Floating a => Eq (Unit a) where
        (Base uType1 name1 abb1 _) == (Base uType2 name2 abb2 _) = (uType1 == uType2) && (name1 == name2) && (abb1 == abb2)
        (Inv a) == (Inv b) = a == b
        (Derived type1 name1 abb1 base1) == (Derived type2 name2 abb2 base2) = (type1 == type2) && (name1 == name2) && (abb1 == abb2) && (base1 == base2)
        (Mult a1 a2) == (Mult b1 b2) = (sort aMult == sort bMult) && (sort aDiv == sort bDiv)
            where aMult = collectMultUnits a1 ++ collectMultUnits a2
                  aDiv = collectDivUnits a1 ++ collectDivUnits a2
                  bMult = collectUnits b1 ++ collectUnits b2
                  bDiv = collectDivUnits b1 ++ collectDivUnits b2
    
    instance Floating a => Num (Unit a) where
        a + b
            | a == b = a
            | otherwise = error ("Unit mismatch! First is " ++ show a ++ ", but second is " ++ show b ++ ".")
            
        a - b
            | a == b = a
            | otherwise = error ("Unit mismatch! First is " ++ show a ++ ", but second is " ++ show b ++ ".")
            
        (Derived _ _ _ a) * (Derived _ _ _ b) = simplifyUnits $ a * b
        (Derived _ _ _ a) * b = simplifyUnits $ a * b
        a * (Derived  _ _ _ b) = simplifyUnits $ a * b
        a * b = simplifyUnits $ Mult a b
    
    instance Floating a => Fractional (Unit a) where
        a@(Base _ _ _ _) / b@(Base _ _ _ _) = Mult a (Inv b)
        
        fromRational v = noUnit
        
    instance Floating a => Num (Quantity a) where
        (Quantity unitA valA) + (Quantity unitB valB)
            | unitA == unitB = Quantity unitA (valA + valB)
            | canConvert unitA unitB = Quantity unitB (convert unitA unitB valA + valB)
            | otherwise = error errMsg
            where errMsg = "Unit mismatch! Value " ++ show valA ++ " has units of " ++ show unitA ++ " and value " ++ show valB ++ " has units of " ++ show unitB ++ "."
            
        (Quantity unitA valA) - (Quantity unitB valB)
            | unitA == unitB = Quantity unitA (valA + valB)
            | canConvert unitA unitB = Quantity unitB (convert unitA unitB valA + valB)
            | otherwise = error errMsg
            where errMsg = "Unit mismatch! Value " ++ show valA ++ " has units of " ++ show unitA ++ " and value " ++ show valB ++ " has units of " ++ show unitB ++ "."
            
        (Quantity unitA valA) * (Quantity unitB valB) = Quantity (Mult unitA unitB) (valA * valB)
        
        abs (Quantity unitA valA) = Quantity unitA (abs valA)
        signum (Quantity unitA valA) = Quantity unitA (signum valA)
        fromInteger v = Quantity noUnit (fromIntegral v)
    
    instance Floating a => Fractional (Quantity a) where
        (Quantity unitA valA) / (Quantity unitB valB) = Quantity (unitA / unitB) (valA / valB)
        
        fromRational v = Quantity noUnit (fromRational v)
        
    instance Show a => Show (Quantity a) where
        show (Quantity unit val) = show val ++ " " ++ show val
        
    -----------------------------------------------------------------------------
    -------------------------- MAIN FUNCTIONS
    -----------------------------------------------------------------------------
    
    canConvert :: Floating a => Unit a -> Unit a -> Bool
    canConvert a@(Base typeA _ _ _) b@(Base typeB _ _ _)
        | typeA == typeB = True
        | otherwise = isJust $ convert (Quantity a 0) b
    canConvert (Inv a) (Inv b) = canConvert a b
    canConvert a b
        | (sort aMult == sort bMult) && (sort aDiv == sort bDiv) = True
        | otherwise = isJust $ convert (Quantity a 0) b
        where aMult = collectMultUnits a
              aDiv = collectDivUnits a
              bMult = collectUnits b
              bDiv = collectDivUnits b
              
    -- Returns the total conversion factor for those two units if possible
    -- Otherwise returns Nothing
    getConversion :: Floating a => Unit a -> Unit a -> Maybe a
    getConversion b (Base _ _ _ conv) = case Map.lookup (show b) conv of
                                            Nothing -> -- check the other possible paths
                                            Just v -> v -- We found it
        where allConvs = map (getConversion b . getUnitByName . fst) $ Map.fromList conv
    
    convert :: Floating a => Quantity a -> Unit a -> Maybe (Quantity a)
    convert a@(Quantity unit val) b
        | unit == b = a
        | otherwise = Quantity b val
        where (m, d) = getBaseUnitTuple unit
    
    parseUnits :: Floating a => String -> Maybe (Unit a)
    parseUnits [] = noUnit
    parseUnits xs
        | length xs > 1 = case takeAlpha xs of
                            [] -> Nothing
                            res -> 
        | otherwise = 
    
    parseQuantity :: Floating a => String -> Maybe (Quantity a)