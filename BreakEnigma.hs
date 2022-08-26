module BreakEnigma where
    import Data.Char
    import Data.List
    import Data.Ord
    import Data.Maybe
    import Helper
    import Ciphers

    
    
    {- Alberto Pia
       19/12/2019
       Reg. Num.: 190169988
       Assignment 3 - COM2108 Functional Programming
    -}
    
    --TYPE DECLARATION--
    
    type SteckerPair = (Char, Char)
    
    --CONSTANTS--
    defaultOffsets = (0,0,0)
    defaultRotors = (rotor1,rotor2,rotor3)
    
--------------------FUNCTIONS-------------------
    
    --BREAK ENIGMA--
    {- combines all the functions together to find a solution for the crib given-}
    breakEnigma :: Crib -> Maybe (Offsets, Steckerboard)
    breakEnigma (plain,cipher)
        | ans /= Nothing = ans
        | otherwise = Nothing
        where crib = adjustCrib(plain,cipher)
              menu = longestMenu crib
              startLetter = plain!!(menu!!0)
              ans = breakEA crib menu [(startLetter,startLetter)] defaultOffsets
    
    --ADJUST CRIB--
    {- this function deals with cases where the cipher is longer than the plain. It keeps in mind
    the fact that the enigma machine could never encode a letter to itself, so it keeps 
    aligning the plain to the cipher until no letters encode to themselves -}
    adjustCrib :: Crib -> Crib
    adjustCrib crib@(plain,cipher)
        | length sameLetters /= 0 = adjustCrib (plain, tail cipher)
        | otherwise = unzip merged
        where merged = zip plain cipher
              sameLetters = [(x,y) | (x,y)<-merged, x==y]
    
    
    
    --BREAK EA--
    {- generates a list of all possible offsets and calls findStecker until one offset works
    if none work Nothing is returned -}
    breakEA :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe (Offsets, Steckerboard)
    breakEA crib menu board off
        | length boards /= 0 = Just $ boards
        | otherwise = Nothing
        where allOff = take (26*26*26) (iterate(updateRotors)off)
              boards = [(o,fromMaybe [] $ findStecker crib menu board o) 
                 | o<-allOff, isJust(findStecker crib menu board o)]!!0
    
    
    --FIND STECKER--
    {- generates a list of all possible pairs and then tries them all until one succeeds,
    if not Nothing is returned -}
    findStecker :: Crib -> Menu -> Steckerboard -> Offsets -> Maybe Steckerboard
    findStecker crib menu [(a,b)] off 
        | length validPairs /= 0 = validPairs!!0
        | otherwise = Nothing
        where allNum = take 26 (iterate(+1)(alphaPos b))
              modNum = [x `mod` 26 | x<-allNum] 
              allPairs = [(followMenu crib menu [(a,alphabet!!z)] off) | z<-modNum]
              validPairs = [x | x <- allPairs, isJust x]
    
    
    --FOLLOW MENU--
    {- recursively calls followMenu with the next menu position until the menu is empty or
    adding the current pairs returns Nothing. -}
    followMenu :: Crib -> Menu -> Steckerboard -> Offsets ->  Maybe Steckerboard
    followMenu _ [] b _ = Just $ removeUnsteckered b
    followMenu crib@(plain,cipher) (i:menu) board off 
        | board' == [] = Nothing 
        | otherwise = followMenu crib menu board' off
        where en = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (indexOffsets off i) board 
              en1 = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (indexOffsets off i)
              p = plain!!i
              q = useStecker en p 
              r = enigmaEncode q en1
              newPair = (r,cipher!!i)
              board' = fromMaybe [] (steckerAdd newPair board)
    

    --STECKER ADD--
    {-steckerAdd compares any combination of the pair with all the letters in the steckerboard,
    using list comprehension to extract all the characters and compare them to the new pair -}
    steckerAdd :: SteckerPair -> Steckerboard -> Maybe Steckerboard
    steckerAdd (a,b) board
        | length [(x,y) | (x,y)<-board, (a==x && b==y) || (b==x && a==y)] /= 0 = Just board
        | a `elem` l = Nothing
        | a `elem` r = Nothing
        | b `elem` l = Nothing
        | b `elem` r = Nothing
        | otherwise = Just $ (a,b):board
        where r = [x | (x,y)<-board]
              l = [y | (x,y)<-board]
    
    
    --INDEX OFFSETS--
    {- indexOffsets takes an initial offset and an index,
    returns the offset at that index e.g. (0,0,0) 31 = (0,1,5) -}
    indexOffsets :: Offsets -> Int -> Offsets
    indexOffsets off ix = (iterate(updateRotors)off)!!ix
   
    --REMOVE UNSTECKERED--
    {- removeUnsteckered, takes steckerboard and removes any unsteckered pairs e.g. (N,N) -}
    removeUnsteckered :: Steckerboard -> Steckerboard
    removeUnsteckered board = [(x,y) | (x,y)<-board, x/=y]
    
    
    {- This function takes in a crib, tries to break it and then returns the plain decoded with
    settings found. If the plain found and the original match, the function simply returns valid,
    otherwise it returns the partial decode.-}
    checkCorrectness :: Crib -> String
    checkCorrectness (plain,cipher)
       | genPlain == plain = "Valid"
       | otherwise = "Partial encode: " ++ genPlain
       where settings = fromMaybe (defaultOffsets, [])$ breakEnigma (plain,cipher)
             enigma = SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (fst settings) (snd settings)
             genPlain = enigmaEncodeMessage cipher enigma