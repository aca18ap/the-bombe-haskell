module Ciphers where
    import Data.Char
    import Data.List
    import Data.Ord
    import AssignmentHelp
    
    --Alberto Pia, Reg.Num.180169988, COM2108, Assignment 2--
    
    
    --ASSIGNMENT 1 FUNCTIONS
    
    
    rotate :: Cipher->String
    rotate cipher = (last cipher):(init cipher)
   
    offsetCipher :: Cipher->Int->String
    offsetCipher cipher offset = (iterate(rotate)cipher)!!(offset)
    
    encode :: Cipher -> Int -> Char -> Char
    encode cipher offset char = [x | (x,y)<-zip(offsetCipher cipher offset)['A'..'Z'], y==char]!!0
    
    reverseEncode :: Cipher->Int->Char->Char
    reverseEncode cipher offset encChar = [y | (x,y)<-zip(offsetCipher cipher offset)['A'..'Z'], x==encChar]!!0
    
---------------------------------------------------------------------------------------------------------------------------------	
    --ASSIGNMENT 2--
    
    {- type and data declarations, alphabet used later on for encode and decode functions -}
    alphabet = ['A'..'Z']
    trd (_,_,a) = a
    snd' (_,b,_) = b
    fst' (c,_,_) = c
    type Rotor = String
    type Reflector = [(Char,Char)]
    type Offsets = (Int,Int,Int)
    type Steckerboard = [(Char,Char)]
    type Crib = (String,Cipher)
    type Menu = [Int]
    data Enigma = SimpleEnigma {lr::Rotor, mr::Rotor, rr::Rotor, ref::Reflector, off::Offsets}
                               | SteckeredEnigma {lr::Rotor, mr::Rotor, rr::Rotor, ref::Reflector, off::Offsets, steck::Steckerboard}deriving (Show)
   
   
    --UPDATE ROTORS--
    {- updateRotors, function to increment the offset -}
    updateRotors :: Offsets->Offsets
    updateRotors off = case off of (25,25,25) -> (0,0,0)
                                   (x,25,25)|x<25 -> (x+1,0,0)
                                   (x,y,25)|y<25 -> (x,y+1,0)
                                   (x,y,z)|z<25 -> (x,y,z+1)
    {- tested by passing different combination of offsets including edge cases such as (0,0,25),(0,25,0),(0,25,25). Assumes that no offset will be <25 -}
   
   
    --USE REFLECTORS--
    {- useReflector takes a reflector and a char and returns the matching reflector character -}
    useReflector :: Reflector->Char->Char
    useReflector reflector char = (if elem char (map snd reflector) then [x | (x,y) <- reflector, y==char] else [y | (x,y) <- reflector, x==char])!!0
    {-tried with different characters of the alphabet, assumes the character passed in is a letter and upper case -}
    
    
    --NEW ENCODE--
    {- new encode implements the different use of offsets for the enigma machine by shifting the alphabet by the offset before encoding the character with the cipher
    to then "unshift" the alphabet -}
    newEncode :: Cipher->Int->Char->Char
    newEncode cipher offset char = encode alphabet offset (encode cipher 0 (reverseEncode alphabet offset char))
    {- passed in different characters and compared it to manually calculated values, returning correct encodings -}
    
    
    --NEW REVERSE ENCODE--
    {-newReverseEncode works like newEncode but reverseEncode the character with the cipher instead, used 3 times after the reflector is used-}
    newReverseEncode :: Cipher->Int->Char->Char
    newReverseEncode cipher offset char = encode alphabet offset(reverseEncode cipher 0 (reverseEncode alphabet offset char))
    {- like the newEncode function, compared results with values worked out by hand -}
    
    
    --TRIPLE ENCODE--
    {- tripleEncode combines the function newEncode 3 times using the 3 different offsets and rotors from the enigma passed in, the offsets are updated before the function runs
     this function completes the first 3 encodings before the reflector is used in the enigma machine-}
    tripleEncode :: Enigma->Char->Char
    tripleEncode enig char = newEncode (lr enig) (fst' uoff) (newEncode (mr enig) (snd' uoff) (newEncode (rr enig) (trd uoff) char)) where uoff = (updateRotors (off enig))
    {- any character passed in returns the correct encoded character worked out by hand or by executing newEncode 3 times on the character-}
    
    
    --TRIPLE DECODE--
    {- tripleDecode works like tripleEncode but using the rotors and offsets in a different order, to be used after the reflector has been used in the enigmaencode -}
    tripleDecode :: Enigma->Char->Char
    tripleDecode enig char = newReverseEncode (rr enig) (trd uoff) (newReverseEncode (mr enig) (snd' uoff) (newReverseEncode (lr enig) (fst' uoff) char)) where uoff = (updateRotors (off enig))
    {- like tripleEncode, values passed encoded correctly compared to values worked out by hand or running 3 times newReverseEncode on the character -}
    
    
    --USE STECKERBOARD--
    {- useStecker takes an enigma and a character and returns the matching steckerboard character, if none it returns the passed in character, it tries both using list 
    comprehension and then using if statements it is checked if one of the lists contains the stecker character, else returning the character -}
    useStecker :: Enigma->Char->Char
    useStecker enig char = do {let x = [x | (x,y) <- (steck enig), y==char]
                              ;let y = [y | (x,y) <- (steck enig), x==char]
                              ;if (length x)/=0 then x!!0 else (if (length y)/=0 then y!!0 else char)}
    {- tested using different steckerboards and passing characters that both were and werent in the steckerboard, returning the expected value each time. Function assumes the enigma
    passed uses a steckerboard -}
    
    
    --ENIGMA ENCODE--	
    {- enigma encode executes tripleEncode, useReflector and tripleDecode in that order for SimpleEnigmas. For steckered enigmas the useStecker function is used at the start
    and end of the enigmaEncode -}
    enigmaEncode :: Char->Enigma->Char
    enigmaEncode char enig@(SimpleEnigma _ _ _ _ _) = tripleDecode enig (useReflector (ref enig)(tripleEncode enig char))
    enigmaEncode char enig@(SteckeredEnigma _ _ _ _ _ _) = useStecker enig (tripleDecode enig (useReflector (ref enig)(tripleEncode enig (useStecker enig char))))
    {- using examples from the pdf and assignment page the function returns the correct value for any character given the correct enigma -}
    
    
    --ENIGMA ENCODE MESSAGE--
    {- enigmaEncodeMessage executes enigmaEncode on every value of the message. To use the correct offsets for each character, first a list of offsets is generated matching the 
    length of the message. A list of enigmas using the updated offsets is then created. Finally using list comprehension I encode character of message with the corresponding 
    enigma and offsets. Steckered enigma does the same operations but takes the steckerboard into account when creating the list of updated enigmas-}
    enigmaEncodeMessage :: String->Enigma->String
    enigmaEncodeMessage message enig@(SimpleEnigma _ _ _ _ _) = 
                                          do {let offsets = (take (length message)(iterate(updateRotors)(off enig)))
                                          ;let newEnigs = [(SimpleEnigma (lr enig)(mr enig)(rr enig)(ref enig)(newOff)) | newOff <- offsets]
                                          ;[enigmaEncode char e | (char,e)<-zip(message)(newEnigs)] }
    enigmaEncodeMessage message enig@(SteckeredEnigma _ _ _ _ _ _) = 
                                          do {let offsets = (take (length message)(iterate(updateRotors)(off enig)))
                                          ;let newEnigs = [(SteckeredEnigma (lr enig)(mr enig)(rr enig)(ref enig)(newOff)(steck enig)) | newOff <- offsets]
                                          ;[enigmaEncode char e | (char,e)<-zip(message)(newEnigs)] }
    {- the cipher matches to the one provided in the assignment page when using the correct setting of rotors and offsets. The operation is always reversible using the same settings -}
    
    
    
    {-attempt at longestMenu, not finished -}
    --findNextChar crib char = elemIndex(char)(fst crib)
    --longestMenu :: Crib->Menu
    --longestMenu crib = [(findNextChar crib $(snd crib)!!(findNextChar crib char)) | char <- (fst crib), (findNextChar crib char) /= Nothing]