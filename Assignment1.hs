module Ciphers where
    import Data.Char
    import Data.List
    import Data.Ord
    import AssignmentHelp

    --Alberto Pia, Reg.Num.180169988, COM2108, Assignment 1--
    
    -- Validate cipher 
    --DESCRIPTION: Compares the cipher sorted in alphabetical order with a list of the alphabet, if matching cipher is valid
    validateCipher :: Cipher -> Bool
    validateCipher cipher = sort(cipher) == ['A'..'Z']
    --TEST: I tried to pass all 5 rotors given in the help file and they all returned TRUE, whilst if any letter was changed or removed 
    --the function returned FALSE
    
    
    --Encode/Message 
    --DESCRIPTION: The rotate function drops the last character of the cipher and puts it at the beginning,
    --the offset function repeats the rotate function the number of times specified by the offset,
    --the encode function zips the cipher with the alphabet and then returns the character matching,
    --the encodeMessage function executes the encode function for every character of the message.
    encode :: Cipher->Int->Char->String
    offsetCipher :: Int->Cipher->Cipher
    rotate :: Cipher->String
    encodeMessage :: Cipher->Int->String->String
    
    rotate cipher = (last cipher):(init cipher)
    offsetCipher offset cipher = (iterate(rotate)cipher)!!(offset)
    encode cipher offset character = [x | (x,y)<-zip(offsetCipher offset cipher)['A'..'Z'], y==character]
    encodeMessage cipher offset message = if validateCipher cipher then concat[(encode cipher offset x) | x<-message] else "Cipher is not valid"
    --TEST: I passed in different strings such as "HELLOWORLD" and tried different ciphers and offsets combinations
    --all returning the encoded string. For every encoded message I tried I encoded them myself by hand and 
    --they were all correct. The function can deal with any size offset and the cipher must be valid otherwise
    --a message is returned
    
    
    --ReverseEncode/Message 
    --The reverseEncode function works like encode but compares and returns the other value of the tuple
    --The reverseEncodeMessage function just like encodeMessage, executes reverseEncode for each character of encMessage
    reverseEncode :: Cipher->Int->Char->String
    reverseEncodeMessage :: Cipher->Int->String->String
    
    reverseEncode cipher offset encChar = [y | (x,y)<-zip(offsetCipher offset cipher)['A'..'Z'], x==encChar]
    reverseEncodeMessage cipher offset encMessage = if validateCipher cipher then concat[(reverseEncode cipher offset x) | x<-encMessage] else "Cipher is not valid"
    --TEST: To test reverseEncodeMessage I used the results from encodeMessage using the same cipher and offset and they
    --have returned the original strings. This function also deals with invalid ciphers and any offset size
    
    
    --LetterStats 
    --The function count returns the length of a list containing all the characters that are the same
    --The function letterStats goes through the alphabet and using the function count we are able to know how many times
    count :: String->Char->Int
    letterStats :: [Char]->[(Char,Int)]
    
    count message character = length(filter(==character)message)
    letterStats message = reverse (sortBy(comparing snd) [(x,(percent (count message x) (length(message)))) | x <- ['A'..'Z'], count message x /=0])
    --TEST: To test this I started by trying passing a single letter as function, with it returning 100 in all cases. Adding more letters the percentages were
    --always correct and they added up to 100 llike the should. The list of letters is returned in descending order and only upper case letters are shown.
    
    
    --PartialDecode
    --To write partialDecode I used a combination of functions that can decode a single character with a single guess, moving on to a single character and multiple
    --guesses, ending with the final function going through each character of the message and comparing it with all the guesses passed in, checking if any of them appeared.
    singleCharDecode :: (Char,Char)->Char->Char
    singleGuessDecode :: (Char,Char)->String->[Char]
    singleCharManyGuess :: [(Char,Char)]->Char->Char
    partialDecode :: [(Char,Char)]->String->String
    
    singleCharDecode guess character = if (snd guess==character) then toLower(fst guess) else character
    singleGuessDecode guess message = [singleCharDecode guess singleChar | singleChar <- message]
    singleCharManyGuess guesses character = head [singleCharDecode guess character | guess<-guesses, snd guess == character]
    partialDecode guesses message = [if elem character (map snd guesses) then singleCharManyGuess guesses character else character | character<-message]
    --TEST: Using the example in the assignment brief returned the same result, then I moved on to try and partialDecode the mystery message with the guesses supplied
    --as reflectorB. I found this part rather confusing as I thought the assignment required us to partially decode the mystery message with that but it only returned 
    --an unreadable message. I proceeded to try using the letter frequencies to try decode the mystery but that also returned an unreadable message, below I left the function
    --I wrote to decipher it as mysteryTrySolved. In the end I figured that the mystery message must have been encoded using one of the rotors and an offset, and since the brief specified 
    --the message contained punctuation written as words, I tested for the reverseEncoded message to contain the string "STOP" anywhere in it. To find that I wrote the function
    --mysterySolved1, turns out the mystery message was encoded using rotor2 and offset of 18.
    
    
    mysteryTrySolved = partialDecode (zip (map toUpper(map fst (reverse(sortBy(comparing snd) engFreq))))(map fst(letterStats mystery))) mystery 
    mysterySolved1 cipher mystery = [if  isInfixOf "STOP" (reverseEncodeMessage cipher offset mystery) then (reverseEncodeMessage cipher offset mystery) else "NO"  | offset<-[0..26]]