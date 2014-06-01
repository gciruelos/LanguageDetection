import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy)
import Data.Function (on)
import Data.Maybe

p = "El cálculo de la frecuencia de letras en una lengua es difícil y está sujeto a la interpretación. Se cuenta la frecuencia de las letras de un texto arbitrariamente largo, pero en los resultados influyen varios parámetros."
q = "The frequency of letters in text has often been studied for use in cryptanalysis, and frequency analysis in particular. No exact letter frequency distribution underlies a given language, since all writers write slightly differently."


type Language = (String, [[(String, Float)]])

spanish :: Language
spanish = ("Spanish",
           [[("e",0.1368), ("a",0.1253), ("o",0.868), ("s",0.798), ("r",0.687), ("n",0.671), ("i",0.625), ("d", 0.586), ("l", 0.497)]])
english :: Language
english = ("English",
           [[("e",0.13), ("t",0.1253), ("a",0.868), ("o",0.798), ("i",0.687), ("n",0.671), ("s",0.625), ("h", 0.586), ("r", 0.497)]])


----------------------- N-GRAM ANALYSIS --------------------------------
ngram :: Int -> String -> [String]
ngram n word = [take n (drop i word) | i <- [0..((length word)-n)]]

clean :: String -> [String]
clean phrase = words (map toLower (filter (\x -> isAlpha x || x==' ') phrase))

ngrams :: Int -> String -> [String]
ngrams n phrase = concat $ map (ngram n) (clean phrase)


---------------------- FREQUENCY ---------------------------------------
divFloat :: Int -> Int -> Float
divFloat x y = (fromIntegral x) / (fromIntegral y)

sortSndDec :: Ord b => [(a, b)] -> [(a, b)]
sortSndDec list = reverse $ sortBy (compare `on` snd) list

frequency :: [String] -> [(String, Float)] 
frequency list = sortSndDec (map (\l -> (head l, divFloat (length l) (length list))) ((group .sort) list))

------------------------------------------------------------------------

languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs((fromJust(search c)) - fromJust(lookup c freq)) | c <- (map fst freq)]
                              --sum $ filter (\x -> x notElem (map fst freq)) (map fst grams)
                                    where grams      = lang !! (n-1)
                                          search x   = lookup x grams
                                          freq       = filter (\i -> isJust (search $ fst i)) (frequency $ ngrams n phrase)



detect :: String -> [(String, Float)]
detect phrase = [head $ sortSndDec[(fst lang, languageProbability (snd lang) n phrase) | lang <- [english, spanish]] | n <- [1]]
