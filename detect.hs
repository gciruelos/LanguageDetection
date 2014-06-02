import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy)
import Data.Function (on)
import Data.Maybe

p = "El cálculo de la frecuencia de letras en una lengua es difícil y está sujeto a la interpretación. Se cuenta la frecuencia de las letras de un texto arbitrariamente largo, pero en los resultados influyen varios parámetros."
q = "The frequency of letters in text has often been studied for use in cryptanalysis, and frequency analysis in particular. No exact letter frequency distribution underlies a given language, since all writers write slightly differently."


type Language = (String, [[(String, Float)]])


--http://practicalcryptography.com/cryptanalysis/letter-frequencies-various-languages/
spanish, english :: Language

spanish = ("Spanish",
           [[("a",0.1297656),("e",0.12887716),("o",8.811309e-2),("s",7.593987e-2),("n",6.758174e-2),("r",6.291179e-2),("l",6.0097132e-2),("d",4.9237914e-2),("i",4.879369e-2),("u",4.643072e-2),("c",3.824804e-2),("t",3.620237e-2),("m",2.8635636e-2),("p",2.3431871e-2),("b",1.7070886e-2)],
            [("de",2.8322656e-2),("en",2.6526561e-2),("es",2.2604384e-2),("la",2.2128254e-2),("os",2.1142995e-2),("ue",2.1133566e-2),("er",1.8686919e-2),("an",1.7630948e-2),("ra",1.7357526e-2),("qu",1.716896e-2),("as",1.6829541e-2),("el",1.5137158e-2),("re",1.46798855e-2),("ar",1.4269753e-2),("co",1.4071759e-2)],
            [("que",1.9388521e-2),("ent",8.790797e-3),("los",7.640365e-3),("con",7.584094e-3),("nte",6.840065e-3),("est",6.6087283e-3),("las",5.639615e-3),("ado",5.427035e-3),("aba",5.3957733e-3),("era",5.33325e-3),("com",5.158184e-3),("por",4.770539e-3),("ndo",4.670501e-3),("par",4.589221e-3),("ien",4.482931e-3)]
            ])

english = ("English",
           [[("e",0.12409292),("t",9.1239974e-2),("a",8.16647e-2),("o",7.898921e-2),("n",6.8285726e-2),("i",6.6967726e-2),("h",6.691002e-2),("s",6.159093e-2),("r",5.5415437e-2),("d",4.445838e-2),("l",4.0719982e-2),("u",2.8308261e-2),("w",2.8050127e-2),("m",2.7700886e-2),("f",2.1968778e-2)],
            [("th",3.9204806e-2),("he",3.797695e-2),("in",2.3124618e-2),("an",2.2908168e-2),("er",1.997627e-2),("nd",1.789836e-2),("re",1.6395023e-2),("ha",1.6229736e-2),("ou",1.4921202e-2),("at",1.4057375e-2),("en",1.3067612e-2),("to",1.30656455e-2),("hi",1.2066045e-2),("or",1.2052271e-2),("ed",1.1780726e-2)],
            [("the",3.1920146e-2),("and",1.8501451e-2),("ing",1.2713747e-2),("her",9.17255e-3),("tha",8.46486e-3),("hat",8.456631e-3),("ere",6.7504924e-3),("for",6.459736e-3),("all",6.083946e-3),("his",6.0482877e-3),("you",5.225391e-3),("was",5.2226484e-3),("not",5.143102e-3),("thi",5.0004665e-3),("ith",4.586275e-3)]
            ])

----------------------- N-GRAM -----------------------------------------
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


---------------------- LEARN -------------------------------------------
learn :: FilePath -> Int -> IO [(String, Float)]
learn file n = fmap ((take 20) . frequency. (ngrams n)) (readFile file)


------------------------------------------------------------------------
languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs((fromJust(search c)) - fromJust(lookup c freq)) | c <- (map fst freq)]
                              --sum $ filter (\x -> x notElem (map fst freq)) (map fst grams)
                                    where grams      = lang !! (n-1)
                                          search x   = lookup x grams
                                          freq       = filter (isJust . search . fst) (frequency $ ngrams n phrase)



detect :: String -> [(String, Float)]
detect phrase = [head $ sortSndDec[(fst lang, languageProbability (snd lang) n phrase) | lang <- [english, spanish]] | n <- [1,2,3]]
