import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy, groupBy, maximumBy)
import Data.Function (on)
import Data.Maybe

type Language = (String, [[(String, Float)]])

spanish, english, italian :: Language
spanish = ("Spanish",
           [[("e",0.1487224),("a",0.13570592),("o",0.103057705),("s",8.30638e-2),("n",7.373588e-2),("r",7.00455e-2),("l",6.163288e-2),("d",5.699944e-2),("i",5.5384815e-2),("u",5.0404027e-2),("t",4.3898344e-2),("c",4.1449346e-2),("m",3.1305082e-2),("p",2.5570406e-2),("q",1.9024482e-2)],
            [("en",9.11264e-2),("de",9.025751e-2),("ue",8.59461e-2),("es",8.359241e-2),("qu",7.5394854e-2),("er",7.1926646e-2),("os",6.7675725e-2),("la",6.5349534e-2),("ra",6.1316743e-2),("do",5.5262055e-2),("an",5.479279e-2),("as",5.455632e-2),("ar",4.9531832e-2),("el",4.8069026e-2),("on",4.520208e-2)],
            [("que",0.21683818),("ent",8.4788516e-2),("est",8.1647955e-2),("con",6.736604e-2),("nte",5.686873e-2),("los",5.6749213e-2),("ndo",5.3090762e-2),("ien",5.2738857e-2),("ado",5.173627e-2),("por",5.1437482e-2),("aba",4.829692e-2),("ero",4.6716683e-2),("las",4.4233445e-2),("era",4.4107296e-2),("tra",4.338357e-2)]
            ])
english = ("English",
           [[("e",0.13435903),("t",0.100457065),("a",9.1642216e-2),("o",9.023508e-2),("n",7.9675145e-2),("i",7.7340804e-2),("s",7.218792e-2),("h",7.007509e-2),("r",6.336313e-2),("d",5.1451437e-2),("l",5.12633e-2),("u",3.3276487e-2),("m",2.957192e-2),("w",2.7913457e-2),("g",2.7187878e-2)],
            [("he",0.1405049),("th",0.13262807),("in",9.043757e-2),("an",8.149719e-2),("er",7.4490264e-2),("nd",6.3181475e-2),("ou",5.292134e-2),("re",5.2322533e-2),("on",4.7654226e-2),("ng",4.7311626e-2),("at",4.5461588e-2),("en",4.3897543e-2),("ed",4.3691978e-2),("ha",4.2226247e-2),("it",4.1773416e-2)],
            [("the",0.2580196),("and",0.13972743),("ing",0.1129443),("her",6.674776e-2),("hat",5.213958e-2),("his",4.2310346e-2),("you",4.0918674e-2),("tha",4.0603578e-2),("ere",3.8975585e-2),("was",3.866049e-2),("for",3.7014987e-2),("all",3.4791812e-2),("ter",3.2945e-2),("ver",3.2691177e-2),("out",3.1509567e-2)]
            ])

italian = ("Italian",
           [[("e",0.1263113),("a",0.11522885),("i",0.107644975),("o",0.102329515),("n",7.207785e-2),("r",7.045834e-2),("l",6.353687e-2),("t",6.1903838e-2),("s",6.1279282e-2),("c",5.656674e-2),("d",4.0674414e-2),("u",3.645935e-2),("m",3.2682285e-2),("p",3.0635586e-2),("v",2.2210868e-2)],
            [("er",8.9174986e-2),("ch",8.872918e-2),("co",7.597639e-2),("on",7.574672e-2),("an",7.2801694e-2),("di",7.1761474e-2),("la",6.6303715e-2),("ra",6.225092e-2),("en",6.0818933e-2),("to",6.0616292e-2),("te",5.5861015e-2),("he",5.5550303e-2),("no",5.54017e-2),("or",5.5253096e-2),("ta",5.3753562e-2)],
            [("che",0.17018242),("per",9.926609e-2),("que",6.894527e-2),("chi",6.470958e-2),("ent",6.466764e-2),("non",6.319983e-2),("con",6.236108e-2),("ndo",5.6196265e-2),("ant",5.4854263e-2),("qua",5.1079888e-2),("ell",5.074439e-2),("nte",5.0115325e-2),("est",4.9066886e-2),("com",4.734745e-2),("and",4.7263578e-2)]
            ])

supportedLanguages = [english, spanish, italian]
nGram              = 3

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
learnPercent :: FilePath -> Int -> IO [(String, Float)]
learnPercent file n = fmap ((take 15) . frequency. (ngrams n)) (readFile file)

fixValues :: [(String, Float)] -> [(String, Float)]
fixValues l = map (\x -> (fst x, (snd x) / total)) l
              where total = (sum . (map snd)) l

learn :: FilePath -> Int -> IO [(String, Float)]
learn file n = fmap fixValues (learnPercent file n)

-- Lo mas cabeza que hice ever
automaticLearn :: FilePath -> IO ()
automaticLearn file = do a0 <- a !! 0; print a0
                         a1 <- a !! 1; print a1
                         a2 <- a !! 2; print a2
                         return ()
                         where a = (fmap (learn file) [1..nGram])


------------------------------------------------------------------------
languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs((fromJust(search c)) - fromJust(lookup c freq)) | c <- (map fst freq)] + ((sum . (map snd)) (filter (\x -> (fst x) `notElem` (map fst freq)) grams))
                                    where grams      = lang !! (n-1)
                                          search x   = lookup x grams
                                          freq       = filter (isJust . search . fst) (frequency $ ngrams n phrase)


detectNGram :: String -> [(String, Float)]
detectNGram phrase = [(fst lang, sum [languageProbability (snd lang) n phrase | n <- [1..nGram]]) | lang <- supportedLanguages]

detectWithProb :: String -> (String, Float)
detectWithProb = normalize . last . sortSndDec . detectNGram
                 where normalize (a,b) = (a, b / (fromIntegral nGram))

detect :: String -> String
detect = fst . detectWithProb

