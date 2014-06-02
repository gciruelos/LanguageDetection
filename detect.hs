import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy, groupBy, maximumBy)
import Data.Function (on)
import Data.Maybe

type Language = (String, [[(String, Float)]])

spanish, english, italian :: Language
spanish = ("Spanish",
           [[("e",0.13530396),("a",0.11784992),("o",9.3477674e-2),("s",7.663439e-2),("n",6.609797e-2),("r",6.1534382e-2),("l",5.4334555e-2),("d",5.3174e-2),("u",4.7661368e-2),("i",4.7309056e-2)],
            [("ue",2.8653601e-2),("de",2.6397998e-2),("en",2.6267093e-2),("es",2.565904e-2),("qu",2.516098e-2),("er",2.114551e-2),("os",2.0169526e-2),("la",1.7962722e-2),("do",1.6736548e-2),("an",1.6714085e-2)],
            [("que",2.670043e-2),("est",9.823831e-3),("ent",7.967839e-3),("con",7.331942e-3),("los",6.333119e-3),("ien",5.9381393e-3),("ado",5.7613286e-3),("por",5.7189357e-3),("ero",5.589688e-3),("nte",5.454237e-3)]
            ])
english = ("English",
           [[("e",0.11964177),("t",8.472684e-2),("a",7.876179e-2),("o",7.730373e-2),("i",6.884711e-2),("n",6.77764e-2),("s",6.5048866e-2),("h",6.135803e-2),("r",5.9074517e-2),("l",4.656001e-2)],
            [("he",3.6002513e-2),("th",3.4096308e-2),("in",2.4494225e-2),("er",2.0081425e-2),("an",1.886048e-2),("re",1.4552695e-2),("nd",1.3641494e-2),("on",1.2544658e-2),("ng",1.2539354e-2),("at",1.2484195e-2)],
            [("the",2.9582798e-2),("ing",1.3659975e-2),("and",1.2732922e-2),("her",7.7394815e-3),("hat",5.8811633e-3),("his",5.5651227e-3),("tha",4.5636254e-3),("ere",4.5257006e-3),("for",4.2588217e-3),("ith",4.160498e-3)]
            ])
italian = ("Italian",
           [[("e",0.11549025),("a",0.10535723),("i",9.8423064e-2),("o",9.3562976e-2),("n",6.590296e-2),("r",6.442219e-2),("l",5.8093686e-2),("t",5.6600556e-2),("s",5.6029506e-2),("c",5.1720686e-2)],
            [("er",2.0541657e-2),("ch",2.0438965e-2),("co",1.750133e-2),("on",1.7448427e-2),("an",1.6770033e-2),("di",1.6530417e-2),("la",1.527321e-2),("ra",1.4339639e-2),("en",1.4009777e-2),("to",1.3963099e-2)],
            [("che",1.658859e-2),("per",9.675995e-3),("que",6.7204633e-3),("chi",6.307588e-3),("ent",6.3035e-3),("non",6.1604246e-3),("con",6.078667e-3),("ndo",5.4777497e-3),("ant",5.3469376e-3),("qua",4.9790293e-3)]
            ])

supportedLanguages = [english, spanish, italian]

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
learn file n = fmap ((take 10) . frequency. (ngrams n)) (readFile file)


------------------------------------------------------------------------
languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs((fromJust(search c)) - fromJust(lookup c freq)) | c <- (map fst freq)] + ((sum . (map snd)) (filter (\x -> (fst x) `notElem` (map fst freq)) grams))
                                    where grams      = lang !! (n-1)
                                          search x   = lookup x grams
                                          freq       = filter (isJust . search . fst) (frequency $ ngrams n phrase)


detectNGram :: String -> [(String, Float)]
detectNGram phrase = [(fst lang, sum [(languageProbability (snd lang) n phrase)*(fromIntegral n) | n <- [1,2,3]]) | lang <- supportedLanguages]

detectWithProb :: String -> (String, Float)
detectWithProb = last . sortSndDec . detectNGram

detect :: String -> String
detect = fst . detectWithProb

