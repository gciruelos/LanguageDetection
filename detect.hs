import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy, groupBy, maximumBy)
import Data.Function (on)
import Data.Maybe

type Language = (String, [[(String, Float)]])

spanish, english, italian :: Language
spanish = ("Spanish",
           [[("e",0.17959654),("a",0.15642881),("o",0.12407816),("s",0.10172113),("n",8.773554e-2),("r",8.167804e-2),("l",7.212131e-2),("d",7.058083e-2),("u",6.326361e-2),("i",6.279597e-2)],
            [("ue",0.1274246),("de",0.11739377),("en",0.116811626),("es",0.11410757),("qu",0.11189266),("er",9.403558e-2),("os",8.969532e-2),("la",7.9881504e-2),("do",7.442862e-2),("an",7.432872e-2)],
            [("que",0.3082497),("est",0.11341364),("ent",9.1986686e-2),("con",8.464542e-2),("los",7.311425e-2),("ien",6.855432e-2),("ado",6.6513084e-2),("por",6.602366e-2),("ero",6.4531535e-2),("nte",6.2967785e-2)]
            ])
english = ("English",
           [[("e",0.16409378),("t",0.116208926),("a",0.10802627),("o",0.10602643),("i",9.442875e-2),("n",9.296018e-2),("s",8.921453e-2),("h",8.415693e-2),("r",8.102493e-2),("l",6.385923e-2)],
            [("he",0.18064724),("th",0.17108262),("in",0.12290292),("er",0.10076113),("an",9.4634876e-2),("re",7.302001e-2),("nd",6.844795e-2),("on",6.2944435e-2),("ng",6.291782e-2),("at",6.264105e-2)],
            [("the",0.31922695),("ing",0.14740431),("and",0.13740052),("her",8.351648e-2),("hat",6.346343e-2),("his",6.0053047e-2),("tha",4.9245927e-2),("ere",4.883668e-2),("for",4.5956798e-2),("ith",4.4895794e-2)]
            ])

italian = ("Italian",
           [[("e",0.15084873),("a",0.13761339),("i",0.12855625),("o",0.1222082),("n",8.6079806e-2),("r",8.414568e-2),("l",7.587964e-2),("t",7.392938e-2),("s",7.318349e-2),("c",6.755549e-2)],
            [("er",0.123139195),("ch",0.1225236),("co",0.10491362),("on",0.10459649),("an",0.10052978),("di",9.909338e-2),("la",9.1556914e-2),("ra",8.596052e-2),("en",8.398313e-2),("to",8.370331e-2)],
            [("che",0.22526924),("per",0.1313978),("que",9.126235e-2),("chi",8.56556e-2),("ent",8.5600086e-2),("non",8.365715e-2),("con",8.2546905e-2),("ndo",7.438659e-2),("ant",7.2610185e-2),("qua",6.761408e-2)]
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
learnPercent :: FilePath -> Int -> IO [(String, Float)]
learnPercent file n = fmap ((take 10) . frequency. (ngrams n)) (readFile file)

fixValues :: [(String, Float)] -> [(String, Float)]
fixValues l = map (\x -> (fst x, (snd x) / total)) l
              where total = (sum . (map snd)) l


learn :: FilePath -> Int -> IO [(String, Float)]
learn file n = fmap fixValues (learnPercent file n)


------------------------------------------------------------------------
languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs((fromJust(search c)) - fromJust(lookup c freq)) | c <- (map fst freq)] + ((sum . (map snd)) (filter (\x -> (fst x) `notElem` (map fst freq)) grams))
                                    where grams      = lang !! (n-1)
                                          search x   = lookup x grams
                                          freq       = filter (isJust . search . fst) (frequency $ ngrams n phrase)


detectNGram :: String -> [(String, Float)]
detectNGram phrase = [(fst lang, sum [languageProbability (snd lang) n phrase | n <- [1..3]]) | lang <- supportedLanguages]

detectWithProb :: String -> (String, Float)
detectWithProb = last . sortSndDec . detectNGram

detect :: String -> String
detect = fst . detectWithProb

