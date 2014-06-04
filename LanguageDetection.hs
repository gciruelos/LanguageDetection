module LanguageDetection where

import Data.Char (toLower, isAlpha)
import Data.List (sort, group, sortBy, groupBy, maximumBy)
import Data.Function (on)
import Data.Maybe


type Language = (String, [[(String, Float)]])
spanish, english, italian, french, german :: Language
spanish = ("Spanish",
           [[("e",0.1487224),("a",0.13570592),("o",0.103057705),("s",8.30638e-2),("n",7.373588e-2),("r",7.00455e-2),("l",6.163288e-2),("d",5.699944e-2),("i",5.5384815e-2),("u",5.0404027e-2),("t",4.3898344e-2),("c",4.1449346e-2),("m",3.1305082e-2),("p",2.5570406e-2),("q",1.9024482e-2)],
            [("en",9.11264e-2),("de",9.025751e-2),("ue",8.59461e-2),("es",8.359241e-2),("qu",7.5394854e-2),("er",7.1926646e-2),("os",6.7675725e-2),("la",6.5349534e-2),("ra",6.1316743e-2),("do",5.5262055e-2),("an",5.479279e-2),("as",5.455632e-2),("ar",4.9531832e-2),("el",4.8069026e-2),("on",4.520208e-2)],
            [("que",0.21683818),("ent",8.4788516e-2),("est",8.1647955e-2),("con",6.736604e-2),("nte",5.686873e-2),("los",5.6749213e-2),("ndo",5.3090762e-2),("ien",5.2738857e-2),("ado",5.173627e-2),("por",5.1437482e-2),("aba",4.829692e-2),("ero",4.6716683e-2),("las",4.4233445e-2),("era",4.4107296e-2),("tra",4.338357e-2)]
            ])
english = ("English",
           [[("e",0.1375582),("t",9.7873785e-2),("a",8.941654e-2),("o",8.857305e-2),("n",7.851809e-2),("i",7.744325e-2),("s",7.636313e-2),("h",6.9370724e-2),("r",6.823243e-2),("l",5.2671727e-2),("d",4.7101866e-2),("u",3.2112293e-2),("m",3.0451782e-2),("c",2.7663546e-2),("g",2.6649524e-2)],
            [("th",0.14128864),("he",0.13721545),("in",9.1359265e-2),("an",7.6659665e-2),("er",7.437671e-2),("nd",5.851646e-2),("re",5.6102116e-2),("ng",4.989105e-2),("ou",4.854154e-2),("es",4.758894e-2),("on",4.718381e-2),("at",4.5798708e-2),("en",4.3614294e-2),("or",4.192261e-2),("ar",3.9940763e-2)],
            [("the",0.280356),("and",0.12562127),("ing",0.12045586),("her",6.100071e-2),("hat",4.5886785e-2),("you",4.0859655e-2),("all",3.859826e-2),("his",3.8370494e-2),("for",3.819967e-2),("ith",3.6678515e-2),("ere",3.584066e-2),("tha",3.5401396e-2),("wit",3.4620486e-2),("ter",3.419749e-2),("ent",3.391278e-2)]
            ])
italian = ("Italian",
           [[("e",0.12748316),("a",0.11531494),("i",0.11437282),("o",0.10502163),("n",7.24209e-2),("r",7.02985e-2),("l",6.5541185e-2),("t",6.300434e-2),("s",5.846417e-2),("c",5.071938e-2),("d",3.9474607e-2),("u",3.4265496e-2),("m",3.0505177e-2),("p",2.8966492e-2),("v",2.4147304e-2)],
            [("er",9.195987e-2),("re",7.495379e-2),("on",7.484507e-2),("di",7.1165755e-2),("co",6.929463e-2),("to",6.821315e-2),("no",6.821315e-2),("an",6.500305e-2),("la",6.329215e-2),("ra",6.29946e-2),("ri",5.9790224e-2),("in",5.8399756e-2),("ch",5.7735987e-2),("en",5.7232447e-2),("te",5.6906283e-2)],
            [("che",0.12396106),("ell",9.129848e-2),("per",7.9760864e-2),("lla",7.877661e-2),("ent",7.350904e-2),("del",7.1376495e-2),("con",6.891587e-2),("gli",6.308326e-2),("non",5.6430444e-2),("ato",5.1873725e-2),("que",4.9941674e-2),("ere",4.8957422e-2),("are",4.804608e-2),("era",4.7717992e-2),("all",4.635098e-2)]
            ])
french  = ("French",
           [[("e",0.15805699),("a",9.301707e-2),("s",9.0873234e-2),("i",8.090436e-2),("t",7.716121e-2),("u",7.346523e-2),("r",7.30193e-2),("n",7.278777e-2),("l",6.2154308e-2),("o",5.8814198e-2),("m",3.944672e-2),("d",3.8602043e-2),("c",3.1776045e-2),("p",2.7702743e-2),("\233",2.2218794e-2)],
            [("ai",9.303814e-2),("es",9.0868674e-2),("le",8.606739e-2),("en",7.369076e-2),("re",6.899617e-2),("de",6.840935e-2),("ou",6.739575e-2),("on",6.261225e-2),("nt",6.1740905e-2),("an",5.718858e-2),("it",5.5961587e-2),("la",5.3898815e-2),("qu",5.3845465e-2),("is",5.3738773e-2),("ur",5.2547347e-2)],
            [("ait",0.11029944),("ent",0.10245109),("que",9.9432506e-2),("ais",8.868631e-2),("les",7.2204776e-2),("our",6.9005065e-2),("ant",6.369234e-2),("eur",5.910408e-2),("qui",5.1678337e-2),("lle",5.0833132e-2),("des",4.9021974e-2),("tai",4.8116393e-2),("ans",4.606375e-2),("mme",4.4856314e-2),("est",4.4554453e-2)]
            ])
german  = ("German",
           [[("e",0.19220483),("n",0.11152754),("r",8.666563e-2),("i",8.386764e-2),("s",6.824635e-2),("t",6.683186e-2),("h",6.0471836e-2),("a",6.029632e-2),("d",5.326519e-2),("u",4.4200093e-2),("l",3.8986113e-2),("c",3.7726495e-2),("g",3.758195e-2),("m",3.268804e-2),("o",2.5440091e-2)],
            [("en",0.13077414),("er",0.12911756),("ch",0.105543174),("te",7.87512e-2),("de",6.476585e-2),("ie",6.467028e-2),("ei",6.4064994e-2),("in",5.6705963e-2),("ge",4.9984075e-2),("nd",4.9633645e-2),("ic",4.59382e-2),("un",4.4950623e-2),("es",4.0044602e-2),("re",3.8356166e-2),("be",3.669959e-2)],
            [("ich",0.12687892),("ein",9.9057056e-2),("sch",8.607974e-2),("die",7.8330696e-2),("und",7.543647e-2),("der",7.076838e-2),("cht",7.002148e-2),("den",5.872468e-2),("ter",5.76977e-2),("ine",5.5176925e-2),("gen",5.3309686e-2),("ten",4.6680987e-2),("che",4.2479698e-2),("tte",4.0052287e-2),("nde",3.9305393e-2)]
            ])
            

supportedLanguages = [english, spanish, italian, french, german]
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


------------------------- DETECT ---------------------------------------
-- Esta funcion a veces flashea con las probabilidades y devuelve cosas
-- > 3, o sea que despues va a ser mas grande que > 1 y eso no tiene
-- sentido a la hora de expresar un porcentaje. Pasa porque el valor 
-- abs(d_oracion - d_idioma) no es siempre menor que d_idioma
-- donde d_ es la distribucion del ngram.                                                 
languageProbability :: [[(String, Float)]] -> Int -> String -> Float
languageProbability lang n phrase = sum [abs(fromJust((search c)) - (fromJust(lookup c freq))) | c <- (map fst freq)] + ((sum . (map snd)) (filter (\x -> (fst x) `notElem` (map fst freq)) grams))
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
