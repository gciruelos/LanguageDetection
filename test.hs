import LanguageDetection
import Data.List (unfoldr)

full  = [("the man", "English"),
         ("el hombre", "Spanish"),
         ("l'uomo", "Italian"),
         ("l'homme", "French"),
         ("Der Mann","German"),
         ("M is the thirteenth letter and the tenth consonant of the basic Latin alphabet.", "English"),
         ("La M es la decimotercera letra y la décima consonante del alfabeto latino básico.", "Spanish"),
         ("La M è l'undicesima lettera dell'alfabeto italiano e la tredicesima dell'alfabeto latino moderno,", "Italian"),
         ("M est la treizième lettre et la dixième consonne de l'alphabet latin. ", "French"),
         ("M ist der 13. Buchstabe des lateinischen Alphabets und ein Konsonant.","German"),
         ("The Moon (Latin: Luna) is the Earth's only natural satellite. Although not the largest natural satellite in the Solar System, it is the largest relative to the size of the object it orbits (its primary) and, after Jupiter's satellite Io, it is the second most dense satellite among those whose densities are known.", "English"),
         ("La Luna es el único satélite natural de la Tierra. Con un diámetro de 3476 km es el quinto satélite más grande del Sistema Solar, mientras que en cuanto al tamaño proporcional respecto de su planeta es el satélite más grande: un cuarto del diámetro de la Tierra y 1/81 de su masa.", "Spanish" ),
         ("La Luna è l'unico satellite naturale della Terra. Il suo nome proprio viene talvolta utilizzato, per estensione e con l'iniziale minuscola (una luna), come sinonimo di \"satellite naturale\" anche per i satelliti di altri pianeti.", "Italiano"),
         ("La Lune est l'unique satellite naturel de la Terre. Suivant la désignation systématique des satellites, la Lune est appelée Terre ; cependant en pratique cette forme n'est pas utilisée. Elle est le cinquième plus grand satellite du système solaire, avec un diamètre de 3 474 km.","French"),
         ("Der Mond ist der einzige natürliche Satellit der Erde. Seit den Entdeckungen von Trabanten bei anderen Planeten des Sonnensystems, im übertragenen Sinn zumeist als Monde bezeichnet, wird er zur Vermeidung von Verwechslungen auch Erdmond genannt.","German"),
         ("Mars is the fourth planet from the Sun and the second smallest planet in the Solar System, after Mercury. Named after the Roman god of war, it is often described as the \"Red Planet\" because the iron oxide prevalent on its surface gives it a reddish appearance.Mars is a terrestrial planet with a thin atmosphere, having surface features reminiscent both of the impact craters of the Moon and the volcanoes, valleys, deserts, and polar ice caps of Earth. The rotational period and seasonal cycles of Mars are likewise similar to those of Earth, as is the tilt that produces the seasons. Mars is the site of Olympus Mons, the second highest known mountain within the Solar System (the tallest on a planet), and of Valles Marineris, one of the largest canyons.", "English"),
         ("Marte es el cuarto planeta del Sistema Solar más cercano al Sol. Llamado así por el dios de la guerra de la mitología romana Marte, recibe a veces el apodo de Planeta rojo debido a la apariencia rojiza que le confiere el óxido de hierro que domina su superficie. Tiene una atmósfera delgada formada por dióxido de carbono, y dos satélites: Fobos y Deimos. Forma parte de los llamados planetas telúricos (de naturaleza rocosa, como la Tierra) y es el planeta interior más alejado del Sol. Es, en muchos aspectos, el más parecido a la Tierra.Aunque en apariencia podría parecer un planeta muerto, no lo es. Sus campos de dunas siguen siendo mecidos por el viento marciano, sus casquetes polares cambian con las estaciones e incluso parece que hay algunos pequeños flujos estacionales de agua","Spanish"),
         ("Marte è il quarto pianeta del sistema solare in ordine di distanza dal Sole e l'ultimo dei pianeti di tipo terrestre dopo Mercurio, Venere e la Terra. Viene chiamato il Pianeta rosso a causa del suo colore caratteristico dovuto alle grandi quantità di ossido di ferro che lo ricoprono. Pur presentando un'atmosfera molto rarefatta e temperature medie superficiali piuttosto basse (tra −140 °C e 20 °C), il pianeta è il più simile alla Terra tra quelli del sistema solare. Nonostante le sue dimensioni siano intermedie fra quelle del nostro pianeta e della Luna (il raggio equatoriale è di 3397 km, circa la metà di quello della Terra e la massa poco più di un decimo), presenta inclinazione dell'asse di rotazione e durata del giorno simili a quelle terrestri.", "Italian"),
         ("Mars est la quatrième planète par ordre de distance croissante au Soleil et la deuxième par masse et par taille croissantes sur les huit planètes que compte le Système solaire. Son éloignement au Soleil est compris entre 1,381 et 1,666 UA (206,6 à 249,2 millions de km), avec une période orbitale de 686,71 jours terrestres. C’est une planète tellurique, comme le sont Mercure, Vénus et la Terre, environ dix fois moins massive que la Terre mais dix fois plus massive que la Lune. Sa topographie présente des analogies aussi bien avec la Lune, à travers ses cratères et ses bassins d'impact, qu'avec la Terre, avec des formations d'origine tectonique et climatique telles que des volcans, des rifts, des vallées, des mesas, des champs de dunes et des calottes polaires.","French"),
         ("Der Mars ist, von der Sonne aus gesehen, der vierte Planet im Sonnensystem und der äußere Nachbar der Erde. Er zählt zu den erdähnlichen (terrestrischen) Planeten. Sein Durchmesser ist mit knapp 6800 Kilometer etwa halb so groß wie der Erddurchmesser, sein Volumen beträgt gut ein Siebentel der Erde. Damit ist der Mars nach dem Merkur der zweitkleinste Planet des Sonnensystems. Mit einer durchschnittlichen Entfernung von 228 Millionen Kilometern ist er rund 1,5-mal so weit von der Sonne entfernt wie die Erde.Wegen seiner orange- bis blutroten Farbe wurde er nach dem römischen Kriegsgott Mars benannt und wird oft auch als der Rote Planet bezeichnet. Diese Färbung geht auf Eisenoxid-Staub (Rost) zurück, der sich auf der Oberfläche und in der dünnen CO2-Atmosphäre verteilt hat.","German")]

testDetection :: [(String, String)] -> Float
testDetection test = divFloat (length $ filter id (zipWith (==) (map (detect . fst) test) (map snd test))) (length test)

testDetailed :: [(String, String)] -> [Float]
testDetailed x = map testDetection (split x)
                 where split = splitEvery (length supportedLanguages)
                               where splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n) -- magia
                 
test2 :: [(String, String)] -> [String]
test2 = map (detect . fst)
