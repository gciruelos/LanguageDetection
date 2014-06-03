LanguageDetection
=================

Language detection in Haskell. Right now it is under development, so it will definitely fail.


How To Use
----------

    *Main> detect "What the actual fuck"
    "English"
    
    *Main> detectWithProb "Programar esta mierda me llevo horas"
    ("Spanish",0.7647936) --You can be 76.47% sure this is Spanish.
    
    *Main> detect "The longer the sentences are, the less prone it is to fail"
    "English"



Books used
----------

**Spanish**
* El ingenioso hidalgo don Quijote de la Mancha 
* Rayuela

**English**
* Ulysses
* Adventures of Huckleberry Finn

**Italian**
* Divina Comedia (http://www.gutenberg.org/cache/epub/1012/pg1012.txt)

Contribute
----------
You could contribute by adding new languages.
