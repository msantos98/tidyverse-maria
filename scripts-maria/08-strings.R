library(tidyverse)
library(stringr)

s1 <- "Esto es un string"
s2 <- 'Esto es un string que contiene otro "string" dentro'

double_quote <- "\"" # '"' <- para guardar este carácter entre comillas
single_quote <- '\'' # "'" <- para guardar este carácter entre comillas
backslash <- "\\" # <- para guardar este carácter entre comillas

x <- c(single_quote, double_quote, backslash)
writeLines(x)

## Caracteres escapantes
#\\ -> backslash
#\n -> intro, salto de línea
#\t -> tabulador

mu <- "\u00b5" #\u es unicode

?'"' #todas los caracteres escapantes

c("uno", "dos", "tres") #vector de strings





# Funciones para trabajar con strings

### STR_LENGTH

str_length(c("x", "Juan Gabriel mola como profe", NA)) #número de caracteres





### STR_C

str_c("a", "b","c", sep = ", ") #concatenar strings

x <- c("abc", NA)
str_c("hola", str_replace_na(x), "adios", sep = " ") #stringuizar los NA

str_c("prefix-", c("a","b","c"),"-suffix")


name <- "Ricardo"
momento_del_dia <- "mañana"
birthday <- F

str_c(
  "Buena ", momento_del_dia," ", name,
  if(birthday) " y FELIZ CUMPLEAÑOS!!! =D",
  "."
)



str_c(c("a", "b", "c"), collapse = ",") #colapsar strings





### STR_SUB

x <- c("Manzanas", "Peras", "Limones", "Plátanos") 
str_sub(x, 1,3) #substraer strings
str_sub(x, -3,-1)
str_sub("x", 1,8)





### STR_TO_LOWER/UPPER/TITTLE

str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1)) #de mayúsculas a minúsculas
str_to_upper(x) #de minúsculas a mayúsculas
str_to_title(x) #solo la primera mayúscula

str_to_upper("i", locale = "tr") #ISO-639





### STR_SORT/ORDER

str_sort(x, locale = "es")
str_sort(c("apple", "banana",  "eggplant"), locale = "haw") #cambia en función de locale

str_order(x, locale = "haw")





# REGEXP- Expresiones Regulares
# - str_view()
# - str_view_all()

library(htmlwidgets)
x <- c("manzana", "banana", "pera", "pomelo")
str_view(x, "an")

#. -> cualquier caracter (pero solo uno)
str_view(x, ".a.")

#\. -> localizar un punto
dot <- "\\."
writeLines(dot)
str_view(c("abc","a.c","bc."), "a\\.c")

#\\ -> localizar un backslash
backslash <- "\\\\"
writeLines(backslash)
str_view("a\\b", "\\\\")


#^ -> inicio del string
#$ -> final del string
str_view(x, "^p")
str_view(x, "a$")

y <- c("tarta de manzana", "manzana", "manzana al horno", "pastel de manzana")
str_view(y, "^manzana$")

#\b -> localizar la frontera de una palabra
sum()
summarise()

#\d -> localizar cualquier dígito
#\s -> cualquier espacio en blanco (espacio, tabulador, salto de línea)
#[abc] -> localizar la a, la b o la c indistintamente
#[^abc] -> localizar cualquier cosa excepto la a, b o c

#abc|d..m, abc|xyz
str_view(c("grey", "gray"), "gr(e|a)y")

#? -> 0 o 1
#+ -> 1 o + veces
#* -> 0 o más veces

x <- "El año 1888 es el más largo en números romanos: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "C[LX]+")

#"colou?r"
#"ba(na)+"

#{n} -> exactamente n repeticiones 
#{n,} -> n o más repeticiones
#{,m} -> como máximo m repeticiones
#{n,m} -> entre n y m repeticiones
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+?")

fruits = c("banana", "coco", "papaya", "manzana", "pera", "pepino")
str_view(fruit, "(..)\\1", match = TRUE)
str_view("abc-def-", "(...)(-)\\1\\2")

# Otras herramientas
str_detect(fruits, "a")

sum(str_detect(words, "^j"))
mean(str_detect(words, "^[aeiou]"))
mean(str_detect(words, "[aeiou]$"))

f1 <- !str_detect(words, "[aeiou]")
f2 <- str_detect(words, "^[^aeiou]+$")
identical(f1, f2)

words[!str_detect(words, "[aeiou]")]
str_subset(words, "[aeiou]$")

df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% View()

df %>% filter(str_detect(words, "x$"))

str_count(fruits, "a")

mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababababa", "aba")
str_view("abababababa", "aba")
str_view_all("abababababa", "aba")





### STR_EXTRACT
head(sentences) #dataset
length(sentences)

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|") #combinamos los colores con una barra
color_match

has_color <- str_subset(sentences, color_match) #vemos en qué frases aparece algún color
has_color

matches <- str_extract(has_color, color_match) #buscamos los colores / solo se localiza la primera ocurrencia del color en cada clase
matches

more_than_one <- sentences[str_count(sentences, color_match)>1] #filtramos las frases que contienen más de un color
str_view_all(more_than_one, color_match) #vemos todas las ocurrencias
str_extract(more_than_one, color_match) #solo se nos muestra el primer color de las frases que tienen más de uno
str_extract_all(more_than_one, color_match) #muestra todas las ocurrencias por consola de las frases que tienen más de un color 
                                            #en una estructura compleja (lista de listas)
str_extract_all(more_than_one, color_match, simplify = TRUE) #muestra todas las ocurrencias por consola de las frases que tienen más de un color 
                                                             #en una estructura matricial



x <- c("x", "x y", "x y z")
str_extract_all(x, "[a-z]", simplify = TRUE) #[a-z] expr reg de la "a" a la "z"



noun <- "(a|the) ([^ ]+)" #loalizamos sustantivos
#Buscamos "a" o "the" seguido de un espacio seguido de un conjunto donde no haya espacio
#No es un filtro perfecto, también pilla adjetivos

sentences %>% #obtenemos todos los sustantivos del dataset sentences
  str_subset(noun) %>%
  str_extract(noun) %>% #da la coincidencia completa
  head(20)

sentences %>% 
  str_subset(noun) %>%
  str_match_all(noun) %>% #da la coincidencia completa y cada una de las palabras por separado en columnas diferentes
  head(20)



tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, 
    c("article", "noun"), 
    "(a|the) ([^ ]+)",
    remove = FALSE
  )





### STR_REPLACE / REPLACE_ALL

str_replace(fruits, "[aeiou]", "_") #cambiamos las vocales por "_" (solo la primera vocal de cada palabra)
str_replace_all(fruits, "[aeiou]", "_") #cambiamos todas las vocales por "_"

str_replace_all(c("1 coche", "2 teléfonos", "3 amigos"),
                c("1" = "un", "2" = "dos", "3" = "tres")
                )



#Intercambiar el orden de las segunda y tercera
sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(20)



#Dividimos frases por espacios en blanco
sentences %>%
  head(10) %>%
  str_split(" ") %>%
  .[[1]] #Nos quedamos solo con la primera frase --> pasamos a tener un vector

"a,b,c,d,e,f" %>%
  str_split(",") %>%
  .[[1]]



sentences %>%
  head(10) %>%
  str_split(" ", simplify = TRUE) #Nos genera matriz y rellena con espacios en blanco para completar

sentences %>%
  head(10) %>%
  str_split(" ", simplify = TRUE, n = 3) #Nos devuelve 3 elementos, separados por espacios



fields <- c("Name: Juan Gabriel", "Country: España", "Age: 30")
fields %>% str_split(": ", n = 2, simplify = TRUE)



## Parámetro boundary = c("character", "line_break", "sentence", "word")

sent <- "El perro de San Roque no tiene rabo. Y esto es todo."
str_view_all(sent, boundary("word")) #por palabra
str_view_all(sent, boundary("sentence")) #por frase
str_view_all(sent, boundary("character")) #por caracter
str_view_all(sent, boundary("line_break")) #por salto de línea

str_split(sent, " ") #rompemos por espacio en blanco. Problema: el punto queda como parte de la palabra
str_split(sent, boundary("word"))[[1]]





str_locate_all(sent, "[aeiou] ") #devuelve las posiciones de donde empieza 
                                 #y donde acaba la expresión regular cada vez que aparece
                                 #en este caso es vocal y un espacio
str_sub(sent, 8, 9) #Buscamos por posición indicando el principio y el final





## Parámetro regex
str_view(fruit, "nana")
str_view(fruit, regex("nana")) #Hace lo mismo que lo anterior



apples <- c("manzana", "Manzana", "MANZANA")
str_view(apples, "manzana")
str_view(apples, regex("manzana", ignore_case = TRUE)) #Ignora mayúsculas y minúsculas



x <- "Linea 1\nLinea 2\nLinea 3\nLinea 4"
str_extract_all(x, "^Linea")[[1]] #Buscamos cosas que empiecen por Linea -> solo aparece la primera
                                  #porque nos estamos refiriendo al principio del vector
str_extract_all(x, regex("^Linea", multiline = TRUE))[[1]] #Busca principios y finales de línea



phone <- regex("
               \\(?      #paréntesis de apertura opcionales
               (\\d{3})  #código de área - 971
               [)- ]?    #cierre de paréntesis, guión o espacio opcionales
               (\\d{3})  #tres dígitos de teléfono
               [ -]?     #espacio o guión opcional
               (\\d{3})  #tres dígitos finales
               ", comments = TRUE) #Ignoramos comentarios

str_match("971-123-456", phone)



library(microbenchmark)
#dotall = TRUE <-> permite que el punto '.' reemplace cualquier cosa (incluido \n)
microbenchmark::microbenchmark( #para ver eficiencias de los códigos
  regex = str_detect(sentences, "the"),
  fixed = str_detect(sentences, fixed("the")),
  collation = str_detect(sentences, coll("the")),
  times = 30 #número de evaluaciones
)

"\u00e1" == "a\u0301" #letra "a" con acento

str_detect("\u00e1", fixed("a\u0301")) #fixed dice que son diferentes

str_detect("\u00e1", coll("a\u0301")) #coll detecta como un humano normal y corriente
                                      #las igualdades dependen del idioma



turkish_i = c("I", "İ", "ı",	"i")
turkish_i
str_subset(turkish_i, coll("i", ignore_case = TRUE))
str_subset(turkish_i, coll("i", ignore_case = TRUE, locale = "tr"))



stringi::stri_locale_info()
stringi::stri_locale_info()$Name #Información de nuestro sistema



str_view_all("Esto es una frase.", boundary("word"))
str_extract_all("Esto es una frase.", boundary("word"))[[1]]





apropos("replace") #para buscar cosas de las que no nos acordamos
apropos("fruit")

getwd()
dir(pattern = "\\.Rmd$") #Para buscar todos los Rmd del directorio de trabajo
dir(pattern = "^.*\\.Rmd$")
glob2rx("*.Rmd") #Nos transforma a expresiones regulares

?stringi