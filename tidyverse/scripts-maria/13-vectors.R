library(tidyverse)

#vector atómico: logical, integer, double (numeric), character, complex, raw -> homogéneo
#listas o vectores recursivos -> heterogéneos

# NA -> ausencia de un valor dentro de un vector
# NULL -> ausencia del vector, es como un vector de longitud 0

typeof(letters) # para ver el tipo de dato
typeof(1:10)

length(letters) # para ver la longitud

x <- list("a", "b", 1:10)
typeof(x)
length(x)



# vectores aumentados
# *factor: vector aumentado sobre vectore enteros
# *date y date-time: vector aumentado sobre vectores numéricos
# *data frame y tibble: vector aumentado sobre listas





# Vectores atómicos -------------------------------------------------------

# Lógicos: TRUE; FALSE; NA
1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA, FALSE)





# Numérico: integer, double
# Por defecto, los números son double
typeof(1)
typeof(1L) #para indicar que queremos que sea considerado integer
typeof(1.5L)


# *double: es una aproximación. Depende de la cantidad de memoria que se utiliza para su representación
x <- sqrt(2)^2
x-2
dplyr::near(x, 2) #para comparar números de tipo double

# *integer: tienen un valor especial, el NA
# *double tiene 4 valores especiales: NA, NaN, -Inf, Inf
x <- c(c(-1,0,1)/0, NA, 2)
x



# para comparar valores NA, Nan, Inf, -Inf
is.finite(x)
is.infinite(x)
is.na(x)
is.nan(x)





# Character
x <- "Dábale arroz a la zorra el abad"
pryr::object_size(x)
y <- rep(x, 10000)
pryr::object_size(y) 



# pointer -> son 8 bytes de información
(8 * 10000 + 152)/1000 #8 bites por 10000 punteros + 152 posiciones entre 1000 (ya que son kB), obtenemos el tamaño que se nos ha dado anteriormente
152*10000/1000/1024 #tamaño que ocuparía el vector y si se guardasen las 10000 copias y no se optimizara el espacio





# Complex
1 + 5i





# Tipos de NA
# Hay funciones que requieren saber el tipo de NA, aunque no suele ocurrir a menudo
NA            #lógico
NA_character_ #character
NA_complex_   #complejo
NA_integer_   #entero
NA_real_      #double





# Castings ----------------------------------------------------------------

# Casting directo o explícito
as.logical(c(1,0,0,0,1))
as.integer(c(T,F,F,F,T))
as.double(c(1,2,3))
as.character(c(1,2,3))





# Casting implícito
x <- sample(20, size = 100, replace = TRUE)
y <- x > 10
sum(y) # ¿cuántos elementos >10 hay en el array?
mean(y) # ¿qué proporción de elementos son > 10?



if(length(x)){ # si length(x) = 0 -> F, > 0 -> T //
  #hacer algo con el vector... 
}



# Jerarquía de los tipos de datos de un vector atómico
# Lógico < integer < double < character
typeof(c(TRUE, 1L))
typeof(c(1L, 1.6))
typeof(c(1.6, "a"))



# funciones is_estructura de vectores atómicos (del paquete purrr)
is_logical(c(T,T,T))            #lgl
is_integer(c(1L,2L,3L,4L))      #int
is_double(c(1,2,3,4))           #dbl
is_numeric(c(1,2,3,4L))         #int, dbl
is_character(c("a","b", "c"))   #chr
is_atomic(c(T,T, 1,2L, "a"))    #lgl, int, dbl, chr



# funciones is_estructura de vectores aumentados
is_list(list(1,2,3))            #list



# funciones is_estructura de vectores en general
is_vector(c(T,T))               #lgl, int, dbl, chr, list



# funciones is_scalar_estructura
# todas las estructuras tienen su versión
# es para comprobar que además de la estructura respectiva, el vector tiene longitud 1
is_scalar_integer(c(4L))





# Recycling rule ----------------------------------------------------------

# permite crear solo a partir de un valor su versión vectorizada repetida o reciclada de 
# la misma longitud del vector de mayor tamaño con el que se está comparando
# Para R, cualquier escalar es un vector de longitud 1

sample(10) + 12
rep(12,10)



runif(10) > 0.5



1:10 + 1:2
1:10 + 1:3 #R nos avisa si el vector de longitud mayor NO es múltiplo que el de longitud menor



#Tidyverse pone pegas, aún incluso cuando el mayor es múltiplo del menor
tibble(
  x = 1:4,
  y = 1:2
)

# Pero con un escalar no pone ninguna pega
tibble(
  x = 1:4,
  y = 1
)

# Si queremos hacer la primera, hay que hacerlo de forma explícita:
# y = (1,2,1,2)
tibble(
  x = 1:4,
  y = rep(1:2,2)
)

# y = (1,1,2,2) <- Este es el efecto del parámetro each
tibble(
  x = 1:4,
  y = rep(1:2, each = 2)
)



# Se pueden nombrar los elementos de un vector
c(x = 4, y = 3, z = -1)

set_names(1:3, c("x", "y", "z")) # Función de purrr





# Subsetting --------------------------------------------------------------
# Subconjuntos de vectores, con la sintaxis de corchete []

x <- letters
x[c(3,2,6)] #Subconjunto con vector de índices
x[c(1,1,7,7,7,3,3,3)] #Subconjunto con vector de índices
x[c(-3,-5,-7)] #Subconjunto eliminando con vector de índices
x[-c(6:15)] #Subconjunto eliminando con vector de índices
x[c(4,-3)] #Es imposible mezclar
x[0] #Dato curioso



x <- c(4,5,8,NA,2,1,3,NA)
x[!is.na(x)] #Para quedarnos con todos los elementos que NO son NA
x[x %% 2 == 0] #Para quedarnos con todos los números pares



x <- c(abc = 1, def = 2, ghi = 3)
x[c("def", "abc", "abc")] #Subconjunto por nombre de posición
x["abc"] #Devuelve un elemento con nombre y su valor
x[["abc"]] #Devuelve un elemento solamente con su valor



x <- matrix(1:9, byrow = T, ncol = 3)
x[1,]
x[,1]
x[-2,]
x[,-1]





# Lists -------------------------------------------------------------------

x <- list(1,2,3)
x
str(x) #Para mostrar la estructura, pero no el contenido



# Dando nombre a cada variable de la lista
x <- list(x = 1, y = 2, z = 3)
str(x)



# El contenido es heterogéneo
x <- list("x", 1L, pi, TRUE)
str(x)



# Una lista puede contener otra lista
x <- list(list(1,2,3), list(pi,sqrt(2)), list("a", "b", 0))
str(x)

x1 <- list(c(1,2),c(3,4))
x1

x2 <- list(list(1,2), list(3,4))
x2

x3 <- list(1, list(2, list(3)))
x3

x <- list(
  a = 1:3, 
  b = "soy un string",
  c = pi, 
  d = list(-2,-5)
)

x

x[1:2] #Extraemos una sublista, que siempre tendrá estructura de lista
str(x[1:2])
x[4]
str(x[4])
x[c("a", "c")]
x[[1]] #Extraemos solamente los objetos de la posición 1 de la lista, ya no tiene estructura de lista
str(x[[1]])
str(x[[4]])
x[["a"]]
x$a
x[[1]][[1]] 
x[[1]][1] #Me da lo mismo que el anterior porque se trata de un vector
x[[4]][[1]]





# Attributes --------------------------------------------------------------

x <- 1:12
attr(x, "desc") <- "Vector de las horas del día" # Asignamos el atributo descripción al vector x
attr(x, "desc")
attr(x, "created") <- "Juan Gabriel Gomila"
attr(x, "source") <- "Curso Tidyverse"
attributes(x) # Buscar todos los atributos de un vector



as.Date
#UseMethod es un indicador de que se trata de una función genérica
methods("as.Date") #Listar todos los métodos de una función genérica
getS3method("as.Date", "character") #Para obtener de una función genérica la implementación especial cuando el parámetro
                                    # que se le pasa es un caracter
getS3method("as.Date", "numeric")



methods("print")





# Augmented Vectors -------------------------------------------------------

# factor
x <- factor(c("L", "M", "J", "S", "D"), 
            levels = c("L", "M", "X", "J", "V", "S", "D") )
typeof(x)
attributes(x)





# date & date-time
x <- as.Date("1988-05-19")
typeof(x)
attributes(x)
unclass(x) #Transforma la fecha a su valor asociado a números double



#POSIXct -> número de segundos desde el epoch
# POSIXct (Portable Operating System Interface calendar time)

x <- lubridate::ymd_hm("1988-05-19 16:30")
x
typeof(x)
attributes(x)
unclass(x)
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x



#POSIXlt -> POSIX en formato lista
# Se ha quedado un poco en el pasado gracias al paquete lubridate
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

attr(y, "names")

z <- lubridate::as_datetime(y) #Pasamos de nuevo a POSIXct
typeof(z)





#Tibble -> Clase superior de listas aumentadas
# Las variables tienen que tener la misma longitud
tb <- tibble(x = 1:5, y = 6:10)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 6:10)
typeof(df)
attributes(df)
