library(tidyverse)

#Un bucle for suele tener 3 componentes:
# *variable de salida o variable resultado, que se inicializa antes del bucle
# *secuencia, que determina cuantas veces se tiene que ejecutar el bucle
# *cuerpo, lo que está entre las llaves de apertura y cierre

#Un bucle while suele tener 2 componentes:
# *condición, el bucle para cuando la condición es falsa
# *cuerpo, lo que está entre las llaves de apertura y cierre

t <- tibble(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100),
  e = rnorm(100)
)

t %>% 
  View()

#Incumple nuestra regla de copia-pega o repetir lo mismo más de 2 veces
median(t$a)
median(t$b)
median(t$c)
median(t$d)
median(t$e)

#Creamos un vector que contine las medianas anteriores
output <- vector("double", ncol(t)) #La función vector es muy útil a la hora de inicializar vectores vacíos
output #1 - output
seq_along(t) #La función seq_along es muy útil para las secuencias de un bucle. Es equivalente a hacer 1:ncol(t)
#En el caso de tener un vector de longitud 0, seq_along sabe qué hacer 
for(i in seq_along(t)){  # 2 - sequence
  output[[i]] <- median(t[[i]]) # 3 - body
}
output

output[[2]] #La estructura de doble corchete nos devuelve la columna introducida

#Diferencias entre seq_along y : cuando el vector es de longitud 0
y <- vector("double",0)
seq_along(y)
1:length(y)





# Bucle for advanced ------------------------------------------------------

# Modificación

df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

rescale_0_1 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1])/(rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <- rescale_0_1(df[[i]])
}

df



# Patrones de bucle
# *Por posición
# for (i in seq_along(df)) -> df[[i]] 
#Esta es la forma más sencilla, a través de los índices

# *Por elementos
# for(x in df) -> x

# *Por nombre
# for(name in names(df)) -> df[[name]]

df #Objeto de entrada
results <- vector("list", length(df)) #Objeto de salida
names(results) <- names(df) #Necesitamos que los nombres de los objetos de entrada y salida se correspondan
results

for(i in seq_along(df)){
  name <- names(df)[[i]]
  value <- df[[i]]
  print(paste0(name, " - ", value))
}



# Desconocimiento de la longitud de output

means <- 0:100000
#output <- double() <- Esto es lo que no se debe hacer si queremos optimizar las cosas
output <- vector("list", length(means)) #Esto es lo que hay que hacer

for(i in seq_along(means)){
  n <- sample(100, 1)
  #output <- c(output, rnorm(n, means[[i]])) <- Esto es lo que no se debe hacer si queremos optimizar las cosas
  output[[i]] <- rnorm(n, means[[i]]) #Esto es lo que hay que hacer
}  

#Todas las funciones mostradas a continuación vienen a hacer algo similar con el conjunto de salida,
# algunas son más eficientes que otras
str(output)
str(unlist(output)) #unlist combina la lista de datos en solo un objeto, un vector
str(purrr::flatten_dbl(output)) #flatten_dbl aplanará la lista de datos solamente si hay doubles
#En general, flatten_ solo aplanará si los datos son homogéneos
paste(output, collapse = "") #Esto no conviene ejecutarlo, R peta.
#rbind()
#cbind()
dplyr::bind_rows()
dplyr::bind_cols()



# Desconocimiento de la longitud de la iteración
while(condition){ # condition es una condición booleana
  #aquí va el cuerpo del bucle
  #dentro del bucle, en algun momento condition = FALSE
}



# Podemos pasar de un bucle for a un bucle while
for (i in seq_along(df)){
  #body
  print(names(df)[[i]])
}

i <- 1
while(i <= length(df)){
  #body
  print(names(df)[[i]])
  i <- i+1
}



# El ejemplo de la moneda: es una simulación
flip_coin <- function() sample(c("C", "X"), 1)

count_heads <- function(total_heads = 5) {
  flips <- 0
  n_heads <- 0 
  
  while (n_heads < total_heads) {
    if(flip_coin() == "C"){
      n_heads <- n_heads + 1
    } else {
      n_heads <- 0
    }
    flips <- flips + 1
  }
  
  flips
}

count_heads(8)





# Programación funcional --------------------------------------------------

#R es funcional

df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

col_means <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- mean(df[[i]])
  }
  output
}

col_medians <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- median(df[[i]])
  }
  output
}

col_sd <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- sd(df[[i]])
  }
  output
}

#Con lo anterior hemos hecho copiar-pegar. Oh-Oh. Vamos a arreglarlo:

#Cambiemos a un ejemplo más sencillo y luego volvemos al anterior
f1 <- function(df) abs(df - mean(df)) ^ 1
f2 <- function(df) abs(df - mean(df)) ^ 2
f3 <- function(df) abs(df - mean(df)) ^ 3

#La siguiente función generaliza f1, f2 y f3 y además calcula para cualquier número que introduzcamos
f <- function(df, i = 1) abs(df - mean(df)) ^ i



#Volvemos al ejemplo de medias, medianas y sd
col_summary <- function(df, fun){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- fun(df[[i]])
  }
  output
}

col_summary(df, mean)
col_summary(df, sd)
col_summary(df, min)
col_summary(df, max)
col_summary(df, median)



# Paquete base de R
# funciones análogas a las del paquete purrr
apply()
lapply()
sapply()
tapply()





# purrr -------------------------------------------------------------------

#El objetivo de purrr es crear funciones que en lugar de bucles nos permitan dividir el problema de manipulación de listas a
# problemas o fragmentos totalmente independientes el uno del otro

# PASO 1: un elemento de la lista -> (purrr) -> todos los elementos de la lista
# PASO 2: resolver pequeños problemas que se unan en conjunto con una pipe %>%

?map() # crea una lista
map_lgl() # crea un vector lógico
map_int() # crea un vector de enteros
map_dbl() # crea un vector de doubles
map_chr() # crea un vector de caracteres



map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)



df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

df %>% map_dbl(mean, trim = 0.5) # quitamos el 50% de los datos más grandes y más pequeños
# Las funciones map_ preservan el nombre de las variables



# Las funciones map_ también funcionan con listas
z <- list(x = 1:5, y = 6:10)
map_int(z, length)



models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg~wt, data = .))

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared) #~. equivale a decir "del objeto anterior"

# Esta secuencia hace exactamente lo mismo que la anterior y es más sencilla
models %>%
  map(summary) %>%
  map_dbl("r.squared")





x <- list(list(1,2,3), list(4,5,6), list(7,8,9))
x %>% map_dbl(2) #Devuelve los elementos en las segundas posiciones

# map() <-> lapply()
# sapply() puede dar problemas si no conocemos el tipo del objeto de salida. Es inconsistente

x1 <- list(
  runif(5),
  runif(5),
  runif(5)
)

x2 <- list(
  runif(5)/2,
  runif(5)/2,
  runif(5)
)

x3 <- list(
  0.8,
  0.9,
  0.85
)

x1
x2
x3

threshold <- function(x, cutoff = 0.75) x[x>cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()
x3 %>% sapply(threshold) %>% str()

# En general es mejor map_ que funciones de la familia apply, pero vapply puede producir matrices mientras
# que map_ solamente produce vectores

vapply(df, is.numeric, logical(1))
map_lgl(df, is.numeric)





# safely siempre da resultado y error
safe_log <- safely(log)
str(safe_log(12)) # no hay error. Resultado es correcto y en error aparece NULL
str(safe_log("antonio")) # hay error. En resultado aparece NULL y se nos muestra el error



# La función safely está pensada para utilizarla con las funciones de la familia map_
x <- list(1, 10, "z", -8)
x %>% 
  map(safe_log) %>% # Aplicamos a todos los elementos de la lista la función safe_log
  transpose() %>% 
  str()



# Si queremos solamente ver los errores o los resultados
x %>% 
  map(safe_log) %>% 
  transpose() -> y

y$error %>% 
  map_lgl(is_null) -> is_ok # Buscamos aquellos errores en los que ha habido NULL

x[!is_ok] # Mostramos los elementos donde ha habido errores

y$result[is_ok] %>% # Hacemos lo mismo que antes, pero con los resultados
  flatten_dbl() # Aplanamos la lista para visualizar mejor





# possibly es una versión simplificada de safely, siempre da éxito
x %>%
  map_dbl(possibly(log, NA_real_)) # Le suministramos valor por defecto en caso de fallo





# quietly captura la salida, los mensajes y los warnings
# se supone que para esta función no debe haber errores
list(5,-5) %>% 
  map(quietly(log)) %>% 
  str()





# Multiple mappings -------------------------------------------------------

# Simulando varias distribuciones normales con diferentes medias y desviaciones típicas
mu <- list(2, 17, -5)

# Las siguientes instrucciones solo simulan normales cambiando las medias
mu %>%
  map(rnorm, n = 10) %>%
  str()



sigma <- list(1, 5, 25)

# Primera alternativa para iterar múltiples argumentos
seq_along(mu) %>% # . valdrá 1, 2 y 3 respectivamente
  map(~rnorm(10, mu[[.]], sigma[[.]])) %>%
  str()

# Esto hace exactamente lo anterior
# Para combinaciones de cada dato del vector mu, con su correspondiente del vector sigma, se llama a
#   la función rnorm con las parejas de parámetros como entrada
map2(mu, sigma, rnorm, n = 10) %>% 
  str()



# Esquema de la estructura de la función map2
map2 <- function(x, y, f, ...){
  out <- vector("list", length(x))
  for(i in seq_along(x)){
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
}





# La función pmap es de purrr y sirve para introducir más de 2 argumentos a la vez
n <- list(10, 15, 22)

args <- list(n, mu, sigma) # Lista de los argumentos en orden

args %>%
  pmap(rnorm) %>% 
  str()

# En caso de no dar nombre a los argumentos, pmap lo hace por posición como en el caso anterior.
#   Si quisiéramos hacerlo bien, bien, bien, haríamos lo siguiente

args_ok <- list(mean = mu, sd = sigma, n = n) 

args_ok %>% 
  pmap(rnorm) %>% 
  str()

# También lo podemos hacer con tibbles o tribbles
param <- tribble(
  ~mean, ~sd, ~n,
  2,     1,   10,
  17,    5,   15,
  -5,    25,  22
)

param %>% 
  pmap(rnorm)



# También podemos introducir diferentes funciones. Utilizaremos invoke_map
f <- c("runif", "rnorm", "rpois") # Diferentes funciones
param <- list( # Diferentes parámetros
  list(min = -5, max = 5),
  list(sd = 3),
  list(lambda = 12)
)

invoke_map(f, param, n = 15) %>% 
  str()

# De nuevo, podemos hacerlo todo con tribble o tibble
matchs <- tribble(
  ~f,      ~params,
  "runif", list(min = -5, max = 5),
  "rnorm", list(sd = 3), 
  "rpois", list(lambda = 12)
)

matchs %>% 
  mutate(sim = invoke_map(f, params, n = 15)) %>% # Añadimos a la propia tribble la simulación
  View()





# Alternativas a map ------------------------------------------------------

# *WALK
x <- list("hola", 123, -pi)
x %>% 
  walk(print)



# *PWALK
# Lo que hacemos a continuación es dibujar plots y guardarlos en pdf
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + 
        geom_point())

paths <- str_c(names(plots), ".pdf") # Creamos los paths de cada plot

pwalk(list(paths, plots), ggsave, path = "data/")



# *KEEP
iris %>%
  keep(is.factor) %>%
  str()



# *DISCARD
iris %>%
  discard(is.factor) %>%
  str()



# *SOME y EVERY
x <- list(1:5, letters, list(16))

x %>% 
  some(is_character) # Nos devuelve si alguno de los elementos en character
x %>% 
  every(is_vector) # Nos devuelve si todos los elementos son vectores



# *DETECT
x <- sample(12)

x %>% 
  detect(~.<7) # Para que devuelva el primer valor menor que 7
x %>% 
  detect_index(~.<7) # Para que devuelva la posición del primer valor menor que 7



# *HEAD_WHILE y TAIL_WHILE
x %>% 
  head_while(~.>7)
x %>% 
  tail_while(~.<7)



# *REDUCE
dfs <- list(
  age = tibble(name = "Juan Gabriel", age = 30),
  sex = tibble(name = c("Juan Gabriel", "María"), sex = c("M", "F")),
  trt = tibble(name = "María", treatment = "Mrs")
) 

dfs %>% 
  reduce(full_join)

# Ver la intersección común
vs <- list(
  1:5,
  c(1,3,5,6,8,10),
  c(1,2,3,7,8,10),
  c(1,2,4,6,9,10)
)

vs %>% 
  reduce(intersect)



# *ACCUMULATE
x <- sample(12)
x %>% 
  accumulate(`+`)
