#Utilidades del uso de funciones: 
# *nombre a la función 
# *cambios en un único lugar
# *eliminais probabilidad de error del C&P

library(magrittr)
library(tibble)



df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

df %>% View()

#Si quisiésemos normalizar las variables, podríamos ir 1 a 1...

df$a <- (df$a - min(df$a, na.rm = TRUE))/(max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE))/(max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE))/(max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE))/(max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
df$e <- (df$e - min(df$e, na.rm = TRUE))/(max(df$e, na.rm = TRUE) - min(df$e, na.rm = TRUE))

#En resumen, es un coñazo

View(df)



#Primera pregunta: ¿Cuántos datos de entrada tenemos?
#En nuestro caso, solamente 1 dato de entrada: df$a

#Segunda pregunta: ¿Qué es lo que hace la función?

x <- df$a
rng <- range(x, na.rm = TRUE)
(x - rng[1])/(rng[2] - rng[1])


# Calcular rango de los datos excluyendo NAs e Infinitos ------------------

rescale_0_1 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE) #Evitamos problemas con NA's y con el infinito
  (x - rng[1])/(rng[2] - rng[1])
}

#Probamos nuestra función
rescale_0_1(c(0,5,10))
rescale_0_1(c(-1,0,1))
rescale_0_1(c(1,2,NA,4,5))

#El código queda mucho más limpio
df$a <- rescale_0_1(df$a)
df$b <- rescale_0_1(df$b)
df$c <- rescale_0_1(df$c)
df$d <- rescale_0_1(df$d)
df$e <- rescale_0_1(df$e)

x <- c(1:10, Inf)
rescale_0_1(x)





#El nombre de la función es importante. Prevalece la claridad a que sea corto
l() #Esto no tiene sentido, l de qué?
my_fucking_awesome_function() #Esto no es descriptivo



#Snake case
impute_missing() #Perfecto
count_days() #Perfecto
collapse_hours() #Perfecto

#Camel case
imputeMissing() #Perfecto
countDays() #Perfecto
collapseHours() #Perfecto

#En R hay de todo... no se puede hacer nada
col_number()
colMeans()



#Conexión por prefijos, para el autocompletar
input_select()
input_checkbox()
input_text()

#Esto no tiene sentido... no es consistente
select_input()
checkbox_input()
text_input()

#Esto R lo hace bien el str
stringr::str_



# PROHIBIDO HACER ESTO
#T <- FALSE
#c <- 5
#mean <- function(x) median(x)





# Puede ser útil comentar el código e indicar qué se hace en cada momento
# Load data --------------------------------------------- 

read_csv("data/cars.csv")

# Plot data =============================================


# Command+Shift+R <- crea nuevas funciones
# Nueva sección -----------------------------------------------------------

# Moraleja: ABUSAR DE LOS COMENTARIOS





# Condicionales -----------------------------------------------------------

if(condition){
  # código a ejecutar si la condición es TRUE
} else {
  # código a ejecutar si la condición es FALSE
}

?`if`



has_name <- function(x){
  nms <- names(x)
  if(is.null(nms)){
    #no existe el objeto en cuestión
    rep(FALSE, length(x))
  } else {
    # ausencia de NAs y de ""
    !is.na(nms) & nms != ""
  }
}

has_name(c(1,2,3))
has_name(mtcars)
has_name(tribble(
  ~x, ~y, ~` `,
   1,  2,   3
))





if(c(T,F)){} #Devuelve warning ya que la condición es un vector booleano

# Para solucionar lo anterior, podemos utilizar any() o bien all()
if(any(c(T,F))){
  "tenemos almenos un verdadero"
} else{
  "no hay ningún verdadero"
}
if(all(c(T,F))){
  "tenemos todas las condiciones verdaderas"
} else{
  "tenemos alguna condición falsa"
}

if(NA){} #Devuelve error ya que la condición es NA



# Operadores And y Or
# && -> AND
# || -> OR





# La función identical es muy estricta: entero no es lo mismo que double
identical(0, 0L)

2 == sqrt(2)^2

sqrt(2)^2-2

dplyr::near(2, sqrt(2)^2)

2 == NA #Cuidado al comparar con NAs





# Concatenación de if's
if(condicion) {
  # resultado 1
} else if(condicion2) {
  # resultado 2
} else if(condicion3) {
  # resultado 3
} else {
  # resultado por defecto
}





# SWITCH: para recortar if's anidados
calculate <- function(x,y,op) {
  switch (op, #En función de op, hace lo siguiente
    suma = x+y,
    resta = x-y,
    multiplicacion = x*y,
    division = x/y,
    stop("ERROR: no se puede ejecutar la operación indicada")
  )
}

calculate(2,3,"suma")
calculate(2,3,"resta")
calculate(2,3,"multiplicacion")
calculate(2,3,"division")
calculate(2,3,"antonio")





# Escritura correcta de if's: indentación, posición llaves...
x <- 5
y <- -7

if(y < 0 && length("Hace sol")>0) {
  message("y es negativo, con este sol que hace")
}

if(y <= 0) {
  log(x)
} else {
  y^x
}

# Cuando los resultados son muy cortos:
y <- 10
x <- if(y<20) "Número pequeño" else "Número grande"





# Argumentos --------------------------------------------------------------
# Hay 2 tipos:
# *Dato
# *Detalle del cálculo

# Ejemplos:
log(x = 8, base = 2)

mean(x = c(1,2,3,NA,4,5,10), trim = 2, na.rm = T)

t.test(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1),
       alternative = "greater", paired = T, var.equal = T, conf.level = 0.99)

stringr::str_c(c("banana", "manzana", "pera"),  collapse = ", ")



#Intervalo de confianza de la normal estándar
# Especificando valor por defecto
standar_ci <- function(x, conf = 0.95){
  se <- sd(x)/sqrt(length(x))
  alpha <- (1-conf)
  mean(x) + se * qnorm(c(alpha/2, 1-alpha/2))
}

x <- runif(1000)
standar_ci(x)
standar_ci(x, conf = 0.99)
standar_ci(x, conf = 0.999)



# Funcionan igual, pero el primero es mejor
mean(1:10, na.rm = TRUE)
mean(1:10, n=T)

# Los espacios facilitan la lectura
avg <- mean(120 / 12 + 32, na.rm = TRUE)
avg<-mean(120/12+32,na.rm=TRUE)





# Nomenclatura estándar
# x, y, z: vectores
# w: vector de pesos
# df, data, d : data frame
# i, j, k : subíndices numéricos (filas y columnas)
# n : longitud de un vector, o número de filas
# m : número de columnas
# p : probabilidades





# Media ponderada
wt_mean <- function(x, w, na.rm = TRUE){
  #Como es la función de la que muchas hacen uso, comprobamos que todo es correcto
  stopifnot(is.logical(na.rm), 
            length(na.rm) == 1,
            length(x) == length(w)
            ) 
  
  if(na.rm) { #Si falta un dato o un peso
    missing <- is.na(x) | is.na(w)
    x <- x[!missing]
    w <- w[!missing]
  }
  
  sum(x*w) / sum(w)
}

# Varianza ponderada
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w*(x-mu)^2)/sum(w)
}

# Desviación típica ponderada
wt_sd <- function(x, w){
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 6:1, na.rm = T)





#Los 3 puntitos ...
sum(1:10)
str_c(c("a", "b", "c", "d", "e"))

# Generalizamos una función ya existente
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

# Qué función tan chachi! Genera título
rule <- function(..., pad = "=") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(stringr::str_dup(pad, width/2), " ", title, " ", stringr::str_dup(pad, width/2), "\n", sep = "")
}

rule("Sección número 1 ", "Informe del jefe")

# El parámetro mal escrito se lo calla
sum(c(1,2), na.mr = T)
list(...) #Por si queremos los valores exactos de los parámetros de ...





# Lo que puede salir de las funciones
# El uso de return()

my_function <- function(x,y,z) {
  if(length(x) == 0 || length(y) == 0){
    return(0)
  }
  ## el código sigue más adelante con otras funciones...
}



my_function <- function(x){
  if(x){
    #Aquí
    #va
    #un
    #código
    #muy
    #largo
    #y 
    #complejo
  }else {
    return(0)
  }
}

#Forma de hacer el código fácil de leer
my_function <- function(x){
  if(!x){
    return(0)
  }
  
    #Aquí
    #va
    #un
    #código
    #muy
    #largo
    #y 
    #complejo
}


#Dos tipos básicos de funciones pipeables:
# *transformación: el objeto de entrada es modificado antes de ser devuelto
# *efecto secundario: el objeto de entrada no es modificado (plot, write...)

show_nas <- function(df){
  n <- sum(is.na(df))
  cat("Número de NAs: ", n, "\n", sep = "")
  
  invisible(df) #Copia invisible del data frame
}

x <- show_nas(diamonds)

class(x)

dim(x)

mtcars %>%
  show_nas() %>%
  mutate(mpg = ifelse(mpg>20, NA, mpg)) %>%
  show_nas()





# OJO CON ESTO, que en R funciona
f <- function(x){
  x + y
}

y <- 5
f(3)

y <- 30
f(3)



`+` <- function(x,y){
  if(runif(1) < 0.1){
    sum(x,y)
  } else{
    sum(x,y)*1.5
  }
}

table(replicate(1000, 2+3))

rm(`+`) #Eliminemos la aberración... :O