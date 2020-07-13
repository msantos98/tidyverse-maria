library(tidyverse)
?magrittr



#Little bunny Foo Foo
#Hopping through the forest
#Scooping up the field mice
#And bopping them on the head

#Este código no funciona, solo es para ver intuitivamente como funciona todo
foo_foo <- little_bunny()

hop()
scoop()
bop()




#Contamos la historieta de 4 modos
## Variables intermedias
foo_foo1 <- hop(foo_foo, through = forest)
foo_foo2 <- scoop(foo_foo1, up = field_mice)
foo_foo3 <- bop(foo_foo2, on = head)


dd <- ggplot2::diamonds
dd1 <- dd %>%
  dplyr::mutate(price_per_carat = price / carat)

library(pryr)
pryr::object_size(dd) #Para saber la cantidad de memoria que ocupa un objeto
pryr::object_size(dd1)
pryr::object_size(dd,dd1)

dd$carat[1] <- NA #Si cambiamos algún dato que había en común, el tamaño aumenta ligeramente
pryr::object_size(dd)
pryr::object_size(dd1)
pryr::object_size(dd,dd1)



## Sobreescribir la variable original
foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo, on = head)



## Componer funciones
bop(
  scoop(
    hop(
      foo_foo, 
      through = forest
      ),
    up = field_mice),
  on = head
) #De la parte interna a la externa. Es un sandwich



## Usar una pipe
foo_foo %>%
  hop(through = forest) %>%
  scoop(up = field_mice) %>%
  bop(on = head)

#Funcionamiento de la pipe
my_own_pipe <- function(.){
  . <- hop(., through = forest)
  . <- scoop(., through = field_mice)
  . <-bop(., on = head)
  return(.)
}





#Casos en que las pipes no funcionarán: assign, get, load
assign("x", 3)
"x" %>% 
  assign(6) #No funciona

env <- environment()
"x" %>% 
  assign(6, envir = env) #Solución al problema anterior
x

#Con get y load pasa lo mismo


#tryCatch, try, supressMessages, supressWarnings
tryCatch(stop("!"), 
         error = function(e) "Me he encontrado un error")

stop("!") %>%
  tryCatch(error = function(e) "Me he encontrado un error") 
#No funciona, porque es función de evaluación tardía

#Con supressMessages y supressWarnings pasa lo mismo





# Pipes alternativas
library(magrittr)
rnorm(1000) %>%
  matrix(ncol = 2) %T>% #Tendrá dos salidas (izq y derecha)
  plot() %>% #Primera salida
  str() #Segunda salida



mtcars %$%
  cor(disp, mpg)



mtcars <- mtcars %>%
  filter(cyl == 6)

mtcars %<>%
  filter(cyl == 6)
