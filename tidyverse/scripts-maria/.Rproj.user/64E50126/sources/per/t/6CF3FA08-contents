# Família de modelos que expresa el patrón / relación a estudiar
# y = a_1 * x + a_0 -> relación lineal
# y = a_2 * x^2 + a_1 * x + a_0 -> relación cuadrática

# Ajustar el modelo: buscar los parámetros del modelo que hayamos decidido
# y = 6x-8
# y = 2x^2-5x+8

# Ajustar un modelo no implica obtener el mejor modelo, sino que dentro de los de la familia, es el que mejor explica nuestros datos
# Todos los modelos están mal, pero algunos son útiles.

# El objetivo del modelo es descubrir una aproximación de los datos lo suficientemente buena para que nos sea útil para trabajar

library(tidyverse)
library(gridExtra)
library(modelr) # Sirve para modelizar
options(na.action = na.warn) # Lo configuramos para que los NA no pasen desapercibidos, sino que nos salte una advertencia





# Linear models -------------------------------------------------------

sim1 %>% # dataset que viene con el paquete modelr
  View()

sim1 %>% 
  ggplot(aes(x,y)) +
  geom_point()

# Una vez representados los datos gráficamente, vemos que puede existir una aproximación lineal
# y = a_0  + a_1 * x 

# Generamos una serie de modelos a partir de una tibble
models <- tibble(
  a0 = runif(300, -20, 40), # valor del término independiente
  a1 = runif(300, -5,   5) # valor de la pendiente
)

# Visualizamos el gráfico anterior junto con las 300 rectas generadas en la tibble
sim1 %>% 
  ggplot(aes(x,y)) +
  geom_abline(aes(intercept = a0, slope = a1), 
              data = models, 
              alpha = 0.2) +
  geom_point()
# Muchas de las rectas representadas son terriblemente malas, pero hay algunas que se adaptan bien
# Ir probando a cascoporro no es una estrategia óptima

# A la hora de trabajar con una relación lineal, lo que queremos es minimizar la distancia vertical de los puntos a la recta (el error)
# Error = Valor real - Predicción

# Definimos el modelo lineal
model1 <- function(a0, a1, data){
  a0 + data$x * a1
}

# Ejemplo particular de predicción
model1(3, 1.2, sim1)

# Definimos el cálculo de la raíz del error cuadrático medio
rmse <- function(mod, data) {
  diff <- data$y - model1(mod[1], mod[2], data)
  sqrt(mean(diff^2))
}

# Calculamos el error del modelo anterior
rmse(c(3,1.2), sim1)



# Utilicemos purrr
sim1_rmse <- function(a0, a1){
  rmse(c(a0, a1), sim1)
}

# Sobreescribimos models para añadir el rmse a cada modelo simulado
models <- models %>%
  mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse))

models %>% 
  View()

sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") + 
  geom_abline(aes(intercept = a0, slope = a1, color = -rmse), # Color inverso al rmse: cuanto más claro, mejor la aproximación (menor rmse)
              data = filter(models, rank(rmse) <= 10)) # Nos quedamos con el top10 de simulaciones



# Nos quedamos con el top10 y vemos que las pendientes con menos rmse son cercanas a 2
models %>%
  filter(rank(rmse) <= 10)





# Dibujamos un scatterplot con todos los modelos simulados
models %>%
  ggplot(aes(a0, a1))+
  geom_point(data = filter(models, rank(rmse) <= 10), size = 4, color = "red") + # Nos quedamos con el top10
  geom_point(aes(color = -rmse)) # Cuanto más claro, mejor el modelo
# Este camino es dificli





# Alternativa más sistemática: creamos una parrilla donde se harán todas las combinaciones posibles de a0 y a1
grid <- expand.grid(
  a0 = seq(-5,20, length = 25), # Acotamos el eje horizontal en base a la visualización anterior
  a1 = seq(0, 4, length = 25) # Acotamos el eje vertical en base a la visualización anterior
) %>%
  mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse)) # Añadimos a la parrilla una columna, rmse, con el rmse correspondiente calculado con sim1_rmse

# Dibujamos el grid
grid %>%
  ggplot(aes(a0, a1)) +
  geom_point(data = filter(grid, rank(rmse)<=10), size = 4, color = "red") + #Top10
  geom_point(aes(color = -rmse))
# Con esto hemos acotado bastante la información
# Podríamos repetir todo este proceso acotando más los valores de a0 y a1

# Mostramos los modelos del top10 de la parrilla anterior
sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a0, slope = a1, color = -rmse),
    data = filter(grid, rank(rmse)<=10) # Top10
  )

# Acotamos la parrilla
grid <- expand.grid(
  a0 = seq(3,5, length = 25), # Acotamos el eje horizontal en base a la visualización anterior
  a1 = seq(1.9, 2.2, length = 25) # Acotamos el eje vertical en base a la visualización anterior
) %>%
  mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse)) # Añadimos a la parrilla una columna, rmse, con el rmse correspondiente calculado con sim1_rmse

# Dibujamos el grid
grid %>%
  ggplot(aes(a0, a1)) +
  geom_point(data = filter(grid, rank(rmse)<=10), size = 4, color = "red") + #Top10
  geom_point(aes(color = -rmse))

# Mostramos los modelos del top10 de la parrilla anterior
sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a0, slope = a1, color = -rmse),
    data = filter(grid, rank(rmse)<=10) # Top10
  )
# A base de ir acotando, hemos conseguido reducir el rmse

# Automatizamos la convergencia 
convergencia_modelo_lineal <- function(min_a0, max_a0, min_a1, max_a1, len){ # Convergencia con el método de la parrilla
  grid <- expand.grid(
    a0 = seq(min_a0,max_a0, length = len), 
    a1 = seq(min_a1, max_a1, length = len) 
  ) %>%
    mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse))
  
  # Dibujamos el grid
  p1 <- grid %>%
    ggplot(aes(a0, a1)) +
    geom_point(data = filter(grid, rank(rmse)<=10), size = 4, color = "red") + #Top10
    geom_point(aes(color = -rmse))
  
  # Mostramos los modelos del top10 de la parrilla anterior
  p2 <- sim1 %>%
    ggplot(aes(x,y)) + 
    geom_point(size = 2, color = "grey30") +
    geom_abline(
      aes(intercept = a0, slope = a1, color = -rmse),
      data = filter(grid, rank(rmse)<=10) # Top10
    )
  grid.arrange(p1, p2, nrow = 1)
}

convergencia_modelo_lineal(3,5,1.9,2.2,25)





# Aproximación de Newton-Raphson
best <- optim(c(0,0), rmse, data = sim1) # Dado un punto inicial, queremos minimizar la función rmse, dado el dataset sim1
best$par # Parece que hemos conseguido el modelo lineal más óptimo

# Representemos dicho modelo óptimo obtenido mediante Newton-Raphson
sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2], col = "purple")





# Modelo lineal general:
# y = a0 + a1*x1 + a2*x2 + a3*x3 + ... + an*xn
# En R hay una herramienta que hace todo lo anterior automáticamente, diseñada para ajustar cualquier modelo lineal
lm(y~x, data = sim1) -> sim1_mod
coef(sim1_mod) # Nos devuelve los coeficientes del modelo lineal
summary(sim1_mod)

# El resultado de optim y de lm coinciden, a pesar de que lm no utilice Newton-Raphson, sino que utiliza el algoritmo de la Regresión Lineal





# Vamos a trabajar con otro enfoque
# Empecemos visualizando las predicciones
grid <- sim1 %>%
  data_grid(x) # Generamos automáticamente una parrilla de datos equiespaciados
grid

# Hacemos las predicciones mediante la función add_predictions a partir del modelo de regresión lineal creado anteriormente
grid <- grid %>%
  add_predictions(sim1_mod)
grid

# Pintamos las predicciones. Funciona con cualquier modelo
sim1 %>%
  ggplot(aes(x)) + 
  geom_point(aes(y = y)) + # y del dato original
  geom_line(aes(y = pred), data = grid, color = "red", size = 1) # Modelo. Utilizamos geom_line porque sabemos que se trata de una recta.
  # Si lo hubiésemos querido en general, utilizamos la geometría de punto y listo

# Las predicciones nos dicen el patrón que sigue el modelo. La parte que el modelo es capaz de explicar
# Los errores es la parte que el modelo no es capaz de explicar

sim1 <- sim1 %>%
  add_residuals(sim1_mod) # Añadimos los residuos. ¡Ojo! Debe ser sobre el dataset original. Utilizamos el modelo de regresión lineal
sim1

# Expresemos gráficamente la distribución de los residuos
# Útil para ver si los errores siguen una distribución normal o no
# Lo más normal es que los errores sigan una distribución normal entorno al 0
sim1 %>%
  ggplot(aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# Con esto observamos el ruido
sim1 %>%
  ggplot(aes(x, resid)) + 
  geom_ref_line(h = 0)+
  geom_point()
# Vemos que es aleatorio. Por tanto, el modelo ha hecho un buen trabajo




  
# y ~ x <-> y = a0 + a1 * x <-> y = a0 * out0 + a1 * out1

df <- tribble(
  ~y, ~x1, ~x2,
   4,   2,   5,
   5,   1,   6
)

# Para ver exactamente qué hace R con cada una de las fórmulas, utilizamos model_matrix
# Toma un data frame y una fórmula y devuelve una tibble que define la ecuación del modelo
# Con la función model_matrix vemos la matriz de modelización
model_matrix(df, y ~ x1) # Nos da para cada uno de los x1, la ordenada en el origen
model_matrix(df, y ~ x1 - 1) # Eliminamos la columna de Intercept. Aquí imponemos que nuestro modelo pase por el (0,0)
model_matrix(df, y ~ x1 + x2)





# Categorical models -------------------------------------------------------

# El predictor es una categoría

# y ~ sex <-> y = a0 + a1*sexmale (sexmale = 0, 1)
# R lo que hace es ajustar el valor de categorías a 0 y 1
df <- tribble(
  ~sex, ~value,
  "male",    1,
  "female",  5, 
  "male",    1
)

model_matrix(df, value ~sex)
# Sería redundate añadir la columna sexfemale, porque ésta depende de la columna sexmale
# sexfemale = 1 - sexmale 



sim2 %>% # La variable x tiene 4 categorías
  ggplot(aes(x,y)) + 
  geom_point()

mod2 <- lm(y~x, data = sim2) # Hacemos modelo de Regresión Lineal

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2) # Añadimos las predicciones a cada categoría
grid # Las predicciones que nos da son exactamente la media de cada categoría

sim2 %>%
  ggplot(aes(x)) + 
  geom_point(aes(y = y)) + 
  geom_point(data = grid, aes(y = pred), color = "red", size = 4) # Añadimos la media de cada categoría

# No podemos hacer predicciones en base a algo que no hemos observado:
tibble(x = "e") %>%
  add_predictions(mod2) # La función de predicción nos avisa de que la estamos liando





# C-D models -------------------------------------------------------

# ¿Qué ocurre si juntamos numérico y factor?
# Interacción entre variables continua y categórica

sim3 %>%
  ggplot(aes(x1, y)) + 
  geom_point(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3) # Modelo lineal
mod2 <- lm(y ~ x1 * x2, data = sim3) # Modelo lineal con interacción cruzada (las variables interaccionan)
# y = a0 + (a1 * x1 + a2 * x2) + [a12 * x1 * x2] -- > Entre corchetes se encuentra la interacción y entre paréntesis el comportamiento individual

# Hay que cambiar la metodología para contemplar todas las combinaciones
grid <- sim3 %>%
  data_grid(x1, x2) %>% # Generamos todas las combinaciones posibles
  gather_predictions(mod1, mod2) # Añadimos ambas predicciones en una nueva fila
grid%>%View()

sim3 %>%
  data_grid(x1, x2) %>%
  spread_predictions(mod1, mod2) # Añadimos ambas predicciones en una nueva columna

sim3 %>%
  ggplot(aes(x1, y, color = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~model) # Tenemos que tener en cuenta que tenemos 2 modelos

# Vemos que en el modelo 1 todas las rectas tienen la misma pendiente
# ¿Cuál es mejor?

sim3 %>% 
  gather_residuals(mod1, mod2) %>%
  ggplot(aes(x1, resid, color = x2))+ 
  geom_point() +
  facet_grid(model ~ x2)

# Si representamos los residuos, vemos que en el modelo 2 éstos presentan más aleatoriedad.
# Si nos fijamos en las representaciones del modelo 1, vemos que se ha dejado algo pues aún se observan tendencias





# C-C models -------------------------------------------------------

# Interacción entre 2 variables continuas

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>% 
  gather_predictions(mod1, mod2)

grid



# Utilidades de la función seq_range
# seq_range sirve para ver cómo de dispersa debe de estar nuestra parrilla
seq_range(c(0.23675, 0.98765), n = 6, pretty = TRUE) 
# pretty = TRUE sirve para distribuir de otro modo, de forma más bonita

x1 <- rcauchy(1000)
seq_range(x1, n = 10)
seq_range(x1, n = 10, trim = 0.1) # Nos cargamos el 10% de los datos de la cola (lo reparte entre ambos lados)
seq_range(x1, n = 10, trim = 0.25)
seq_range(x1, n = 10, trim = 0.50)
# trim sirve para recortar la cola

x2 <- c(0,1)
seq_range(x2, n = 10)
seq_range(x2, n = 10, expand = 0.1)
seq_range(x2, n = 10, expand = 0.25)
seq_range(x2, n = 10, expand = 0.5)
# expand sirve para expandir el rango



# Utilizamos representación 3D aplanada
grid %>%
  ggplot(aes(x1,x2))+
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~model) # No sugiere diferencia muy grande entre ambos modelos. Es una mera ilusión

# Miremos desde laterales diferenetes
grid %>%
  ggplot(aes(x1, pred, color = x2, group = x2)) + 
  geom_line()+
  facet_wrap(~model) # Ahora sí vemos diferencias

grid %>%
  ggplot(aes(x2, pred, color = x1, group = x1)) + 
  geom_line()+
  facet_wrap(~model) # Ahora sí vemos diferencias

sim4 %>% # No consigo sacar nada en claro
  gather_residuals(mod1, mod2) %>%
  ggplot(aes(x1, resid, color = x2))+ 
  geom_point() +
  facet_grid(~model)





# Transform models -------------------------------------------------------


# log(y) ~sqrt(x1) + x2 <-> log(y) = a0 + a1 * sqrt(x1) + a2 * x2
# y ~ x + I(x ^ 2) <-> y = a0 + a1 * x + a2 * x^2 ¡Cuidado! Hay que recordar poner I()
# y ~ x + x ^ 2 <-> y ~ x + x * x = x <-> y = a0 + a1*x <--- Esto ocurriría si no ponemos I(x^2)

df <- tribble(
  ~y, ~x, 
   1,  1,
   2,  2, 
   3,  3
)

model_matrix(df, y ~ x + x^2)
model_matrix(df, y ~ x + I(x^2))
# y = a0 + a1*x + a2*x^2 + a3*x^3 + ...
model_matrix(df, y ~ poly(x,2)) # Para polinomio de grado 2

# Los polinomios pueden dar problemas porque suelen crecer a infinito o decrecer a -infinito
# Como alternativa tenemos los splines
library(splines)
model_matrix(df, y ~ ns(x,2))





# Creamos la simulación 5
set.seed(2018)
sim5 <- tibble(
  x = seq(0, 3.5 *pi, length = 50),
  y = 4*sin(x) + rnorm(length(x))
)

sim5 %>%
  ggplot(aes(x,y)) +
  geom_point()





# Splines models -------------------------------------------------------

mod1 <- lm(y ~ ns(x,1), data = sim5)
mod2 <- lm(y ~ ns(x,2), data = sim5)
mod3 <- lm(y ~ ns(x,3), data = sim5)
mod4 <- lm(y ~ ns(x,4), data = sim5)
mod5 <- lm(y ~ ns(x,5), data = sim5)
mod6 <- lm(y ~ ns(x,6), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = "y") # El último parámetro cambia el nombre pred por y

sim5 %>%
  ggplot(aes(x,y)) + 
  geom_point() + 
  geom_line(data = grid, color = "red") + 
  facet_wrap(~model)

# El modelo no nos puede decir nada de datos que no conoce





# Polinomial models -------------------------------------------------------

mod1 <- lm(y ~ poly(x,1), data = sim5)
mod2 <- lm(y ~ poly(x,2), data = sim5)
mod3 <- lm(y ~ poly(x,3), data = sim5)
mod4 <- lm(y ~ poly(x,4), data = sim5)
mod5 <- lm(y ~ poly(x,5), data = sim5)
mod6 <- lm(y ~ poly(x,6), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.5)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = "y")

sim5 %>%
  ggplot(aes(x,y)) + 
  geom_point() + 
  geom_line(data = grid, color = "red") + 
  facet_wrap(~model)





# Other models -------------------------------------------------------

df <- tribble(
  ~x, ~y,
   1, 1.5,
   2,  NA, 
   3, 3.5,
   4, 7.5,
  NA,  15
)

mod <- lm(y~x, data = df) # Esto nos devuelve warning
mod <- lm(y~x, data = df, na.action = na.exclude) # Esto excluye las filas con NA

nobs(mod) # Nos dice cuántas filas hay en el modelo





# Modelo lineal
# y = a0 + a1*x1 + a2*x2 + ... + an*xn
# Modelo lineal generalizado
stats::glm()
# Modelo generalizado additivo
mgcv::gam() # y~s(x) <-> y = f(x)
# Modelo lineal penalizado
glmnet::glmnet()
# Modelo lineal robusto
MASS::rlm()
# Árboles y bosques aleatorios
rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()

