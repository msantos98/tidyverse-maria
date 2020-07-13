library(tidyverse)

# Ejemplo de las 4 tablas (1 tidy y las 3 restantes desordenadas)

## Tabla tidy (table1)
table <- read_csv("data/population.csv")
View(table)

table %>%  
  mutate(rate = cases/population*10000) #tanto por 10000 habitantes

table %>%
  count(year, wt = cases) #número de casos por año

table %>%
  ggplot(aes(year, cases)) + 
  geom_line(aes(group = country), color = "grey") + 
  geom_point(aes(color = country))



## Tabla desordenada (table4a y table 4b)
### GATHER
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases") -> tidy4a

table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population") -> tidy4b

### JOINT
left_join(tidy4a, tidy4b) #juntamos las tablas tidy4a y tidy4b
                          #Se unen por las columnas comunes a ambas tablas



## Tabla desordenada (table2)
### SPREAD
table2 %>%
  spread(key = type, value = count) -> tidy2

tidy2 %>%
  gather(`cases`, `population`, key = "type", value = count)



## Tabla desordenada (table3)
### SEPARATE
table3 %>%
  separate(rate, into = c("cases", "population"))
           
table3 %>% #Haremos exactamente lo mismo, pero especificamos separador
  separate(rate, into = c("cases", "population"),  
           sep = "/")

table3 %>% #Haremos lo mismo, pero cambiamos al tipo de dato idóneo de las nuevas columnas
  separate(rate, into = c("cases", "population"),  
           sep = "/", convert = TRUE)

table3 %>% #Separamos en "siglo" y año
  separate(rate, into = c("cases", "population"),  sep = "/", convert = TRUE) %>%
  separate(year, sep = 2, into = c("century","year"), 
           convert = TRUE)



#Tabla 5 -> desordenada
### UNITE

table5 %>%
  unite(new_year, century, year, sep = "")


 


# Apariciones explícitas e implícitas de NA
roi <- tibble(
  year = c(rep(2016,4), rep(2017,4), 2018),
  quarter = c(rep(c(1,2,3,4),2),1),
  return = rnorm(9, mean = 0.5, sd = 1)
)
roi$return[7] = NA

roi %>%
  spread(year, return) %>%
  gather(year, return, `2016`:`2018`, na.rm = TRUE)



### COMPLETE
roi %>%
  complete(year, quarter)



## NA para no repetir el nombre
### FILL()
treatments <- tribble(
  ~name,         ~treatment,  ~response,
  "Juan Gabriel", 1,          8, 
  NA,             2,          10, 
  NA,             3,          4,
  "Ricardo",      1,          7,
  NA,             2,          9
)

treatments %>%
  fill(name)





# Ejemplo del Instituto Nacional de la Salud
tidyr::who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1) -> tidywho


