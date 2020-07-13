#Ejercicio 4
roi <- tibble(
  year = c(rep(2016,4), rep(2017,4), 2018),
  quarter = c(rep(c(1,2,3,4),2),1),
  return = rnorm(9, mean = 0.5, sd = 1)
)

roi %>%
  spread(year, return) %>%
  gather("year", "return", `2016`:`2018`)

#Ejercicio 5
?spread
# convert: If TRUE, type.convert() with asis = TRUE will be run 
# on each of the new columns. This is useful if the value column 
# was a mix of variables that was coerced to a string. If the 
# class of the value column was factor or date, note that will 
# not be true of the new columns that are produced, which are 
# coerced to character before type conversion.

#Ejercicio 7
people <- tribble(
  ~name,         ~key,   ~value,
  #-------------|-------|-------
  "Juan Gabriel", "age",     18,
  "Juan Gabriel", "weight",  58,
  "Juan Gabriel", "age",     30,
  "Juan Gabriel", "weight",  71,
  "Ricardo",      "age",     55,
  "Ricardo",      "age",     75
)

#Ejercicio 8
pregnancy <- tribble(
  ~pregnant, ~male, ~female,
  #--------|------|---------
  "yes",    NA,    32,
  "no",     85,    43
)

pregnancy %>%
  gather("male", "female", key = sex, value = count)  %>%
  mutate(pregnant = (pregnant == "yes"),
         female = (sex == "female")) %>%
  select(-sex)







#Ejercicio 5 - Mapa de aeropuertos de USA
airports %>% 
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon,lat)) + 
  borders("state")+
  geom_point()+
  coord_quickmap()

airports %>% count(alt, lon) %>% filter(n>1)







tibble(x = c("a,b,c", "d,e,f,g","h,i,j")) %>%
  separate(x, c("x", "y", "z"), extra = "drop")

tibble(x = c("a,b,c", "d,e","f,g,h")) %>%
  separate(x, c("x", "y", "z"), fill = "right")