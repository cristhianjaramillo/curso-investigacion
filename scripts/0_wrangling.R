
# PAQUETES
#===============================================================================
library(tidyverse)
library(palmerpenguins)


# DATOS
#===============================================================================
# - Cargar datos sobre características/rendimiento de autos. Conjunto de prueba
#   que viene incluido en R base
data(mtcars)

# - Consultar la documentación (codebook) de este conjunto de datos
?mtcars

# PRINCIPAL #1: EJEMPLOS MOSTRADOS EN LAS DIAPOSITIVAS
#===============================================================================

#-------------------------------------------------------------------------------
# [ A ] Filtrado
#-------------------------------------------------------------------------------
# - Ejemplo: mantener solo los autos con 3 marchas
mtcars %>%
  filter(gear == 3)

# - Ejemplo: eliminar del conjunto los autos con más de 5 marchas
mtcars %>%
  filter(gear < 5)



#-------------------------------------------------------------------------------
# [ B ] Transformación de variables
#-------------------------------------------------------------------------------
# - De continua/discreta/categórica a binaria
mtcars %>%
  mutate(gear_4plus = ifelse(gear > 3, 1, 0))

# - De continua/discreta a categórica
mtcars %>%
  mutate(mpg_cat = cut(mpg, breaks = c(10, 15, 20, 25, 30, 35)))

# ... también puedes etiquetar las nuevas categorías como desees
mtcars %>%
  mutate(mpg_cat = cut(mpg, breaks = c(10, 15, 20, 25, 30, 35),
                       labels = c("muy bajo", "bajo", "moderado",
                                  "alto", "muy alto")))

# - Transformaciones no lineales
mtcars %>%
  mutate(mpg_log = log(mpg))


#-------------------------------------------------------------------------------
# [ C ] Reestructuración del conjunto de datos
#-------------------------------------------------------------------------------
# - De formato ancho a formato largo. Por ejemplo, apilando algunas columnas con
#   variables continuas en una sola columna
mtcars %>%
  rownames_to_column("car") %>%
  select(car, mpg, hp) %>%
  gather(variable, value, -car)

# ... esto permite, por ejemplo, graficar la distribución de las variables
#     anteriores todas a la vez
continous_vars <- mtcars %>%
  rownames_to_column("car") %>%
  select(car, mpg, hp) %>%
  gather(variable, value, -car)

ggplot(continous_vars,
       aes(x = value)) +
  geom_density(fill = "gray80") +
  facet_wrap(~ variable, scales = "free") + # / ! \ hablaremos de facets más adelante
  scale_y_continuous("", breaks = NULL) +
  theme(panel.background = element_blank())

# - De formato largo a formato ancho. Por ejemplo, expandiendo las columnas
#   anteriores después de hacer una transformación no lineal
mtcars %>%
  rownames_to_column("car") %>%
  select(car, mpg, hp) %>%
  gather(variable, value, -car) %>%
  mutate(value_log = log(value),
         variable = paste0(variable, "_log")) %>%
  select(-value) %>%
  spread(variable, value_log)


#-------------------------------------------------------------------------------
# [ D ] Agrupación: cálculo de estadísticas a nivel de grupo
#-------------------------------------------------------------------------------
# - Mirando la velocidad promedio (tiempo en 1/4 de milla) por número de marchas
mtcars %>%
  group_by(gear) %>%
  summarize(cars_n = n(),
            qsec_pe = mean(qsec),
            qsec_lwr = t.test(qsec)$conf.int[1],
            qsec_upr = t.test(qsec)$conf.int[2])

# ... graficando los promedios anteriores
avg_speed <- mtcars %>% # ... guardando el nuevo conjunto de datos en "avg_speed"
  group_by(gear) %>%
  summarize(cars_n = n(),
            qsec_pe = mean(qsec),
            qsec_lwr = t.test(qsec)$conf.int[1],
            qsec_upr = t.test(qsec)$conf.int[2])

ggplot(avg_speed,
       aes(x = factor(gear), y = qsec_pe, ymin = qsec_lwr, ymax = qsec_upr)) +
  geom_pointrange() +
  scale_x_discrete("\nNúmero de marchas del auto") +
  scale_y_continuous("Tiempo promedio para 1/4 de milla\n") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "gray80"),
        axis.line = element_line())

#-------------------------------------------------------------------------------
# [ E ] Ordenamiento: para aprovechar mejor la visualización
#-------------------------------------------------------------------------------
avg_speed <- mtcars %>%
  group_by(gear) %>%
  summarize(cars_n = n(),
            qsec_pe = mean(qsec),
            qsec_lwr = t.test(qsec)$conf.int[1],
            qsec_upr = t.test(qsec)$conf.int[2]) %>%
  arrange(desc(qsec_pe)) %>%
  mutate(gear = factor(gear, levels = unique(gear)))

ggplot(avg_speed,
       aes(x = gear, y = qsec_pe, ymin = qsec_lwr, ymax = qsec_upr)) +
  geom_pointrange() +
  scale_x_discrete("\nNúmero de marchas del auto") +
  scale_y_continuous("Tiempo promedio para 1/4 de milla\n") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(linetype = "dotted", color = "gray80"),
        axis.line = element_line())


# PRINCIPAL #2: ¡PRUEBA COSAS POR TU CUENTA AHORA!
#===============================================================================
# - Usa el siguiente conjunto de datos del paquete "palmerpenguins": mediciones
#   para diferentes tipos de pingüinos
#install.packages("palmerpenguins")
library(palmerpenguins)
data(penguins)
?penguins


#-------------------------------------------------------------------------------
# [ A ] Filtrado
#-------------------------------------------------------------------------------
# Filtra el conjunto de datos (y guarda uno nuevo) como sigue:
#   - (A.1) elimina las filas que tengan un NA en cualquiera de las variables
#   - (A.2) elimina las filas con información registrada antes de 2008


#-------------------------------------------------------------------------------
# [ B ] Transformación de variables
#-------------------------------------------------------------------------------
# Usa el conjunto resultante del punto A y agrega una nueva variable con el log
#   de "flipper_length_mm".


#-------------------------------------------------------------------------------
# [ C ] Reestructuración del conjunto de datos
#-------------------------------------------------------------------------------
# Usa el conjunto resultante del punto A para hacer lo siguiente. Guarda el
#   resultado como un nuevo conjunto de datos.
#   - (C.1) Crea una nueva variable con un ID para cada observación:
#   - (C.2) Conserva solo las siguientes variables:
#             id, bill_length_mm y bill_depth_mm
#   - (C.2) Usa "gather()" para apilar en una sola columna los datos de:
#             bill_length_mm y bill_depth_mm


# Luego, usa el conjunto de datos resultante de arriba (C.1 + C.2) para:
#   - (C.3) Calcular el log de todos los valores en la nueva columna creada en C.2
#   - (C.4) Transformar esta nueva columna log en 2 columnas separadas


#-------------------------------------------------------------------------------
# [ D ] Agrupación: cálculo de estadísticas a nivel de grupo
#-------------------------------------------------------------------------------
# Usa el conjunto de datos resultante del punto A para hacer lo siguiente.
#   Guarda el resultado como un nuevo conjunto de datos.
#   - (D.1) calcula el peso corporal promedio (body_mass_g) y el IC del 95%
#             por sexo Y por especie


#-------------------------------------------------------------------------------
# [ E ] Ordenamiento: para aprovechar mejor la visualización
#-------------------------------------------------------------------------------
# Usa el conjunto de datos resultante del punto D para hacer lo siguiente.
#   Guarda el resultado como un nuevo conjunto de datos.
#   - (E.1) ordena el conjunto de datos por el peso corporal promedio
#   - (E.2) re-especifica los niveles de los factores de las variables
#           sexo y especie, de acuerdo con el orden


#-------------------------------------------------------------------------------
# [ F ] BONUS: figura interesante
#-------------------------------------------------------------------------------
# Usa el conjunto resultante del punto E para crear una figura interesante que
#   muestre los promedios del peso corporal (por sexo y por especie)
