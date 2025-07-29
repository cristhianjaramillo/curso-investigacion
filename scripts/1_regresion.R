#------------------------------------------------------------
# Ejemplo 1: PIB per cápita y esperanza de vida
# Dataset: gapminder (paquete gapminder)
# Tema: Relación entre desarrollo económico y bienestar
#------------------------------------------------------------

library(gapminder)
library(lmtest)      # Para bptest
library(car)         # Para durbinWatsonTest
library(ggplot2)
library(carData)

# Filtramos un año específico (2007) para analizar países en ese año
data1 <- subset(gapminder, year == 2007)

# Modelo: Esperanza de vida ~ PIB per cápita
modelo1 <- lm(lifeExp ~ gdpPercap, data = data1)
summary(modelo1) #H0:El coeficiente es igual a 0.

# Diagnósticos
plot(modelo1, 1)     # Linealidad y homocedasticidad visual
plot(modelo1, 2)     # Normalidad visual (QQ Plot)
bptest(modelo1)      # Test Breusch-Pagan para homocedasticidad. H0: Homocedasticidad
durbinWatsonTest(modelo1) # Independencia de residuos. H0: No hay autocorrelacion

#------------------------------------------------------------
  # Ejemplo 2: Mortalidad infantil y PBI
# Dataset: UN
#------------------------------------------------------------

data_un <- UN
modelo_un <- lm(infantMortality ~ gdp, data = data_un)
summary(modelo_un)

# Diagnósticos
plot(modelo2, 1)     # Linealidad y homocedasticidad
plot(modelo2, 2)     # Normalidad de residuos
bptest(modelo2)
durbinWatsonTest(modelo2)

#------------------------------------------------------------
# Ejemplo 3: Democracia y PIB per cápita
# Dataset: V-Dem (simulación con datos en 'carData::UN')
# Tema: Relación entre índice de democracia y desarrollo económico
#------------------------------------------------------------

set.seed(123)
politica <- data.frame(
  gasto_militar = runif(50, 1, 10),   # % del PIB
  indice_democracia = rnorm(50, 5, 2) # escala 0-10
)

modelo_politica <- lm(indice_democracia ~ gasto_militar, data = politica)
summary(modelo_politica)

# Diagnósticos
plot(modelo_politica, 1)     # Linealidad y homocedasticidad
plot(modelo_politica, 2)     # Normalidad
bptest(modelo_politica)
durbinWatsonTest(modelo_politica)

# Interpretación:
# Se observa cómo el PIB per cápita se asocia con la tasa de participación
# femenina en la fuerza laboral (indicador socioeconómico vinculado a política).
