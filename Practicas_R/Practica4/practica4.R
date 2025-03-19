library(haven) #para leer datos .dta
library(fixest) #para regresiones con efectos fijos y regresiones lineales
library(ggplot2) #gráficas
library(tidyr) #manipulación de datos
library(dplyr) #manipulación de datos
library(lmtest) #tiene múltiples test 
library(MASS)
library(car)
getwd()
setwd("D:/Universidad/ESTADISTICAS/Clase 3")
rm(list=ls())
df<-read_dta("HPRICE2.DTA")
head(df)
str(df)
#modelo: lprice=b0+b1lnox+b2rooms+b3crimes+ldist

modelo1<-lm(lprice~lnox+rooms+crime+dist, data=df)
summary(modelo1)

# --- 1. PRUEBA DE LINEALIDAD ---

resettest(modelo1)
#H0: el modelo está correctamente especificado

# gráfico de los residuos vs los valores ajustados
# si están dispersos alrededor de cero, entonces puede que exista una relación no lineal
ggplot(data.frame(ajustados = fitted(modelo1), residuos = resid(modelo1)), aes(x = ajustados, y = residuos)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", size = 1) +  
  labs(title = "Residuos vs valores ajustados", x = "Valores ajustados", y = "Residuos") +
  theme_classic()
#Detectar no linealidad y heterocedasticidad
residualPlot(modelo1)
#los Pearson Resids son simplemente los residuos escalados por su
#desviación estándar (comparable entre modelos)
# La linea azúl muestra un ajuste suavizado de los residuos utilizando un estimador
#no paramétrico, si tiene una curva o tendencia, sugiere que la relacion no es lineal
# --- 2. INDEPENDENCIA DE LOS ERRORES ---

# Prueba de Durbin-Watson 
#(H0: No hay autocorrelación en los residuos)
dwtest(modelo1)
#un DW cercano a 2 indice independencia, valores menores o mayores
#muestran una posible autocorrelación


# --- 3. NORMALIDAD DE LOS ERRORES ---

# Prueba de Shapiro-Wilk 
#(H0: los residuos siguen una distribución normal)
shapiro.test(resid(modelo1))

# QQ-plot para visualizar normalidad de los residuos
qqnorm(resid(modelo1))
qqline(resid(modelo1), col = "red")
# colas ligeras-> distribución más compacta
#las predicciones están más cercanas a los valores reales
# con menos errores extremos que lo que se esperaría si 
# la distribución de los errores fuera normal
# si los residuos se desvían de las líneas rojas, puede indicar falta 
#de normalidad

# Histograma de los residuos
hist(resid(modelo1), main = "Histograma de residuos", xlab = "Residuos", col = "orange", border = "black")
# Se espera una campana si los residuos son normales

# --- 4. HOMOCEDASTICIDAD ---

# Prueba de Breusch-Pagan 
#(H0: Homocedasticidad)
bptest(modelo1)
#Non constant error variance
#H0: varianza constante de los errores
ncvTest(modelo1)
#Goldfeld-Quandt test
#H0: La varianza en los dos submodelos es la misma
gqtest(modelo1)

# Gráfico de residuos vs valores ajustados para evaluar heterocedasticidad
ggplot(data.frame(ajustados = fitted(modelo1), residuos = resid(modelo1)), aes(x = ajustados, y = residuos)) +
  geom_point() + 
  labs(title = "Residuos vs valores ajustados", x = "Valores ajustados", y = "Residuos") +
  theme_classic()
# Se nota una figura cónica y concentración de los residuos

# --- 5. MULTICOLINEALIDAD ---

# Cálculo del VIF 
# se considera problemática si VIF > 10)
vif(modelo1)

