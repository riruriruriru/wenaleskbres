library(readxl)
library(tidyverse)
library(ggplot2)
library(ez)
#Se abren los archivos y se guardan los datos en variables correspondientes
datos1 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos1.xls")
datos2 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2-datos2.xls")
datos3 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos3.xls")
datos4 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos4.xls")

#Parte 1 archivo 1

#Se cambian los datos a formato long para el ezAnova
etiquetas <-c("AGv1", "AGv2", "AGv3", "AGv4")
idvar <- "col"
timevar <- "Intento"
vname <- "prob"
varying <- etiquetas
datos.long = gather(data = datos1, key = col, value = prob, "AGv1":"AGv4")
datos.long <- mutate(.data = datos.long,
                     Intento = 1:nrow(datos.long))
#Se hace el test de anova para los datos
ez.aov <- ezANOVA(
  data = datos.long, 
  dv = prob,
  wid = Intento,
  between = col,
  type = 3,
  return_aov = TRUE,
  detailed = TRUE
)

#Se hace un box plot para comparar varianzas
p1 <- ggplot(datos.long, aes(x = Intento, y = prob, fill = Intento))
p1 <- p1 + geom_boxplot()
p1 <- p1 + theme(legend.position = "none")

print(p1)

#Para usar ANOVA, primeramente se deben comprobar ciertas condiciones, los datos deben provenir de muestras normales
#Las varianzas deben ser lo suficientemente homogenes
#Primero se verificará la condición de las varianzas para los archivos 1 y 2:
#Se calculan las varianzas de la parte 1 y 2 y se muestran por pantalla

#Varianzas parte 1
var1 = var(datos1[[2]])
var2 = var(datos1[[3]])
var3 = var(datos1[[4]])
var4 = var(datos1[[5]])
print("VARIANZAS")
print(datos1[[2]])
var11 = var(datos2[[2]])
var22 = var(datos2[[3]])
var33 = var(datos2[[4]])
var44 = var(datos2[[5]])
print("VARIANZAS")
print(var1)
print(var2)
print(var3)
print(var4)
print("VARIANZAS 2")
print(var11)
print(var22)
print(var33)
print(var44)
#Fin varianzas parte 1
#Luego, se debe comprobar las condiciones de normalidad
#Esto se realiza con test de shapiro, los cuales verifican que cada grupo tenga una distribución normal
#Test de shapiro Parte 1
ok_this_is_epic1 = shapiro.test(datos1[[2]])
ok_this_is_epic2 = shapiro.test(datos1[[3]])
ok_this_is_epic3 = shapiro.test(datos1[[4]])
ok_this_is_epic4 = shapiro.test(datos1[[5]])
print(ok_this_is_epic1)
print(ok_this_is_epic2)
print(ok_this_is_epic3)
print(ok_this_is_epic4)
ok_this_is_epic11 = shapiro.test(datos2[[2]])
ok_this_is_epic22 = shapiro.test(datos2[[3]])
ok_this_is_epic33 = shapiro.test(datos2[[4]])
ok_this_is_epic44 = shapiro.test(datos2[[5]])
print(ok_this_is_epic11)
print(ok_this_is_epic22)
print(ok_this_is_epic33)
print(ok_this_is_epic44)

#Fin test de shapiro Parte 1
#Se muestran los resultados del primer test ANOVA
print(ez.aov)
#Se realiza un analisis post hoc con el test de Tukey
post_hoc_tukey <- TukeyHSD(ez.aov$aov)
print(post_hoc_tukey)

#Parte 1, archivo 2
#Se cambian los datos a formato long para el ezANOVA
datos.long2 = gather(data = datos2, key = col, value = prob, "AGv1":"AGv4")
datos.long2 <- mutate(.data = datos.long2,
                      Intento = rep(1:24, times = 4))
#Se aplica el test de ANOVA
ez.aov2 <- ezANOVA(
  data = datos.long2, 
  dv = prob,
  wid = Intento,
  between = col,
  type = 3,
  return_aov = TRUE,
  detailed = TRUE,
)
#Se hace un boxplot para comparar varianzas
p2 <- ggplot(datos.long2, aes(x = Intento, y = prob, fill = Intento))
p2 <- p2 + geom_boxplot()
p2 <- p2 + theme(legend.position = "none")

print(p2)

#Se muestran los resultados
print(ez.aov2)
post_hoc_tukey2 <- TukeyHSD(ez.aov2$aov)
print(post_hoc_tukey2)


#Parte 2 Archivo 1


etiquetas <-c("PDFC", "NNEP", "IS.CHC.1NN","FH.GBML", "GASSIST.ADI","DT.GA")
idvar <- "col"
timevar <- "Intento"
vname <- "prob"
varying <- etiquetas
#Se cambian los datos a formato long para el test de ANOVA
datos.long3 = gather(data = datos3, key = col, value = prob, "PDFC":"DT.GA")
datos.long3 <- mutate(.data = datos.long3,
                      Intento = 1:nrow(datos.long3))
#Se aplica el test de ANOVA
ez.aov3 <- ezANOVA(
  data = datos.long3, 
  dv = prob,
  wid = Intento,
  between = col,
  type = 3,
  return_aov = TRUE,
  detailed = TRUE
)

#Se hace un bloxplot para comparar varianzas
p3 <- ggplot(datos.long2, aes(x = Intento, y = prob, fill = Intento))
p3 <- p3 + geom_boxplot()
p3 <- p3 + theme(legend.position = "none")

print(p3)

#Se muestran los reusltados
print(ez.aov3)                 
print(kruskal.test(prob ~ Intento, data = datos.long3))



#Parte 2 Archivo 2

#Se cambian los datos a formato long para el ezANOVA
datos.long4 <- gather(
  data = datos4,
  key = col,
  value = prob,
  "PDFC":"DT.GA"
)

datos.long4 <- mutate(.data = datos.long4,
                      Intento = 1:nrow(datos.long4))

#Se aplica el test de ANOVA
ez.aov4 <- ezANOVA(
  data = datos.long4, 
  dv = prob,
  wid = Intento,
  between = col,
  type = 2,
  return_aov = TRUE,
  detailed = TRUE
)

#Mostrar resultados
show(ez.aov4)

post_hoc4 <- TukeyHSD(ez.aov4$aov)

show(post_hoc4)