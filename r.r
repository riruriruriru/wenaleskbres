library(readxl)
library(tidyverse)
library(ggplot2)
library(ez)
require(multcomp)

datos1 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos1.xls")
datos2 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2-datos2.xls")
etiquetas <-c("AGv1", "AGv2", "AGv3", "AGv4")
idvar <- "col"
timevar <- "Intento"
vname <- "prob"
varying <- etiquetas
datos.long = gather(data = datos1, key = col, value = prob, "AGv1":"AGv4")
datos.long <- mutate(.data = datos.long,
                     Intento = 1:nrow(datos.long))


datos.long2 <- reshape(
  data = datos2,
  idvar = idvar,
  timevar = timevar,
  varying = varying,
  v.names = vname,
  direction = "long"
)
ez.aov <- ezANOVA(
  data = datos.long, 
  dv = prob,
  wid = Intento,
  between = col,
  type = 3,
  return_aov = TRUE,
  detailed = TRUE
)
ez.aov2 <- ezANOVA(
  data = datos.long2, 
  dv = prob,
  wid = col,
  between = Intento,
  type = 2,
  return_aov = TRUE,
  detailed = TRUE
)

#p1 <- ggplot(datos.long, aes(x = Intento, y = prob, fill = Intento))
#p1 <- p1 + geom_boxplot()
#p1 <- p1 + theme(legend.position = "none")

#print(p1)

p2 <- ggplot(datos.long2, aes(x = Intento, y = prob, fill = Intento))
p2 <- p2 + geom_boxplot()
p2 <- p2 + theme(legend.position = "none")

print(p2)
#fcol<-factor(col)

#aov = aov(prob ~ col+Intento,data = datos.long)

print(ez.aov)
print("------------------")
print(ez.aov2)




post_hoc_tukey <- TukeyHSD(ez.aov$aov)
print(post_hoc_tukey)

