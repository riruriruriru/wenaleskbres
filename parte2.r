library(readxl)
library(tidyverse)
library(ggplot2)
library(ez)

datos1 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos3.xls")
datos2 = read_excel("/home/nicolas/Escritorio/USACh/Inferencia/wenaleskbres/Tarea2.datos4.xls")
etiquetas <-c("PDFC", "NNEP", "IS.CHC.1NN","FH.GBML", "GASSIST.ADI","DT.GA")
idvar <- "col"
timevar <- "Intento"
vname <- "prob"
varying <- etiquetas
datos.long = gather(data = datos1, key = col, value = prob, "PDFC":"DT.GA")
datos.long <- mutate(.data = datos.long,
                     Intento = 1:nrow(datos.long))

datos.long2 = gather(data = datos2, key = col, value = prob, "PDFC":"DT.GA")
datos.long2 <- mutate(.data = datos.long2,
                     Intento = 1:nrow(datos.long2))
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
  wid = Intento,
  between = col,
  type = 3,
  return_aov = TRUE,
  detailed = TRUE
)
print(ez.aov)
print("ADSAFASFSASAD")
print("------------------")
print(ez.aov2)

p2 <- ggplot(datos.long2, aes(x = Intento, y = prob, fill = Intento))
p2 <- p2 + geom_boxplot()
p2 <- p2 + theme(legend.position = "none")

print(p2)

print(kruskal.test(prob ~ Intento, data = datos.long))


