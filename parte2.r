library(readxl)
library(tidyverse)
library(ggplot2)
library(ez)

datos1 = read_excel("/home/nicolas/Escritorio/Inferencia/wenaleskbres/Tarea2.datos3.xls")
datos2 = read_excel("/home/nicolas/Escritorio/Inferencia/wenaleskbres/Tarea2.datos4.xls")
etiquetas <-c("PDFC", "NNEP", "IS.CHC.1NN","FH.GBML", "GASSIST.ADI","DT.GA")
idvar <- "col"
timevar <- "Intento"
vname <- "prob"
varying <- etiquetas
datos.long <- reshape(
  data = datos1,
  idvar = idvar,
  timevar = timevar,
  varying = varying,
  v.names = vname,
  direction = "long"
)
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
  wid = col,
  between = Intento,
  type = 2,
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
print(ez.aov)
print("ADSAFASFSASAD")
print("------------------")
print(ez.aov2)