library(readxl)
library(tidyverse)
library(ggplot2)
library(ez)
datos1 = read_excel("C:/Users/espin/Downloads/Tarea2.datos1.xls")
datos2 = read_excel("C:/Users/espin/Downloads/Tarea2-datos2.xls")
etiquetas <-c("AGv1", "AGv2", "AGv3", "AGv4")
idvar <- "uwu"
timevar <- "Intento"
vname <- "iwi"
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
  dv = iwi,
  wid = uwu,
  between = Intento,
  type = 2,
  return_aov = TRUE,
  detailed = TRUE
)
ez.aov2 <- ezANOVA(
  data = datos.long2, 
  dv = iwi,
  wid = uwu,
  between = Intento,
  type = 2,
  return_aov = TRUE,
  detailed = TRUE
)
print(ez.aov)
print("ADSAFASFSASAD")
print("------------------")
print(ez.aov2)