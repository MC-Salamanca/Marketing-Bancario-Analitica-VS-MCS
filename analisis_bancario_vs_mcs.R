
# PROYECTO FINAL DE ANALITICA DE NEGOCIOS
# Marketing bancario y comportamiento de los clientes
# Maria Camila Salamanca y Veronica Sandoval

# PARTE 1: IMPORTACION, REVISION Y LIMPIEZA DE DATOS

# 1. INSTALAR Y CARGAR LIBRERIAS
# Instalar readxl solamente si no esta instalada.
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# Cargar la libreria para leer archivos Excel.
library(readxl)

# 2. IMPORTAR LA BASE DE DATOS

# Leer la hoja bank-full del archivo Excel.
banco <- read_excel(
  "base de datos analitica .xlsx",
  sheet = "bank-full"
)

# Mostrar las primeras seis filas.
head(banco)

# Mostrar la cantidad de filas y columnas.
dim(banco)

# Conocer los nombres de las variables.
names(banco)

# Revisar el tipo de cada variable.
str(banco)

# 3. REVISION GENERAL DE LOS DATOS
# Obtener un resumen de todas las variables.
summary(banco)

# Contar los valores faltantes por variable.
colSums(is.na(banco))

# Contar las filas duplicadas.
sum(duplicated(banco))

# Revisar los resultados de la campaña bancaria.
table(banco$y)

# Calcular el porcentaje de clientes que aceptaron.
prop.table(table(banco$y)) * 100

# 4. PREPARAR LA VARIABLE DEPENDIENTE
# Crear una variable numerica para la aceptacion.
# El valor 1 significa que el cliente acepto.
# El valor 0 significa que el cliente no acepto.
banco$acepta <- ifelse(banco$y == "yes", 1, 0)

# Verificar la nueva variable.
table(banco$acepta)

# Calcular la tasa general de aceptacion.
mean(banco$acepta) * 100

# 5. PREPARAR LAS VARIABLES EXPLICATIVAS
# Convertir las variables categoricas en factores.
banco$job <- as.factor(banco$job)
banco$marital <- as.factor(banco$marital)
banco$education <- as.factor(banco$education)
banco$default <- as.factor(banco$default)
banco$housing <- as.factor(banco$housing)
banco$loan <- as.factor(banco$loan)
banco$contact <- as.factor(banco$contact)
banco$month <- as.factor(banco$month)
banco$poutcome <- as.factor(banco$poutcome)

# Verificar que las variables quedaron preparadas.
str(banco)

# 6. REVISAR LA BASE FINAL
# Contar nuevamente los registros.
nrow(banco)

# Mostrar las primeras observaciones preparadas.
head(banco)

# Verificar que la variable acepta no tenga faltantes.
sum(is.na(banco$acepta))


# PARTE 2: ESTADISTICAS DESCRIPTIVAS Y GRAFICOS

# 1. CARGAR LA LIBRERIA PARA LOS GRAFICOS
# Instalar ggplot2 si no esta instalada.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Cargar la libreria ggplot2.
library(ggplot2)


# 2. CREAR LA CARPETA DE RESULTADOS

# Crear una carpeta para guardar tablas y graficos.
dir.create("resultados", showWarnings = FALSE)


# 3. ESTADISTICAS DESCRIPTIVAS

# Seleccionar las variables numericas principales.
variables <- c("age", "balance", "duration",
               "campaign", "previous")

# Calcular el promedio de cada variable.
sapply(banco[variables], mean, na.rm = TRUE)

# Calcular el valor minimo.
sapply(banco[variables], min, na.rm = TRUE)

# Calcular el valor maximo.
sapply(banco[variables], max, na.rm = TRUE)

# Calcular la desviacion estandar.
sapply(banco[variables], sd, na.rm = TRUE)

# Crear una tabla descriptiva completa.
tabla_descriptiva <- data.frame(
  Variable = variables,
  Promedio = sapply(banco[variables], mean, na.rm = TRUE),
  Minimo = sapply(banco[variables], min, na.rm = TRUE),
  Maximo = sapply(banco[variables], max, na.rm = TRUE),
  Desviacion = sapply(banco[variables], sd, na.rm = TRUE)
)

# Mostrar la tabla.
print(tabla_descriptiva)

# Guardar la tabla en formato CSV.
write.csv(tabla_descriptiva,
          "resultados/tabla_descriptiva.csv",
          row.names = FALSE)


# 4. ANALISIS DE ACEPTACION BANCARIA

# Contar clientes que aceptaron y rechazaron.
table(banco$y)

# Calcular porcentajes de aceptacion y rechazo.
round(prop.table(table(banco$y)) * 100, 2)

# Calcular la tasa general de aceptacion.
mean(banco$acepta) * 100

# Comparar promedios entre quienes aceptaron y no.
aggregate(
  banco[variables],
  by = list(Aceptacion = banco$y),
  FUN = mean,
  na.rm = TRUE
)


# 5. TABLAS POR CARACTERISTICAS DEL CLIENTE

# Calcular aceptacion segun nivel educativo.
tabla_educacion <- aggregate(
  acepta ~ education,
  data = banco,
  FUN = mean
)

# Convertir la tasa a porcentaje.
tabla_educacion$acepta <- tabla_educacion$acepta * 100

# Mostrar los resultados.
print(tabla_educacion)


# Calcular aceptacion segun estado civil.
tabla_estado_civil <- aggregate(
  acepta ~ marital,
  data = banco,
  FUN = mean
)

# Convertir a porcentaje.
tabla_estado_civil$acepta <- tabla_estado_civil$acepta * 100

# Mostrar la tabla.
print(tabla_estado_civil)


# Calcular aceptacion segun prestamo personal.
tabla_prestamo <- aggregate(
  acepta ~ loan,
  data = banco,
  FUN = mean
)

# Convertir a porcentaje.
tabla_prestamo$acepta <- tabla_prestamo$acepta * 100

# Mostrar los resultados.
print(tabla_prestamo)


# 6. CREAR GRUPOS DE EDAD

# Clasificar a los clientes por rangos de edad.
banco$grupo_edad <- cut(
  banco$age,
  breaks = c(0, 30, 45, 60, Inf),
  labels = c("Hasta 30", "31 a 45",
             "46 a 60", "Mas de 60"),
  include.lowest = TRUE
)

# Calcular la tasa de aceptacion por grupo.
tabla_edad <- aggregate(
  acepta ~ grupo_edad,
  data = banco,
  FUN = mean
)

# Convertir a porcentaje.
tabla_edad$acepta <- tabla_edad$acepta * 100

# Mostrar la tabla.
print(tabla_edad)


# 7. GRAFICO DE DISTRIBUCION DE EDAD

# Crear un histograma de edades.
grafico_edad <- ggplot(banco, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue",
                 color = "white") +
  labs(
    title = "Distribucion de edades",
    x = "Edad del cliente",
    y = "Numero de clientes"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_edad)

# Guardar el grafico.
ggsave("resultados/grafico_edad.png",
       plot = grafico_edad, width = 8, height = 5)


# 8. GRAFICO DE ACEPTACION BANCARIA

# Crear un grafico de aceptacion y rechazo.
grafico_aceptacion <- ggplot(banco, aes(x = y)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Resultados de la campana bancaria",
    x = "Aceptacion de la campana",
    y = "Numero de clientes"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_aceptacion)

# Guardar el grafico.
ggsave("resultados/grafico_aceptacion.png",
       plot = grafico_aceptacion,
       width = 8, height = 5)


# 9. GRAFICO DE ACEPTACION POR EDAD

# Crear un grafico con los porcentajes por edad.
grafico_grupo_edad <- ggplot(
  tabla_edad,
  aes(x = grupo_edad, y = acepta)
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Aceptacion por grupo de edad",
    x = "Grupo de edad",
    y = "Porcentaje de aceptacion"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_grupo_edad)

# Guardar el grafico.
ggsave("resultados/grafico_grupo_edad.png",
       plot = grafico_grupo_edad,
       width = 8, height = 5)


# 10. GRAFICO DE SALDO BANCARIO

# Comparar el saldo entre clientes que aceptaron y no.
grafico_saldo <- ggplot(
  banco,
  aes(x = y, y = balance)
) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Saldo bancario segun aceptacion",
    x = "Aceptacion de la campana",
    y = "Saldo bancario"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_saldo)

# Guardar el grafico.
ggsave("resultados/grafico_saldo.png",
       plot = grafico_saldo,
       width = 8, height = 5)


# 11. GRAFICO DE CONTACTOS Y ACEPTACION

# Calcular aceptacion segun numero de contactos.
tabla_contactos <- aggregate(
  acepta ~ campaign,
  data = banco,
  FUN = mean
)

# Convertir la tasa a porcentaje.
tabla_contactos$acepta <- tabla_contactos$acepta * 100

# Crear grafico de dispersion.
grafico_contactos <- ggplot(
  tabla_contactos,
  aes(x = campaign, y = acepta)
) +
  geom_point(color = "steelblue") +
  labs(
    title = "Contactos y tasa de aceptacion",
    x = "Numero de contactos",
    y = "Porcentaje de aceptacion"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_contactos)

# Guardar el grafico.
ggsave("resultados/grafico_contactos.png",
       plot = grafico_contactos,
       width = 8, height = 5)


# 12. GUARDAR LAS TABLAS ADICIONALES

# Guardar la tabla de aceptacion por educacion.
write.csv(tabla_educacion,
          "resultados/tabla_educacion.csv",
          row.names = FALSE)

# Guardar la tabla de aceptacion por estado civil.
write.csv(tabla_estado_civil,
          "resultados/tabla_estado_civil.csv",
          row.names = FALSE)

# Guardar la tabla de aceptacion por prestamos.
write.csv(tabla_prestamo,
          "resultados/tabla_prestamo.csv",
          row.names = FALSE)

# Guardar la tabla de aceptacion por edad.
write.csv(tabla_edad,
          "resultados/tabla_edad.csv",
          row.names = FALSE)

# Guardar la tabla de contactos.
write.csv(tabla_contactos,
          "resultados/tabla_contactos.csv",
          row.names = FALSE)


# 13. VERIFICAR LOS RESULTADOS

# Mostrar los archivos generados.
list.files("resultados")

# PARTE 3: MODELOS MPL Y LOGIT
# 1. PREPARAR LOS DATOS PARA LOS MODELOS

# Seleccionar las variables que utilizaremos.
variables_modelo <- c(
  "acepta", "campaign", "age", "balance",
  "job", "marital", "education", "default",
  "housing", "loan", "previous", "poutcome"
)

# Crear una base con las variables seleccionadas.
datos_modelo <- banco[, variables_modelo]

# Eliminar filas con valores faltantes, si existen.
datos_modelo <- na.omit(datos_modelo)

# Mostrar el numero de observaciones disponibles.
nrow(datos_modelo)

# Fijar una semilla para obtener resultados reproducibles.
set.seed(123)

# Seleccionar aleatoriamente el 80% para entrenamiento.
filas_entrenamiento <- sample(
  1:nrow(datos_modelo),
  size = floor(0.80 * nrow(datos_modelo))
)

# Crear la base de entrenamiento.
entrenamiento <- datos_modelo[filas_entrenamiento, ]

# Crear la base de prueba con el 20% restante.
prueba <- datos_modelo[-filas_entrenamiento, ]

# Revisar el tamaño de las dos bases.
nrow(entrenamiento)
nrow(prueba)


# 2. MODELO DE PROBABILIDAD LINEAL SIMPLE

# Estimar la aceptacion usando numero de contactos.
mpl_simple <- lm(
  acepta ~ campaign,
  data = entrenamiento
)

# Mostrar los resultados del modelo.
summary(mpl_simple)


# 3. MODELO DE PROBABILIDAD LINEAL CON CONTROLES

# Estimar la aceptacion considerando otras caracteristicas.
mpl_controles <- lm(
  acepta ~ campaign + age + balance +
    job + marital + education + default +
    housing + loan + previous + poutcome,
  data = entrenamiento
)

# Mostrar los resultados del modelo.
summary(mpl_controles)


# 4. MODELO LOGIT SIMPLE

# Estimar la probabilidad de aceptacion con campaign.
logit_simple <- glm(
  acepta ~ campaign,
  data = entrenamiento,
  family = binomial(link = "logit")
)

# Mostrar los resultados del modelo.
summary(logit_simple)

# 5. MODELO LOGIT CON CONTROLES

# Estimar la probabilidad con caracteristicas del cliente.
logit_controles <- glm(
  acepta ~ campaign + age + balance +
    job + marital + education + default +
    housing + loan + previous + poutcome,
  data = entrenamiento,
  family = binomial(link = "logit")
)

# Mostrar los resultados del modelo.
summary(logit_controles)


# 6. REVISAR LOS COEFICIENTES

# Mostrar coeficientes del MPL simple.
coef(mpl_simple)

# Mostrar coeficientes del MPL con controles.
coef(mpl_controles)

# Mostrar coeficientes del Logit simple.
coef(logit_simple)

# Mostrar coeficientes del Logit con controles.
coef(logit_controles)


# 7. ANALIZAR LA SIGNIFICANCIA ESTADISTICA

# Crear una tabla con los resultados del MPL.
tabla_mpl <- as.data.frame(
  summary(mpl_controles)$coefficients
)

# Mostrar los resultados.
print(tabla_mpl)

# Crear una tabla con los resultados del Logit.
tabla_logit <- as.data.frame(
  summary(logit_controles)$coefficients
)

# Mostrar los resultados.
print(tabla_logit)

# 8. INTERPRETAR LOS ODDS RATIOS DEL LOGIT

# Transformar los coeficientes del Logit.
odds_ratios <- exp(coef(logit_controles))

# Mostrar los odds ratios.
print(odds_ratios)

# 9. COMPARAR EL AJUSTE DE LOS MODELOS

# Mostrar R cuadrado del MPL simple.
summary(mpl_simple)$r.squared

# Mostrar R cuadrado del MPL con controles.
summary(mpl_controles)$r.squared

# Mostrar AIC del Logit simple.
AIC(logit_simple)

# Mostrar AIC del Logit con controles.
AIC(logit_controles)

# 10. GUARDAR LAS TABLAS DE RESULTADOS
# Guardar los coeficientes del MPL.
write.csv(
  tabla_mpl,
  "resultados/modelo_mpl.csv"
)

# Guardar los coeficientes del Logit.
write.csv(
  tabla_logit,
  "resultados/modelo_logit.csv"
)

# Crear una tabla con los odds ratios.
tabla_odds <- data.frame(
  Variable = names(odds_ratios),
  Odds_Ratio = as.numeric(odds_ratios)
)

# Guardar la tabla de odds ratios.
write.csv(
  tabla_odds,
  "resultados/odds_ratios.csv",
  row.names = FALSE
)


# 11. VERIFICAR LOS RESULTADOS

# Mostrar los archivos generados.
list.files("resultados")

# PARTE 4: ROBUSTEZ Y EVALUACION PREDICTIVA

# 1. INSTALAR Y CARGAR LIBRERIAS

# Instalar lmtest si no esta instalada.
if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest")
}

# Instalar sandwich si no esta instalada.
if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}

# Instalar ggplot2 si no esta instalada.
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Cargar las librerias necesarias.
library(lmtest)
library(sandwich)
library(ggplot2)

# Crear la carpeta para guardar resultados.
dir.create("resultados", showWarnings = FALSE)


# 2. PRUEBA DE BREUSCH-PAGAN

# Evaluar heterocedasticidad en el MPL simple.
bp_simple <- bptest(mpl_simple)

# Mostrar el resultado de la prueba.
print(bp_simple)

# Evaluar heterocedasticidad en el MPL con controles.
bp_controles <- bptest(mpl_controles)

# Mostrar el resultado de la prueba.
print(bp_controles)


# 3. CALCULAR ERRORES ESTANDAR ROBUSTOS

# Calcular errores robustos HC1 para el MPL simple.
robusto_mpl_simple <- coeftest(
  mpl_simple,
  vcov. = vcovHC(mpl_simple, type = "HC1")
)

# Mostrar los resultados del MPL simple.
print(robusto_mpl_simple)

# Calcular errores robustos HC1 para el MPL con controles.
robusto_mpl_controles <- coeftest(
  mpl_controles,
  vcov. = vcovHC(mpl_controles, type = "HC1")
)

# Mostrar los resultados robustos.
print(robusto_mpl_controles)


# 4. CORRECCION PARA GUARDAR LOS ERRORES ROBUSTOS

# Convertir los resultados robustos en una matriz.
matriz_robusta <- as.matrix(robusto_mpl_controles)

# Crear una tabla con los coeficientes robustos.
tabla_robusta <- data.frame(
  Variable = rownames(matriz_robusta),
  Coeficiente = matriz_robusta[, 1],
  Error_Robusto = matriz_robusta[, 2],
  Estadistico_t = matriz_robusta[, 3],
  Valor_p = matriz_robusta[, 4],
  row.names = NULL
)

# Mostrar la tabla corregida.
print(tabla_robusta)

# Guardar los resultados robustos en CSV.
write.csv(
  tabla_robusta,
  "resultados/mpl_errores_robustos.csv",
  row.names = FALSE
)


# 5. GRAFICO DE RESIDUOS DEL MPL

# Crear una tabla de valores estimados y residuos.
datos_residuos <- data.frame(
  Estimado = fitted(mpl_controles),
  Residuo = residuals(mpl_controles)
)

# Crear el grafico de residuos.
grafico_residuos <- ggplot(
  datos_residuos,
  aes(x = Estimado, y = Residuo)
) +
  geom_point(alpha = 0.15, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos del modelo MPL",
    x = "Probabilidad estimada",
    y = "Residuo"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_residuos)

# Guardar el grafico.
ggsave(
  "resultados/grafico_residuos_mpl.png",
  plot = grafico_residuos,
  width = 8,
  height = 5
)


# 6. CALCULAR PREDICCIONES

# Predecir con el MPL simple.
pred_mpl_simple <- predict(
  mpl_simple,
  newdata = prueba
)

# Predecir con el MPL con controles.
pred_mpl <- predict(
  mpl_controles,
  newdata = prueba
)

# Predecir con el Logit simple.
pred_logit_simple <- predict(
  logit_simple,
  newdata = prueba,
  type = "response"
)

# Predecir con el Logit con controles.
pred_logit <- predict(
  logit_controles,
  newdata = prueba,
  type = "response"
)

# Guardar los valores reales de aceptacion.
real <- prueba$acepta

# Limitar las predicciones MPL entre 0 y 1.
pred_mpl_simple <- pmin(pmax(pred_mpl_simple, 0), 1)
pred_mpl <- pmin(pmax(pred_mpl, 0), 1)


# 7. CALCULAR ERRORES AL CUADRADO

# Calcular los errores cuadrados del MPL simple.
error_mpl_simple <- (real - pred_mpl_simple)^2

# Calcular los errores cuadrados del MPL con controles.
error_mpl <- (real - pred_mpl)^2

# Calcular los errores cuadrados del Logit simple.
error_logit_simple <- (real - pred_logit_simple)^2

# Calcular los errores cuadrados del Logit con controles.
error_logit <- (real - pred_logit)^2

# Calcular el error cuadratico medio de cada modelo.
mse_mpl_simple <- mean(error_mpl_simple)
mse_mpl <- mean(error_mpl)
mse_logit_simple <- mean(error_logit_simple)
mse_logit <- mean(error_logit)

# Mostrar los resultados.
mse_mpl_simple
mse_mpl
mse_logit_simple
mse_logit


# 8. COMPARAR LOS CUATRO MODELOS

# Crear una tabla con los errores de prediccion.
tabla_comparacion <- data.frame(
  Modelo = c(
    "MPL simple",
    "MPL con controles",
    "Logit simple",
    "Logit con controles"
  ),
  MSE = c(
    mse_mpl_simple,
    mse_mpl,
    mse_logit_simple,
    mse_logit
  )
)

# Ordenar los modelos de menor a mayor error.
tabla_comparacion <- tabla_comparacion[
  order(tabla_comparacion$MSE), ]

# Mostrar la tabla de comparacion.
print(tabla_comparacion)

# Guardar la tabla en CSV.
write.csv(
  tabla_comparacion,
  "resultados/comparacion_modelos.csv",
  row.names = FALSE
)


# 9. CREAR MATRICES DE CONFUSION

# Establecer un umbral inicial de clasificacion.
umbral <- 0.5

# Clasificar las predicciones del MPL.
clase_mpl <- ifelse(pred_mpl >= umbral, 1, 0)

# Clasificar las predicciones del Logit.
clase_logit <- ifelse(pred_logit >= umbral, 1, 0)

# Crear la matriz de confusion del MPL.
matriz_mpl <- table(
  Real = factor(real, levels = c(0, 1)),
  Predicho = factor(clase_mpl, levels = c(0, 1))
)

# Mostrar la matriz del MPL.
print(matriz_mpl)

# Crear la matriz de confusion del Logit.
matriz_logit <- table(
  Real = factor(real, levels = c(0, 1)),
  Predicho = factor(clase_logit, levels = c(0, 1))
)

# Mostrar la matriz del Logit.
print(matriz_logit)


# 10. CALCULAR INDICADORES DE CLASIFICACION

# Crear una funcion para calcular indicadores.
calcular_metricas <- function(real, predicho) {
  
  # Contar verdaderos positivos.
  VP <- sum(real == 1 & predicho == 1)
  
  # Contar verdaderos negativos.
  VN <- sum(real == 0 & predicho == 0)
  
  # Contar falsos positivos.
  FP <- sum(real == 0 & predicho == 1)
  
  # Contar falsos negativos.
  FN <- sum(real == 1 & predicho == 0)
  
  # Calcular exactitud.
  exactitud <- (VP + VN) / length(real)
  
  # Calcular sensibilidad.
  sensibilidad <- if (VP + FN > 0) {
    VP / (VP + FN)
  } else {
    NA_real_
  }
  
  # Calcular precision.
  precision <- if (VP + FP > 0) {
    VP / (VP + FP)
  } else {
    NA_real_
  }
  
  # Devolver los indicadores.
  return(c(
    Exactitud = exactitud,
    Sensibilidad = sensibilidad,
    Precision = precision
  ))
}

# Calcular los indicadores del MPL.
metricas_mpl <- calcular_metricas(real, clase_mpl)

# Calcular los indicadores del Logit.
metricas_logit <- calcular_metricas(real, clase_logit)

# Crear una tabla de comparacion.
tabla_metricas <- data.frame(
  Modelo = c("MPL con controles", "Logit con controles"),
  Exactitud = c(metricas_mpl["Exactitud"],
                metricas_logit["Exactitud"]),
  Sensibilidad = c(metricas_mpl["Sensibilidad"],
                   metricas_logit["Sensibilidad"]),
  Precision = c(metricas_mpl["Precision"],
                metricas_logit["Precision"])
)

# Mostrar la tabla de indicadores.
print(tabla_metricas)

# Guardar la tabla de indicadores.
write.csv(
  tabla_metricas,
  "resultados/metricas_clasificacion.csv",
  row.names = FALSE
)


# 11. COMPARAR VALORES REALES Y PREDICHOS

# Crear una tabla con las predicciones individuales.
tabla_predicciones <- data.frame(
  Real = real,
  MPL = pred_mpl,
  Logit = pred_logit
)

# Mostrar las primeras observaciones.
head(tabla_predicciones)

# Guardar la tabla de predicciones.
write.csv(
  tabla_predicciones,
  "resultados/predicciones_modelos.csv",
  row.names = FALSE
)


# 12. GRAFICO DE PREDICCIONES

# Crear un grafico de aceptacion real y estimada.
grafico_predicciones <- ggplot(
  tabla_predicciones,
  aes(x = Logit, y = Real)
) +
  geom_jitter(
    height = 0.05,
    width = 0,
    alpha = 0.12,
    color = "steelblue"
  ) +
  labs(
    title = "Aceptacion real frente a probabilidad estimada",
    x = "Probabilidad estimada con Logit",
    y = "Aceptacion real: 0 = No, 1 = Si"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_predicciones)

# Guardar el grafico.
ggsave(
  "resultados/grafico_predicciones.png",
  plot = grafico_predicciones,
  width = 8,
  height = 5
)


# 13. GRAFICO DE ERRORES AL CUADRADO

# Crear una tabla de errores.
tabla_errores <- data.frame(
  MPL = error_mpl,
  Logit = error_logit
)

# Crear un histograma de errores del Logit.
grafico_errores <- ggplot(
  tabla_errores,
  aes(x = Logit)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribucion de errores cuadrados del Logit",
    x = "Error al cuadrado",
    y = "Numero de clientes"
  ) +
  theme_minimal()

# Mostrar el grafico.
print(grafico_errores)

# Guardar el grafico.
ggsave(
  "resultados/grafico_errores.png",
  plot = grafico_errores,
  width = 8,
  height = 5
)


# 14. VERIFICAR LOS RESULTADOS

# Mostrar todos los archivos generados.
list.files("resultados")


# PARTE 5: SEGMENTACION Y RECOMENDACIONES COMERCIALES
# 1. PREPARAR LOS RESULTADOS

# Cargar la libreria para los graficos.
library(ggplot2)

# Crear la carpeta de resultados si no existe.
dir.create("resultados", showWarnings = FALSE)

# Crear una base con los clientes de prueba.
clientes <- prueba

# Agregar la probabilidad estimada por el Logit.
clientes$probabilidad <- as.numeric(pred_logit)

# Verificar que las probabilidades sean validas.
stopifnot(
  nrow(clientes) == length(pred_logit),
  all(!is.na(clientes$probabilidad)),
  all(clientes$probabilidad >= 0),
  all(clientes$probabilidad <= 1)
)

# Mostrar las primeras observaciones.
head(clientes)


# 2. CALCULAR LA PROBABILIDAD PROMEDIO

# Calcular la probabilidad promedio de aceptacion.
probabilidad_promedio <- mean(clientes$probabilidad)

# Mostrar el resultado en porcentaje.
round(probabilidad_promedio * 100, 2)

# Calcular la tasa real de aceptacion.
aceptacion_real <- mean(clientes$acepta)

# Mostrar la tasa real en porcentaje.
round(aceptacion_real * 100, 2)


# 3. DEFINIR LOS SEGMENTOS COMERCIALES

# Calcular probabilidades sobre el conjunto de entrenamiento.
prob_entrenamiento <- predict(
  logit_controles,
  newdata = entrenamiento,
  type = "response"
)

# Calcular los percentiles 33 y 67 de entrenamiento.
limites <- quantile(
  prob_entrenamiento,
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

# Mostrar los limites utilizados.
print(limites)

# Crear segmentos segun la probabilidad estimada.
clientes$segmento <- ifelse(
  clientes$probabilidad >= limites[2],
  "Alta",
  ifelse(
    clientes$probabilidad >= limites[1],
    "Media",
    "Baja"
  )
)

# Ordenar los segmentos.
clientes$segmento <- factor(
  clientes$segmento,
  levels = c("Baja", "Media", "Alta")
)

# Mostrar cuantos clientes hay por segmento.
table(clientes$segmento)


# 4. ANALIZAR LA ACEPTACION POR SEGMENTO

# Contar los clientes de cada segmento.
cantidad <- as.numeric(table(clientes$segmento))

# Calcular la probabilidad promedio por segmento.
prob_media <- aggregate(
  probabilidad ~ segmento,
  data = clientes,
  FUN = mean
)

# Calcular la aceptacion real por segmento.
tasa_real <- aggregate(
  acepta ~ segmento,
  data = clientes,
  FUN = mean
)

# Crear una tabla resumen.
tabla_segmentos <- data.frame(
  Segmento = levels(clientes$segmento),
  Clientes = cantidad,
  Probabilidad_Promedio = round(
    prob_media$probabilidad * 100, 2
  ),
  Aceptacion_Real = round(
    tasa_real$acepta * 100, 2
  )
)

# Mostrar la tabla.
print(tabla_segmentos)

# Guardar los resultados.
write.csv(
  tabla_segmentos,
  "resultados/segmentacion_clientes.csv",
  row.names = FALSE
)


# 5. COMPARAR CARACTERISTICAS DE LOS CLIENTES

# Calcular la edad promedio por segmento.
edad_segmento <- aggregate(
  age ~ segmento,
  data = clientes,
  FUN = mean
)

# Calcular el saldo promedio por segmento.
saldo_segmento <- aggregate(
  balance ~ segmento,
  data = clientes,
  FUN = mean
)

# Calcular los contactos promedio por segmento.
contactos_segmento <- aggregate(
  campaign ~ segmento,
  data = clientes,
  FUN = mean
)

# Crear una tabla con las caracteristicas.
tabla_perfiles <- data.frame(
  Segmento = levels(clientes$segmento),
  Edad_Promedio = round(edad_segmento$age, 1),
  Saldo_Promedio = round(saldo_segmento$balance, 2),
  Contactos_Promedio = round(
    contactos_segmento$campaign, 2
  )
)

# Mostrar los perfiles.
print(tabla_perfiles)

# Guardar los perfiles.
write.csv(
  tabla_perfiles,
  "resultados/perfiles_clientes.csv",
  row.names = FALSE
)


# 6. GRAFICO DE ACEPTACION POR SEGMENTO

# Crear el grafico de aceptacion real.
grafico_segmentos <- ggplot(
  tabla_segmentos,
  aes(x = Segmento, y = Aceptacion_Real,
      fill = Segmento)
) +
  geom_col() +
  geom_text(
    aes(label = paste0(Aceptacion_Real, "%")),
    vjust = -0.5
  ) +
  labs(
    title = "Aceptacion bancaria por segmento",
    x = "Prioridad comercial",
    y = "Porcentaje de aceptacion real"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el grafico.
print(grafico_segmentos)

# Guardar el grafico.
ggsave(
  "resultados/grafico_segmentacion.png",
  plot = grafico_segmentos,
  width = 8,
  height = 5
)


# 7. GRAFICO DE PROBABILIDADES POR SEGMENTO

# Crear un grafico de cajas.
grafico_probabilidades <- ggplot(
  clientes,
  aes(x = segmento, y = probabilidad,
      fill = segmento)
) +
  geom_boxplot() +
  labs(
    title = "Probabilidades estimadas por segmento",
    x = "Segmento comercial",
    y = "Probabilidad de aceptacion"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar el grafico.
print(grafico_probabilidades)

# Guardar el grafico.
ggsave(
  "resultados/grafico_probabilidades.png",
  plot = grafico_probabilidades,
  width = 8,
  height = 5
)


# 8. IDENTIFICAR VARIABLES DEL MODELO LOGIT

# Extraer los coeficientes del modelo.
coeficientes <- summary(logit_controles)$coefficients

# Crear una tabla de coeficientes.
tabla_variables <- data.frame(
  Variable = rownames(coeficientes),
  Coeficiente = coeficientes[, 1],
  Valor_p = coeficientes[, 4],
  row.names = NULL
)

# Calcular los odds ratios.
tabla_variables$Odds_Ratio <- exp(
  tabla_variables$Coeficiente
)

# Identificar las variables significativas al 5%.
tabla_variables$Significativa <- ifelse(
  tabla_variables$Valor_p < 0.05,
  "Si",
  "No"
)

# Excluir el intercepto para el analisis.
tabla_variables <- tabla_variables[
  tabla_variables$Variable != "(Intercept)", ]

# Ordenar por valor p.
tabla_variables <- tabla_variables[
  order(tabla_variables$Valor_p), ]

# Mostrar los resultados.
print(tabla_variables)

# Guardar la tabla.
write.csv(
  tabla_variables,
  "resultados/variables_relevantes.csv",
  row.names = FALSE
)


# 9. GRAFICO DE VARIABLES SIGNIFICATIVAS

# Seleccionar los coeficientes significativos.
variables_significativas <- subset(
  tabla_variables,
  Significativa == "Si"
)

# Verificar si existen variables significativas.
if (nrow(variables_significativas) > 0) {
  
  # Crear el grafico de coeficientes.
  grafico_variables <- ggplot(
    variables_significativas,
    aes(
      x = reorder(Variable, Coeficiente),
      y = Coeficiente
    )
  ) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Coeficientes significativos del Logit",
      x = "Variable",
      y = "Coeficiente estimado"
    ) +
    theme_minimal()
  
  # Mostrar el grafico.
  print(grafico_variables)
  
  # Guardar el grafico.
  ggsave(
    "resultados/grafico_variables.png",
    plot = grafico_variables,
    width = 9,
    height = 7
  )
}


# 10. ANALIZAR EL GRUPO DE MAYOR PRIORIDAD

# Seleccionar clientes de prioridad alta.
clientes_alta <- subset(
  clientes,
  segmento == "Alta"
)

# Mostrar cuantos clientes pertenecen al grupo.
nrow(clientes_alta)

# Calcular su probabilidad promedio.
mean(clientes_alta$probabilidad)

# Calcular su aceptacion real.
mean(clientes_alta$acepta)

# Mostrar su edad promedio.
mean(clientes_alta$age)

# Mostrar su saldo promedio.
mean(clientes_alta$balance)

# Mostrar su numero promedio de contactos.
mean(clientes_alta$campaign)


# 11. COMPARAR LA SEGMENTACION CON EL PROMEDIO GENERAL

# Calcular la aceptacion real del grupo de alta prioridad.
tasa_alta <- mean(clientes_alta$acepta)

# Calcular la aceptacion real de toda la muestra de prueba.
tasa_general <- mean(clientes$acepta)

# Calcular la diferencia en puntos porcentuales.
diferencia_pp <- (tasa_alta - tasa_general) * 100

# Mostrar la diferencia.
round(diferencia_pp, 2)

# Calcular la razon entre ambas tasas.
lift_alta <- if (tasa_general > 0) {
  tasa_alta / tasa_general
} else {
  NA_real_
}

# Mostrar la razon de aceptacion.
round(lift_alta, 2)

# Crear una tabla de comparacion.
tabla_impacto <- data.frame(
  Indicador = c(
    "Aceptacion general (%)",
    "Aceptacion segmento alto (%)",
    "Diferencia (puntos porcentuales)",
    "Lift segmento alto"
  ),
  Valor = round(c(
    tasa_general * 100,
    tasa_alta * 100,
    diferencia_pp,
    lift_alta
  ), 2)
)

# Mostrar la tabla.
print(tabla_impacto)

# Guardar la comparacion.
write.csv(
  tabla_impacto,
  "resultados/impacto_segmentacion.csv",
  row.names = FALSE
)


# 12. CREAR TABLA PARA DECISIONES COMERCIALES

# Crear recomendaciones iniciales por segmento.
tabla_recomendaciones <- data.frame(
  Segmento = c("Alta", "Media", "Baja"),
  Estrategia = c(
    "Priorizar contacto comercial y evaluar oferta",
    "Realizar pruebas de campanas personalizadas",
    "Reducir contactos masivos y evaluar otros canales"
  )
)

# Mostrar las estrategias propuestas.
print(tabla_recomendaciones)

# Guardar las estrategias.
write.csv(
  tabla_recomendaciones,
  "resultados/recomendaciones_comerciales.csv",
  row.names = FALSE
)


# 13. GUARDAR RESULTADOS PARA REPRODUCIBILIDAD

# Guardar las predicciones y segmentos de la muestra.
write.csv(
  clientes,
  "resultados/clientes_segmentados.csv",
  row.names = FALSE
)

# Mostrar los archivos generados.
list.files("resultados")

# Mostrar un mensaje final.
print("Parte 5 ejecutada correctamente")
