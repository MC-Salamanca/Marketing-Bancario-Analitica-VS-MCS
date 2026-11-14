
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
