
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
