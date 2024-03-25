rm(list = ls())
# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# funcion para particionar
particionar <- function(
    data, division, agrupa = "",
    campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

setwd("G:/Mi unidad/Austral - Ciencia de datos/Laboratorio 1/Labo1Fran")
# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar

# particiono el datatrain
particionar(dtrain, division = c(7, 3), 
            agrupa = "clase_ternaria", seed = 606323) # aqui se usa SU semilla


# este es el dataset a predecir, no tiene clase ternaria
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# calculo la columna ganancia para el datatrain
dtrain[, ganancia := ifelse(clase_ternaria == "BAJA+2", 117000, -3000)]

# lista de hiperparametros
hp <- list(
  minbucket = c(40,200,500,1000,2500,5000,10000),    # Mínimo de observaciones en cualquier nodo terminal
  maxdepth = seq(3,10)
  )

# entreno los modelos con diferentes hiperparametros con el dtrain

modelos <- c()
m <- 1
for (i in hp$minbucket){
  for (j in hp$maxdepth){
    modelo <- rpart(
      formula = "clase_ternaria ~ .",
      data = subset(dtrain,select = -c(ganancia))[fold == 1], # los datos donde voy a entrenar
      xval = 0,
      cp = -0.3, # esto significa no limitar la complejidad de los splits
      minsplit = i*2, # minima cantidad de registros para que se haga el split
      minbucket = i, # tamaño minimo de una hoja
      maxdepth = j) # profundidad maxima del arbol

    modelos[[m]] <- modelo
    m <- m+1
    
  }
}

# ahora hago la prediccion y calculo la ganancia con el test
resultados <- c()
k <- 1
for (mod in modelos){
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    object = mod,
    newdata = subset(dtrain,select = -c(ganancia))[fold == 2], #testset
    type = "prob"
  )
  dt2 <- dtrain
  dt2 <- dt2[fold == 2, prob_baja2 := prediccion[, "BAJA+2"]]
  ganancia_test <- dt2[prob_baja2 > 0.025, sum(ganancia)]
  resumen_modelo <- summary(mod)
  resultados[[k]] <- list(
    "ganancia_test" = ganancia_test,
    "resumen" <- resumen_modelo
  )
  k <- k+1
  
}
ganancias <- c()
l <- 1
for (resultado in resultados){
  ganancias[[l]] <- list(
    "ganancia" = resultado$ganancia_test,
    "minbucket"=resultado[[2]]$control$minbucket,
    "minsplit"=resultado[[2]]$control$minsplit,
    "maxdepth"=resultado[[2]]$control$maxdepth
    )
  l <- l+1
  
}
dataframe <- do.call(rbind, ganancias)
dataframe <- as.data.frame(dataframe)

# guardar en excel
library(openxlsx)
write.xlsx(dataframe, "result_modelos.xlsx")

# elegir modelo

selected <- dataframe[26,]
selected
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = subset(dtrain,select = -c(fold,ganancia,prob_baja2)), # los datos donde voy a entrenar
  xval = 0,
  cp = -0.3, # esto significa no limitar la complejidad de los splits
  minsplit = selected$minsplit, # minima cantidad de registros para que se haga el split
  minbucket = selected$minbucket, # tamaño minimo de una hoja
  maxdepth = selected$maxdepth
) # profundidad maxima del arbol


summary(modelo)
# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)


# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
try(dir.create("./exp/"))
try(dir.create("./exp/KA2001"))

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_002.csv",
       sep = ","
)

