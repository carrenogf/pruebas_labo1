# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(442367, 471089, 966109, 968879, 975899)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)
  
  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
                  data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
                  xval = 0,
                  control = param_basicos
  ) # aqui van los parametros del arbol
  
  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
                        dataset[fold == 2], # fold==2  es testing, el 30% de los datos
                        type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad
  
  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades
  
  
  # calculo la ganancia en testing  qu es fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
               ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
               0
    ))
  ]
  
  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3
  
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
                        semillas, # paso el vector de semillas
                        MoreArgs = list(param_basicos), # aqui paso el segundo parametro
                        SIMPLIFY = FALSE,
                        mc.cores = 1 # en Windows este valor debe ser 1
  )
  
  ganancia_promedio <- mean(unlist(ganancias))
  
  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\repos\\lab1-mcd-austral\\rpart\\") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("c:\\Users\\Usuario\\Documents\\Universidad\\austral\\2024\\lab1\\competencia\\datasets\\dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( cp = integer(),
                              max_depth = integer(),
                              min_split = integer(),
                              min_bucket = integer(),
                              ganancia_promedio = numeric() )


i <- 1


PARAM$semillas[1]

# itero por los loops anidados para cada hiperparametro
for ( vcp in c(-10, -9, -8,-7,-6,-5,-4,-3,-2,-1,-0.5, -0.25) )
{
  for (vmax_depth in c(5, 6, 7, 8, 9, 10, 11)) {
    for (vmin_split in c(  10, 20, 30,40, 50,60, 70, 80, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1500, 2000 )) 
    {
      for( vmin_bucket in seq(vmin_split, vmin_split/2, by = -100) ) 
      {
        
        
        # vminsplit  minima cantidad de registros en un nodo para hacer el split
        param_basicos <- list(
          "cp" = vcp, # complejidad minima
          "minsplit" = vmin_split,
          "minbucket" = vmin_bucket, # minima cantidad de registros en una hoja
          "maxdepth" = vmax_depth
        ) # profundidad máxima del arbol
        
        
        cat("Corrida n°: " ,i, "\n")
        # Un solo llamado, con la semilla 17
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas[1], param_basicos)
        
        #cat("Ganancia_promedio: ", ganancia_promedio, "\n", vcp, vmin_split, vmin_bucket, vmax_depth)
        cat("Ganancia_promedio: ", ganancia_promedio, "\n")
        
        
        i <- i + 1
        
        # agrego a la tabla
        tb_grid_search <- rbindlist( 
          list( tb_grid_search, 
                list( vcp, vmax_depth, vmin_split, vmin_bucket, ganancia_promedio) ) )
      }
    }
    
    # escribo la tabla a disco en cada vuelta del loop mas externo
    Sys.sleep(2)  # espero un par de segundos
    
    fwrite( tb_grid_search,
            file = archivo_salida,
            sep = "\t" )
  }
}