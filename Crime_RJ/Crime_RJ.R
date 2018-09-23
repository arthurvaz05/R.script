library(readr)
crimerj <- read_delim("BaseDPEvolucaoMensalCisp.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

delegacias <- read_delim("delegacias.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

popmes <- read_delim("PopulacaoEvolucaoMensalCisp.csv", 
                                          ";", escape_double = FALSE, trim_ws = TRUE)

