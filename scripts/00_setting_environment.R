# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
if(tolower(username) == "daniell") {
  setwd("M:/Leandro/FGV/02 - Analise Exploratoria de Dados/Trabalho Final")
} else if (tolower(username) == "leandro") {
  setwd("D:/OneDrive/FGV/04_Módulos/02 - Análise Exploratória de Dados/TrabalhoFinal")
}
getwd()