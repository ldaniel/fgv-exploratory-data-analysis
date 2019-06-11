# setting the environment -----------------------------------------------------
options(encoding = "UTF-8")

username <- Sys.info()[["user"]]
if(tolower(username) == "daniell") {
  setwd("M:/Leandro/FGV/02 - Analise Exploratoria de Dados/Trabalho Final")
} else if (tolower(username) == "leandro") {
  setwd("D:/OneDrive/FGV/04_Modulos/02 - Analise Exploratoria de Dados/TrabalhoFinal") 
} else if (tolower(username) == "a57622988") {
  setwd("C:/Users/A57622988/Desktop/Aula") 
} else {
  setwd("~/") 
}
getwd()