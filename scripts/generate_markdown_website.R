# getting the necessary directories to generate the website -------------------
scriptsDirectoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
markdownDirectoryPath <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/markdown")
docsDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/docs")
tempSiteDirecoryPath  <- paste(markdownDirectoryPath, "/_site", sep = "")

# generating website from markdown files --------------------------------------
setwd(markdownDirectoryPath)
rmarkdown::render_site()

# moving website files to docs directory --------------------------------------
filesstrings::file.move(paste(tempSiteDirecoryPath, "/*.*", sep = ""), docsDirectoryPath)
filesstrings::dir.remove(tempSiteDirecoryPath)

# setting the working directory to the original one ---------------------------
source("./scripts/step_00_config_environment.R")