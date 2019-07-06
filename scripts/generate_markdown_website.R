# getting the necessary directories to generate the website -------------------
scriptsDirectoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
markdownDirectoryPath <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/markdown")
docsDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/docs")
tempSiteDirecoryPath  <- paste(markdownDirectoryPath, "/_site", sep = "")
rootDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "")

# generating website from markdown files --------------------------------------
setwd(markdownDirectoryPath)
rmarkdown::render_site()

# moving website files to docs directory --------------------------------------
if(file.exists(docsDirectoryPath))
{
  unlink(docsDirectoryPath, recursive=TRUE)
  file.remove(docsDirectoryPath)
}

if(file.exists(tempSiteDirecoryPath))
{
  file.rename(from = tempSiteDirecoryPath,  to = docsDirectoryPath)
}

# setting the working directory to the original one ---------------------------
setwd(rootDirectoryPath)
getwd()
