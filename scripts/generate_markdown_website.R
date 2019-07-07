# getting the necessary directories to generate the website -------------------
scriptsDirectoryPath  <- dirname(rstudioapi::getSourceEditorContext()$path)
markdownDirectoryPath <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/markdown")
docsDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "/docs")
tempSiteDirecoryPath  <- paste(markdownDirectoryPath, "/_site", sep = "")
rootDirectoryPath     <- stringr::str_replace(scriptsDirectoryPath, "/scripts", "")

# generating website from markdown files --------------------------------------
setwd(markdownDirectoryPath)

initialTime <- Sys.time() # get the initial time
rmarkdown::render_site()  # call the website generation function
finalTime <- Sys.time()   # get the final time

# showing the amount of time, in minutes, to generate the website
print(difftime(finalTime, initialTime, tz="GMT", units="mins")) 

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
