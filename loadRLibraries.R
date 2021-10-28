######################################################################################################################################################
##### Common function: loadRLibraries
##### Authors: Ioannis
##### Date: April 13th, 2018
##### Description: checks if libraries are active, and load only inactive ones
#####              checks if libraries have been installed, and installed them 
#####################################################################################################################################################

loadRLibraries <- function(packages) {
  # inputs
  #     packages: string vector with libraries
  #---------------------------------------------------------------------------------
  # e.g.
  # packages <- c("tidyverse","PerformanceAnalytics", "caret","B2Z", "aurelius")
  # loadRLibraries(packages)
  
  # browser()
  
  # searching packages
  search.packages <- search() 
  search.packages <- search.packages[
    grepl(pattern = "package:", x = search.packages)]
  
  if(!require("tm", character.only = FALSE)) {
    install.packages("tm")}
  
  library(tm)
  search.packages <- removeWords(search.packages, "package:")
  
  # returns the additional libraries
  packages <- setdiff(packages, search.packages)
  
  # load the libraries
  if (length(packages) > 0) {
    
    Install.Package <- function(packages) {
      # browser()
      unavailable <- setdiff(packages, rownames(installed.packages()))
      if (length(unavailable) > 0) {install.packages(unavailable)}
    }
    
    Install.Package(packages)
    
    for (ix in 1:length(packages)) {library(packages[ix] , character.only = TRUE)}
    #NonPrint <- sapply(packages, library)
    }
}


#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(XML,RCurl,rlist,xlsx)