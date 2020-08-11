#' Replaces all non alpha-numerical symbols in dataframe titles with a given Symbol
#' 
#' @param ... the dataframe, symbol to change
#' @param .inData Data frame identifier.
#' @param .Symbol: character for the synbol to use
#' @param .Print:  prints before and after the execution the dataframe titles
#' @return data.frame
#' @examples
#' Names_____ <- c('John Lee','Avramm Johnson','Ioannis Flakas')
#' height__in__..._cm____ <- c(185, 180, 195)
#' df <- data.frame(Names_____, height__in__..._cm____)
#' df <- CleanTitles(df,"_")

########################################################################################################################
##### Common function: CleanTitles
##### Authors: Ioannis Kyriakos
##### Date: February 2nd, 2018
##### Description: Replaces all non alpha-numericals symbols in data.frame titles with a symbol
########################################################################################################################

# TODO tests for vectors and variables

CleanTitles <- function (inData, Symbol, Print = TRUE) {

  # Validate inputs
  if (is.data.frame(inData)) {
    #message("CleanTitles function replaced dataframe titles names")
  }  else {
    stop("Input has to be a dataframe")
  }
  
  # Execute statement
  if (Print == TRUE) {
    message('From:')
    print(names(inData)) # print df titles before changes
  }
  
  names(inData) <- gsub("[^a-zA-Z0-9]", Symbol, names(inData))  # Replace all non alpha-numericals
  names(inData) <- gsub(paste("[", Symbol, "]", "+", sep = ""), Symbol, names(inData))  # Replace dublicated
  names(inData) <- gsub(paste("^[", Symbol, "]|[", Symbol, "]$", sep = ""), "", names(inData)) # Remove leading/trailing
  
  # Output
  outData <- inData
  if (Print == TRUE) {
    message('to:')
    print(names(outData)) # print final df titles
  }
  #message('successfully executed')
  
  return(outData)
}

