#' Convert data.frame variables to other types with dplyr: mutate_if
#' 
#' @param ... the dataframe to convert characters to factors or factos to characters
#' @param .inData Data frame identifier.
#' @param .convert: character 
#'              Factor2Char: Converts all factor variables to characters
#'              Char2Factor: Converts all characters variables to factors
#'      
#'  @return data.frame
#'  @seealso dplyr::mutate_if
#'  @examples
#'  Iris <- Convert(iris, "Factor2Char")

######################################################################################################################################################
##### Common function: Convert
##### Authors: Ioannis Kyriakos
##### Date: March 29th, 2018
##### Description: Convert data.frame variables to other types
#####################################################################################################################################################

Convert <- function (inData, convert ) {
  require(dplyr)
  # browser()
  
  # Validate inputs 
  if (is.data.frame(inData)) {
    message("Converts data.frame variables to other types")
  }  
  else {
    stop("inData needs to be a dataframe")
  }
  
  # print str  import
  message("Structure of input: "); print(str(inData))
  
  # Function Statements
  if (convert == "Factor2Char") {
    message("Factors to Characters")
    
    ConvertFun <- function (inData) {
      inData <- inData %>% dplyr::mutate_if(is.factor,as.character)
    }
    
  } else if (convert == "Char2Factor")  {
    message("Characters to Factors")
    ConvertFun <- function (inData) {
      inData <- inData %>% dplyr::mutate_if(is.character,as.factor)
    }
    
  } else {
    stop("parameter does not exist")
  }
  
  # Execute ConvertFun
  outData <- ConvertFun(inData)
  
  # Print str of output
  message("Structure of output:"); print(str(outData))
  return(outData)
}

