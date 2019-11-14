# Script to clear R enviroment 

# rm plots 
dev.off()   
# graphics.off() 

# rm workspace
rm(list = ls())  

# clear console
# ctrl+l
# clr <- function(){cat(rep("\n", 50))}
cat("\014")
