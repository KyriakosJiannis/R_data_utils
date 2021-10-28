# Script to clear R environment 

# rm plots 
dev.off()   
# graphics.off() 

# rm workplace
rm(list = ls())  

# clear console
# ctrl+l
# clr <- function(){cat(rep("\n", 50))}
cat("\014")
