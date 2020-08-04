######################################################################################################################################################
##### function: CorrCharts
##### Authors: Ioannis
##### Date: March 20th, 2018
##### Description: Vizualize correlations in a dataframe 
#####################################################################################################################################################

CorrCharts <- function(inData, Colours = NULL) {
  # Provides some basic corretion plots
  #
  # inputs
  #     inData: data.frame
  #     Colours: string,  Add variable name for to viz cor chart for groups 
  # outputs
  #     outData: correlation charts
  #              
  #---------------------------------------------------------------------------------
  # eg Desc_out <- CorrCharts(iris, Colours ="Species")
  
  #browser()
  
  # setwd(file.path("P:","R","CodeRepository"))
  
  # load R-libraries
  # source(file.path(".", "Common.data", "loadRLibraries.R")) 
  #source(file.path("P:","R","CodeRepository", "Common.data", "loadRLibraries.R")) 
  
  packages <- c("GGally","tidyverse","PerformanceAnalytics","corrplot")
  loadRLibraries(packages)
  
  
  inData <-  as.data.frame(inData)
  # index for numerical vars
  ind <- sapply(inData, is.numeric)
  
  if (sum(ind) > 0) {
    
    # ggpairs basic corralation chart
    if (is.null(Colours)) {
      print(ggpairs(inData,
                    title = "Correlation Charts"))
      
      print(ggpairs(inData,
                    upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
                    lower = list(continuous = ggally_points, combo = ggally_dot_no_facet),
                    title = "Correlation Charts"))
    }
    else {
      print(ggpairs(inData,
                    mapping = ggplot2::aes_string(colour=Colours),
                    title = "Correlation Charts"))
      
      
      print(ggpairs(inData,
                    mapping = ggplot2::aes_string(colour=Colours),
                    upper = list(continuous = ggally_density, combo = ggally_box_no_facet),
                    lower = list(continuous = ggally_points, combo = ggally_dot_no_facet),
                    title = "Correlation Charts"))
    }
    
    # Viz cor matrix  
    ggcorr(inData[ind], 
           # method = c("pairwise", "pearson"),
           palette = "RdBu",
           label = TRUE,
           label_round = 3) + ggtitle("Pearson's correlation")
    
    # Correlations charts 
    chart.Correlation(inData[ind], histogram = TRUE, method = c("pearson"))
    
    # Charts using corrplot
    CorMatrix <- cor(inData[ind], use = "pairwise.complete.obs")
    p.CorMatrix <- cor.mtest(inData[ind])
 
    corrplot(CorMatrix, 
             method="color",
             type="upper", # order="hclust",
             tl.col="black", tl.srt = 45,
             diag = FALSE)
    
    corrplot(CorMatrix, 
             method="color",  
             type="upper", # order="hclust", 
             addCoef.col = "black",
             tl.col="black", tl.srt = 45,
             p.mat = p.CorMatrix$p, sig.level = 0.01, insig = "blank", 
             diag = FALSE 
    )
  }
 # setwd <- CurrentDir
}

  