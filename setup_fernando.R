#Setup Fernando
library(tidyverse)
library(scales)
library(ggthemes)
library(grid)
library(gridExtra)
library(knitr)
library(RColorBrewer) # visualisation
library(corrplot) # visualisation
library(lubridate)

# modelling
library(caret) # modelling
library(MLmetrics) # gini metric
library(Information) #IV
library(InformationValue) #KS

Sys.setenv(TZ = "UTC") #Define Timezone
Sys.setenv(ORA_SDTZ = "UTC") #Define Oracle timezone


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# function to extract binomial confidence levels
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))


# ks plot
ks_plot <- function (actuals, predictedScores) {
  rank <- 0:10
  ks_table_out <- InformationValue:::ks_table(actuals = actuals, predictedScores = predictedScores)
  perc_positive <- c(0, ks_table_out$cum_perc_responders) * 100
  perc_negative <- c(0, ks_table_out$cum_perc_non_responders) * 100
  random_prediction <- seq(0, 100, 10)
  df <- data.frame(rank, random_prediction, perc_positive, perc_negative)
  df_stack <- stack(df, c(random_prediction, perc_positive, perc_negative))
  df_stack$rank <- rep(rank, 3)
  df_stack$delta <- df_stack$values[12:22] - df_stack$values[1:11]
  values <- df_stack$values
  ind <- df_stack$ind
  
  rowmax <- which.max(ks_table_out$difference)
  l_start <- ks_table_out[rowmax, "cum_perc_non_responders"]
  l_end <- ks_table_out[rowmax, "cum_perc_responders"]
  
  print(ggplot2::ggplot(df_stack, aes(x = rank, y = values, 
                                      colour = ind, label = paste0(round(values, 2), "%"))) + 
          geom_line(size = 1.25) + 
          labs(x = "rank", y = "Percentage +Ve & -Ve Captured", 
               title = "KS Chart", subtitle=paste("KS Statistic: ", ks_stat(actuals, predictedScores))) + 
          theme(plot.title = element_text(size = 20, 
                                          face = "bold")) + 
          geom_text(aes(y = values + 4)) + 
          scale_x_continuous(breaks=0:10, labels=0:10) + 
          geom_segment(x = rowmax, y = l_start*100, xend = rowmax, yend = l_end*100, col="red", arrow = arrow(length = unit(0.05, "npc"), ends="both"), linetype = "dashed", lwd=1))
}
