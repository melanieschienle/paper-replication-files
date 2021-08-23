plot_fct <- function (first, second, y_scale1, y_scale2, outlier, plots, pl_name){
  
  pdf(str_c("plots/", pl_name, ".pdf"), width=8, height=14)
  
  par( mfrow = c(length(plots), 2),
       oma = c(5,4,0,0) + 0.1,
       mar = c(0,0,1,1) + 0.1)
  
  for( k in 1:length(plots) ){
    
    series1 <- list()
    series2 <- list()
    for( l in 1:length(plots) ){
      if( l == k ){
        series1[[ plots[l] ]] <- c(1000,1000)  # fake plot for diagonal element
        series2[[ plots[l] ]] <- c(1000,1000)  # fake plot for diagonal element
      } else {
        series1[[ plots[l] ]] <- first[ ,l+(k-1)*length(plots)]
        series2[[ plots[l] ]] <- second[ ,l+(k-1)*length(plots)]
      }
      
    }
    
    boxplot(series1,
                outline = outlier,
                ylab    = plots[k],
                xaxt    = if( k != length(plots) ){'n'}, # labels only in last plot
                at      = 1:length(plots),
                ylim    = y_scale1,
                xlim    = c(0.7,(length(plots)+0.3)),
                names   = plots,
                border  = "brown")
    
    boxplot(series2,
                outline = outlier,
                ylab    = plots[k],
                yaxt    = 'n',
                xaxt    = if( k != length(plots) ){'n'}, # labels only in last plot
                at      = 1:length(plots),
                ylim    = y_scale2,
                xlim    = c(0.7,(length(plots)+0.3)),
                names   = plots,
                border  = "brown")
  }
  
  par()
  dev.off()

}
