plot.fun <- function(ds, plotvar,maintitle='', quantset=NA,cutset=NA, subtitle='', plot.cuts='quantile',roundscale=-2, statevec=state.abb, scaletitle='Rate'){
  
  if(plot.cuts=='quantile'){
    ds$cuts <- as.integer(cut(ecdf(ds[,plotvar])(ds[,plotvar]), seq(0,1,.01)))
    quants <- quantile(ds[,plotvar])
    plotlabs <- c(as.character(''), 
                  rep('', 23),  
                  as.character(round(quants[2],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[3],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[4],roundscale)),
                  rep('', 24),  
                  as.character(''))
    
  }else if(plot.cuts=='weighted_quantile'){
    ds$cuts <- as.integer(cut(ewcdf(ds[,plotvar], weights=ds[,'pop_2015'])(ds[,plotvar]), seq(0,1,.01)))
    
    
    quants <- weighted.quantile(ds[,plotvar], w=ds[,'pop_2015'], probs=seq(0,1,0.25), na.rm = TRUE)
    plotlabs <- c(as.character(''), 
                  rep('', 23),  
                  as.character(round(quants[2],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[3],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[4],roundscale)),
                  rep('', 24),  
                  as.character(''))
  }else if(plot.cuts=='precalc'){
    quants <- quantset
    
    
  }else{
    ds[,plotvar] <- ds[,plotvar]/max(ds[,plotvar])*100  
    quants <- quantile(ds[,plotvar])
    
    ds$cuts <- ds[,plotvar]
    
    plotlabs <- c(as.character(round(quants[1],roundscale)), 
                  rep('', 23),  
                  as.character(round(quants[2],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[3],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[4],roundscale)),
                  rep('', 24),  
                  as.character(round(quants[5],roundscale)))
  }
  
  
  p1 <- plot_usmap(include = c(statevec),data= ds, values='cuts' ,regions = "counties", color='NA') + 
    labs(title = maintitle,
         subtitle = subtitle) + 
    theme(panel.background = element_rect(color = "white", fill = "white")) +
    #scale_fill_gradient(scale_name='Rate', breaks=c(1:3), labels=c("Minimum",0.5,"Maximum"))
    
    
    scale_fill_stepsn(scaletitle, breaks= 1:100, limits = c(0,100), labels=plotlabs,
                      colors=viridis_pal()(10),
                      guide = guide_colorsteps(even.steps = FALSE))
  
  return(p1)
}