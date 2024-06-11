if (!('handles' %in% ls())) { handles <- list() }


handles$getPlotAccoount <- function(id, dat) {
    
    
    require(ggplot2)
    require(RColorBrewer)
    
    #################################################
    
    nameYlab <- id
    
    #################################################
    
    p <- 
        dat %>% 
        ggplot(aes(x=FECHA, y=eval(as.name(id)), color=TIPO_DE_ENTIDAD)) + 
        geom_line(size=1)
       
    
    p <- 
        p + 
        theme_bw() +
        ylab(nameYlab) +
        xlab('') +
        theme(legend.position="bottom", legend.text=element_text(size=8)) + 
        guides(color=guide_legend(ncol=3, title="")) + 
        scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
    
    
    # If TIPO_DE_ENTIDAD is one
    if ( length(unique(dat$TIPO_DE_ENTIDAD))==1 ) {
        
        p <- 
            p + 
            scale_color_manual(values=c( sample( c(brewer.pal(8,"Dark2"), 
                                                   brewer.pal(8,"Accent")[5:7],
                                                   brewer.pal(8,"Set1") ) , size = 1) )) +
            theme(legend.position = "none")
        
    }
    
    
    return(p)
}
