if (!('handles' %in% ls())) { handles <- list() }

handles$getListPlotsAccoounts <- function(listResult, scaleOne=FALSE, numDigScale=NULL, legend_element_text =12, x_y_element_text=12 , ids=NULL, datIdsNames=NULL) {
    
    require(ggplot2)
    require(scales)
    
    listPlots <- list()
    
    if (is.null(ids)) {
        n <- length(listResult)
        ids <- names(listResult)
    }else{
        n <- length(ids)
    }
    
    for (i in seq(n)) {
        
        id <- listResult[[ ids[i] ]][['id']]
        p <- listResult[[ ids[i] ]][['p']]
        
        ###################################################################
        #### Y LAB
        
        if (!is.null(datIdsNames)) {
            
            if (id %in% datIdsNames$INDICADOR) {
                
                nameY_lab <- 
                    datIdsNames %>% 
                    filter(INDICADOR==id) %>% 
                    select(NOMBRES) %>% 
                    pull() %>% 
                    toupper()
            } else {
                
                nameY_lab <- id
                
            }

            
        } else {
            
            nameY_lab <- id
        }
        
        ###################################################################
        
        if (i==n) {
            
            p <- 
                p + 
                ylab(nameY_lab) +
                theme(legend.text = element_text(size = legend_element_text),
                      legend.key.size = unit(1, 'cm'),
                      axis.title.x=element_text(size = x_y_element_text),
                      axis.title.y = element_text(size = x_y_element_text),
                      axis.text.y=element_text(size = x_y_element_text),
                      axis.text.x = element_text(size = x_y_element_text))
            
        } else {
            p <- 
                p +
                ylab(nameY_lab) +
                theme(legend.position = "none",
                      axis.title.x=element_blank(),
                      axis.title.y = element_text(size = x_y_element_text),
                      axis.text.y=element_text(size = x_y_element_text),
                      axis.text.x=element_blank())
        }
        
        if (scaleOne) {
            p <- p + scale_y_continuous(labels = label_number(accuracy = 1))
        }
        
        if (!is.null(numDigScale)) {
            p <- p + scale_y_continuous(labels = label_number(accuracy = numDigScale))
        }
        
        listPlots[[id]] <- p
    }
    
    return(listPlots)
}

