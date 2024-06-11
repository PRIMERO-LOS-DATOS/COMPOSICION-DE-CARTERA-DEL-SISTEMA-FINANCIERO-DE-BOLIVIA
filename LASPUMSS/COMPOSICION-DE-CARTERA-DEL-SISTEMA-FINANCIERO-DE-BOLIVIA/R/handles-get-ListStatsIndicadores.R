if (!('handles' %in% ls())) { handles <- list() }

handles$getListStatsOverviewByIdsNames <- function(ids, datNorm, datIdsNames=NULL, roundInd=FALSE) {
    
    require(dplyr)
    
    listResult <- list()
    
    if (is.null(datIdsNames)) {
        datIdsNames <- handles$getDatIdsNamesCamelIndicadores()
    }
    ####################################################################
    
    for (i in 1:length(ids)) {
        
        id <- ids[i]
        
        if(id %in% datIdsNames$INDICADOR){
            
            nameIndById <- 
                datIdsNames %>% 
                filter(INDICADOR==id) %>% 
                select(NOMBRES) %>% 
                pull()
            
            idsDecreasing <- 
                datIdsNames %>% 
                filter(INDICADOR==id) %>% 
                select(DECRECIENTE) %>% 
                pull()
            
        } else {
            
            nameIndById <- id
            idsDecreasing <- TRUE
            
        }
        
        
        ####################################################################
        
        datTrendInd <- 
            handles$getDatStatsOverviewByNameColumn(id = id, 
                                                    datNorm = datNorm, 
                                                    roundInd = roundInd, 
                                                    idsDecreasing = idsDecreasing)
     
        
        ####################################################################
        
        nameEntBestPromedio <- 
            datTrendInd %>% 
            select(TIPO_DE_ENTIDAD) %>% 
            slice(1L) %>% 
            pull()
        
        bestPromedio <- 
            datTrendInd %>% 
            select(PROMEDIO) %>% 
            slice(1L)  %>% 
            pull()
        
        p <- handles$getPlotAccoount(id, datNorm)
        
        ####################################################################
        
        listResult[[id]] <- 
            list(id=id,
                 nameIndById=nameIndById,
                 idsDecreasing=idsDecreasing,
                 datTrendInd=datTrendInd,
                 nameEntBestPromedio=nameEntBestPromedio,
                 bestPromedio=bestPromedio,
                 p=p)
    }
    
    
    return(listResult)
}
