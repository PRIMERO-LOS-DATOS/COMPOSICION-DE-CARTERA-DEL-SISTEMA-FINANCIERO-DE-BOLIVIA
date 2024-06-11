if (!('handles' %in% ls())) { handles <- list() }
#
#
# CARACTERÍSTICAS:
#   - Depende de otras funciones.

handles$getDatEstFinByGroupAsfi_CalificacionPorTipoCredito <- function(dat=NULL, by='ENTIDAD', yearInit = 2014, byClass = NULL, byEnts = NULL) {
    
    #rootRSourse <- 'ALGOTIMOS-MODELOS/HANDLES'
    #sapply( list.files(rootRSourse, full.names=TRUE, pattern='\\.[rR]$'), source )
    
    require(dplyr)
    
    # Verificar si "dat" es nulo
    
    if (is.null(dat)) { 
        dat <- handles$getDatAsfiBasic_CalificacionPorTipoCredito(yearInit = yearInit, 
                                                                  byClass = byClass, 
                                                                  byEnts = byEnts) 
    }
    
    # Agrupar por entidades  
    if (by=='ENTIDAD') {
        
        datResult <- 
            dat %>%
            mutate(TIPO_DE_ENTIDAD = ENTIDAD) %>% 
            select(-GESTION, -MES, -DIA, -ENTIDAD,-EXPRESADO,-ID)
    }
    
    # Agrupar por sectores 
    if (by=='TIPO_DE_ENTIDAD') {
        
        datResult <- 
            dat %>% 
            group_by(TIPO_DE_ENTIDAD, FECHA) %>% 
            summarise_if(is.numeric, sum) %>% 
            relocate(FECHA, .before = 1) 
        
        if ('GESTION' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-GESTION)
        }
        
        if ('MES' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-MES)
        }
        
        if ('DIA' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-DIA)
        }
        
    }
    
    
    # Agrupar por total sistema 
    if (by=='TOTAL_SISTEMA') {
        
        datResult <- 
            dat %>% 
            group_by(FECHA) %>% 
            summarise_if(is.numeric, sum) %>% 
            mutate(TIPO_DE_ENTIDAD=rep('TOTAL_SISTEMA',n() )) %>%
            relocate(TIPO_DE_ENTIDAD, .before = 2) 
        
        if ('GESTION' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-GESTION)
        }
        
        if ('MES' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-MES)
        }
        
        if ('DIA' %in% names(datResult)) {
            datResult <- datResult %>% 
                select(-DIA)
        }
        
        
    }
    
    return(as.data.frame(datResult))
}
