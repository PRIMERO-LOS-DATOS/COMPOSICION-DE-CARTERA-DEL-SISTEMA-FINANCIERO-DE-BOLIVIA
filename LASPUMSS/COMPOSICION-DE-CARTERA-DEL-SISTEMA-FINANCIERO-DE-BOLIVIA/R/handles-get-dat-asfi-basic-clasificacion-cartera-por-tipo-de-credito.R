if (!('handles' %in% ls())) { handles <- list() }
#
#
# CARACTERÍSTICAS:
#   - No depende de otras funciones.

handles$getDatAsfiBasic_CalificacionPorTipoCredito <- function(yearInit = 2014, byClass = NULL, byEnts = NULL) {
    
    require(openxlsx)
    require(dplyr)
    
    rootFile <- 'FUENTES-DATOS/ESTADOS-FINANCIEROS-ASFI-WEB/data/COLOCACIONES/BBDD_CLASIFCARTERATIPOCREDITO.xlsx'
    
    
    dat <- openxlsx::read.xlsx(rootFile)
    
    print(unique(dat$TIPO_DE_ENTIDAD))
    
    ###################################
    # ARREGLAR NOMBRE DE COLUMNAS
    namesDat <- gsub( pattern = 'ENTIDIDAD', replacement = 'ENTIDAD', names(dat) )
    names(dat) <- namesDat
    
    ###################################
    
    dat <- 
        dat %>% 
        mutate(GESTION = as.numeric(GESTION),
               MES = as.numeric(MES),
               DIA = as.numeric(DIA)) %>% 
        filter(GESTION>=yearInit)
    
    ###################################
    
    dat <- 
        dat %>% 
        mutate( FECHA = as.Date(paste0(GESTION, '-', MES, '-', DIA)) )
    
    ###################################
    
    dat <- 
        dat %>% 
        mutate(GESTION = as.character(GESTION),
               MES = as.character(MES),
               DIA = as.character(DIA))
    
    
    datEMB <- 
        dat %>% 
        filter(EXPRESADO=='EN_MILES_DE_BOLIVIANOS') %>% 
        mutate(across(.cols = where(is.numeric), ~ .x * 1000 ))
    
    datEB <- 
        dat %>% 
        filter(EXPRESADO=='EN_BOLIVIANOS')
    
    dat <- 
        bind_rows(datEMB,datEB)
    
    dat <- 
        dat %>% 
        mutate( EXPRESADO = rep('EN_BOLIVIANOS', n()) )
    
    
    ###################################
    
    dat <- 
        dat %>% 
        replace(is.na(.), 0)
    
    rm(datEB)
    rm(datEMB)
    
    
    ####################################
    # REDUNDANTES
    
    redundancias <- c('SBA','SIF','SEF','SBY','SSP','SCO')
    
    dat <- 
        dat %>% 
        filter(!(ENTIDAD %in% redundancias))
    
    gc()
    
    ###################################
    # FILTER DOS
    if (!is.null(byClass)) {
        dat <- 
            dat %>% 
            filter(TIPO_DE_ENTIDAD == byClass)
    }
    
    if (!is.null(byEnts)) {
        dat <- 
            dat %>% 
            filter(ENTIDAD %in% byEnts)
    }
    
    
    ###################################
    # AGREGAR EL CAMPO TOTAL CARTERA
    dat <- 
        dat %>% 
        mutate(TOTAL_CARTERA = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
    
    return(dat)
}
