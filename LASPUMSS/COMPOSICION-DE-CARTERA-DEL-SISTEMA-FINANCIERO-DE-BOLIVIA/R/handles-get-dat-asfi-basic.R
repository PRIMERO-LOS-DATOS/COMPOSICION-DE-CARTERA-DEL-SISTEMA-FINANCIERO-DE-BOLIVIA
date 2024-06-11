if (!('handles' %in% ls())) { handles <- list() }
#
#
# CARACTERÍSTICAS:
#   - No depende de otras funciones.

handles$getDatEEFF <- function(gestionIncial=2014) {
    
    require(openxlsx)
    require(dplyr)

 
    rootFile <- 'FUENTES-DATOS/ESTADOS-FINANCIEROS-ASFI-WEB/data/BBDD_ESTADOS_FINANCIEROS_ASFI_WEB_____ESTADOS_FINANCIEROS.xlsx'
    dat <- read.xlsx(rootFile)

    dat$FECHA <- convertToDate(dat$FECHA)
    
    dat$TIPO_DE_ENTIDAD <- gsub('COPERATIVAS_DE_AHORRO_Y_CREDITO',
                                'COOPERATIVAS_DE_AHORRO_Y_CREDITO',
                                dat$TIPO_DE_ENTIDAD)
    
    dat$ENTIDIDAD <- gsub('CSN \\(1\\)','CSN', dat$ENTIDIDAD)
    
    dat$GESTION <- as.character(dat$GESTION)
    dat$MES <- as.character(dat$MES)
    dat$DIA <- as.character(dat$DIA)
    
    datEMB <- dat %>% 
        filter(EXPRESADO=='EN_MILES_DE_BOLIVIANOS') %>% 
        mutate(across(.cols = where(is.numeric), ~ .x * 1000 ))
    
    datEB <- dat %>% 
        filter(EXPRESADO=='EN_BOLIVIANOS')
    
    dat <- bind_rows(datEMB,datEB)
    
    dat$EXPRESADO <- rep('EN_BOLIVIANOS',nrow(dat))
    
    dat$GESTION <- as.numeric(dat$GESTION)
    dat$MES <- as.numeric(dat$MES)
    dat$DIA <- as.numeric(dat$DIA)
    
    dat <- dat %>% replace(is.na(.), 0)
    
    dat <- dat %>% filter(GESTION>=gestionIncial)
    
    ###################################
    # ARREGLAR NOMBRE DE COLUMNAS
    namesDat <- gsub( pattern = 'ENTIDIDAD', replacement = 'ENTIDAD', names(dat) )
    names(dat) <- namesDat
    
    ####################################
    # REDUNDANTES
    
    redundancias <- c('SBA','SIF','SEF','SBY','SSP','SCO')
    
    dat <- 
        dat %>% 
        filter(!(ENTIDAD %in% redundancias))
    
    gc()
    
    ###################################
    
    return(dat)
    
}



handles$getDatEstFinDesagregadosAsfi <- function(yearInit = 2014, accounts = NULL, rootDatXlsx = NULL, byClass = NULL, byEnts = NULL, allAccounts=FALSE) {
    
    stopifnot(!is.null(accounts))
    
    require(openxlsx)
    require(dplyr)
    
    if (is.null(rootDatXlsx) ) {
        rootFile <- 'FUENTES-DATOS/ESTADOS-FINANCIEROS-ASFI-WEB/data/BBDD_ESTADOS_FINANCIEROS_ASFI_WEB_____ESTADOS_FINANCIEROS_DESAGREGADOS.xlsx'
    }
    
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
    
    ###################################
    # ENTIDAD
    namesDatAux <- c("ID", "FECHA", "GESTION", "MES", "DIA", "TIPO_DE_ENTIDAD", "EXPRESADO", "ENTIDAD")
    namesDat <- names(dat)
    
    names(dat) <- ifelse(!namesDat %in% namesDatAux, paste0('CD_', namesDat), namesDat)
    
    ###################################
    # CUENTAS NECESARIAS
    
    if (allAccounts) {
        
        dat <- 
            dat
        
    }else{
        
        dat <- 
            dat %>% 
            select( all_of( c(namesDatAux,accounts) ) )
        
    }
    
    
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
    
    return(dat)
    
}

