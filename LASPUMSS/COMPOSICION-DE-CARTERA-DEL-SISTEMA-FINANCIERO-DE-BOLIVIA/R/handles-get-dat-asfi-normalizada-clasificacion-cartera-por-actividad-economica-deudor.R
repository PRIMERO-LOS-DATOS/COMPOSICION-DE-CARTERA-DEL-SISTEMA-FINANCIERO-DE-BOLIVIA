if (!('handles' %in% ls())) { handles <- list() }
# PARA PODER EXPLORAR LAS CUENTAS DE MANERA INDIVIDUAL
#
# CARACTERÍSTICAS:
#   - Depende de otras funciones.
#   - Frecuencia Mensual

handles$getDatEstFinAsfi_Norm_CalificacionCartActEcoDeudor <- function(dat= NULL, by='ENTIDAD', yearInit = 2014,  byClass = NULL, byEnts = NULL) {
    
    #rootRSourse <- 'ALGOTIMOS-MODELOS/HANDLES'
    #sapply( list.files(rootRSourse, full.names=TRUE, pattern='\\.[rR]$'), source )
    
    
    
    
    if (is.null(dat)) {
        
        dat <- handles$getDatEstFinByGroupAsfi_CalificacionCartActEcoDeudor(dat = dat, 
                                                                          by = by, 
                                                                          yearInit = yearInit,
                                                                          byClass=byClass, 
                                                                          byEnts = byEnts)
    }
    
    # Numero de filas que debera tener el data.frame 
    entidades <- unique(dat$TIPO_DE_ENTIDAD)
    gestionInc <- min(as.numeric(format(dat$FECHA, format='%Y')), na.rm = T)
    gestionFn <- max(as.numeric(format(dat$FECHA, format='%Y')), na.rm = T)
    nGestiones <- (gestionFn-gestionInc+1)
    nMeses <- 12
    nEntidades <- length(entidades)
    n <- nGestiones*nMeses*nEntidades
    
    datNorm <- data.frame(matrix(0, nrow = n, ncol = ncol(dat)))
    names(datNorm) <- names(dat)
    
    datNorm$TIPO_DE_ENTIDAD <- sort(rep(entidades,nMeses*nGestiones))
    
    mesesId <- c('01','02','03','04','05','06','07','08','09','10','11','12')
    fechas <- vector()
    
    for (i in seq(gestionInc, gestionFn)) {
        
        for (i2 in mesesId) {
            
            fechas <- append(fechas, paste0(i,'-',i2,'-28'))
            
        }
        
    }
    
    datNorm$FECHA <- as.Date(rep(fechas,nEntidades))
    
    datNorm$ID <- paste0(datNorm$TIPO_DE_ENTIDAD,
                         format(datNorm$FECHA, format='%Y'),
                         format(datNorm$FECHA, format='%m'))
    
    dat$ID <- paste0(dat$TIPO_DE_ENTIDAD,
                     format(dat$FECHA, format='%Y'),
                     format(dat$FECHA, format='%m'))
    
    
    ###########################################################
    
    dat <- dat %>% select(-FECHA, -TIPO_DE_ENTIDAD)
    
    ###########################################################
    
    datResult <- 
        left_join(datNorm, 
                  dat, 
                  by = join_by(ID == ID), 
                  relationship = 'one-to-one', 
                  suffix = c("_x", "")) %>% 
        select(-ends_with("_x")) %>% 
        relocate(FECHA, .after = 1) %>% 
        select(-ID)
    
    
    
    return(datResult)
}
