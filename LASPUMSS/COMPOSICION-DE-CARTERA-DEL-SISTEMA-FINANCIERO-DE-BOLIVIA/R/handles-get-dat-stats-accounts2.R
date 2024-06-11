if (!('handles' %in% ls())) { handles <- list() }
#
#
# CARACTERÍSTICAS:
#   - No depende de otras funciones.

handles$getDatStatsOverviewByNameColumn <- function(id=NULL, datNorm, roundInd=TRUE, idsDecreasing=FALSE) {
    
    require(dplyr)
    
    ####################################################################
    
    dat <- datNorm
    
    listDataFramesByTipoDeEntidad <- 
        dat %>% 
        group_by(TIPO_DE_ENTIDAD) %>% 
        group_split()
    
    
    ####################################################################
    nFactors <- length(listDataFramesByTipoDeEntidad)
    
    datResult <- data.frame(
        TIPO_DE_ENTIDAD = rep(NA, nFactors),
        
        N_OBS = rep(NA, nFactors),
        
        PROMEDIO = rep(NA, nFactors),
        DESVIACION = rep(NA, nFactors),
        COEFICIENTE_DE_VARIACION = rep(NA, nFactors),
        MINIMO = rep(NA, nFactors),
        MINIMO_NO_CERO = rep(NA, nFactors),
        MAXIMO = rep(NA, nFactors),
        
        TENDENCIA_MCO = rep(NA, nFactors),
        PROBABILIDAD_T_TENDENCIA_MCO = rep(NA, nFactors) 
        )
    
    ####################################################################
    
    for (i in seq(nFactors)) {
        
  
        datByFactor <- listDataFramesByTipoDeEntidad[[i]]
        
        x <- datByFactor %>% select(all_of(id)) %>% pull()
        
        rowX <- 1
        
        ###############################################################################
        # TIPO_DE_ENTIDAD
        
        tipo_de_entidad <- unique(datByFactor$TIPO_DE_ENTIDAD)
        
        datResult[i, rowX] <- tipo_de_entidad
        
        rowX <- rowX + 1
        
        ###############################################################################
        # N_OBS
        
        n_obsX <- length(x)
        
        datResult[i, rowX] <- n_obsX
        
        rowX <- rowX + 1
        
        # PROMEDIO
        
        promedio <- ifelse(roundInd, round(mean(x,na.rm = TRUE),0), mean(x,na.rm = TRUE) ) 
        
        datResult[i, rowX] <- promedio
        
        rowX <- rowX + 1
        
        # DESVIACIÓN
        
        desviacion <- sd(x,na.rm = TRUE) 
        
        datResult[i, rowX] <- desviacion
        
        rowX <- rowX + 1
        
        # COEFICIENTE_DE_VARIACION
        
        coeficiente_variacion <- sd(x,na.rm = TRUE) / mean(x,na.rm = TRUE)
        
        datResult[i, rowX] <- coeficiente_variacion
        
        rowX <- rowX + 1
        
        # MINIMO
        
        minimo <- ifelse(roundInd, round(min(x ,na.rm = TRUE),0), min(x,na.rm = TRUE) )
        
        datResult[i, rowX] <- minimo
        
        rowX <- rowX + 1
        
        # MINIMO_NO_CERO
        
        minimo_no_cero <- ifelse(roundInd, round(min(x[x>0] ,na.rm = TRUE),0), min(x[x>0],na.rm = TRUE) )
        
        datResult[i, rowX] <- minimo_no_cero
        
        rowX <- rowX + 1
        
        # MAXIMO
        
        maximo <- ifelse(roundInd, round(max(x,na.rm = TRUE),0), max(x,na.rm = TRUE) )  
        
        datResult[i, rowX] <- maximo
        
        rowX <- rowX + 1

        
        ###############################################################################
        
        if ('FECHA' %in% names(datByFactor) & (length(x)>5) ) {
            
            datByFactor <- 
                datByFactor %>% 
                arrange(FECHA) %>% 
                mutate(trend=seq(n()))
            
            mco_trend_model <- lm(eval(as.name(id)) ~ trend, data = datByFactor, na.action=na.omit)
            
            # TENDENCIA_MCO
            
            tendencia <- mco_trend_model$coefficients[2]
            
            datResult[i, rowX] <- tendencia
            
            rowX <- rowX + 1
            
            # PROBABILIDAD_T_TENDENCIA_MCO
            
            probabilidad_t <- summary(mco_trend_model)[['coefficients']][2,4]
            
            datResult[i, rowX] <- probabilidad_t
            
            rowX <- rowX + 1
            
        } else {
            
            rowX <- rowX + 2
            
        }
        
        
     
    }
    
    ####################################################################
    
    if (idsDecreasing) {
        datResult <- datResult %>% arrange(desc(PROMEDIO), desc(DESVIACION))
    }else{
        datResult <- datResult %>% arrange(PROMEDIO, DESVIACION)
    }
    
    return(datResult)
}

