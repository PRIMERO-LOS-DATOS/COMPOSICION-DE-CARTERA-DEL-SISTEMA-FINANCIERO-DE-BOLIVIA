if (!('handles' %in% ls())) { handles <- list() }
#
#
# CARACTERÍSTICAS:
#   - No depende de otras funciones.

handles$getDatStatsOverviewAsfi <- function(id='indCap_CAP', datCamelIndNorm, roundInd=TRUE, idsDecreasing=FALSE) {
    
    require(fpp2)
    require(dplyr)
    
    ####################################################################
    
    dat <- datCamelIndNorm
    
    dat$TIPO_DE_ENTIDAD <- gsub('_', ' ', dat$TIPO_DE_ENTIDAD)
    
    gestInc <- min(as.numeric(format(dat$FECHA, format='%Y')))
    gestFn <- max(as.numeric(format(dat$FECHA, format='%Y')))
    
    tsDat <- ts(matrix(0, nrow=(gestFn-gestInc+1)*12, ncol=length(unique(dat$TIPO_DE_ENTIDAD))), 
                start=gestInc, frequency=12)
    
    for (i in 1:length(unique(dat$TIPO_DE_ENTIDAD))) {
        
        tipoEnt <- as.character(unique(dat$TIPO_DE_ENTIDAD)[i])
        colnames(tsDat)[i] <- tipoEnt
        x <- dat[dat$TIPO_DE_ENTIDAD==tipoEnt,][order(dat[dat$TIPO_DE_ENTIDAD==tipoEnt,'FECHA']), id]
        x <- sapply(x, as.numeric)
        tsDat[,i] <- x
    }
    
    ####################################################################
    
    
    datResult <- data.frame(TIPO_DE_ENTIDAD=rep(NA,length(colnames(tsDat))),
                              PROMEDIO=rep(NA,length(colnames(tsDat))),
                              DESVIACION=rep(NA,length(colnames(tsDat))),
                              MINIMO=rep(NA,length(colnames(tsDat))),
                              MAXIMO=rep(NA,length(colnames(tsDat))),
                            TENDENCIA=rep(NA,length(colnames(tsDat))),
                            PROBABILIDAD_T=rep(NA,length(colnames(tsDat))))
    
    ####################################################################
    
    for (i in 1:(length(colnames(tsDat)))) {
        
  
        valor <- colnames(tsDat)[i]
        x <- tsDat[,valor]
        
        promedio <- ifelse(roundInd, round(mean(x,na.rm = TRUE),0), mean(x,na.rm = TRUE) )   
        desviacion <- sd(x,na.rm = TRUE) 
        minimo <- ifelse(roundInd, round(min(x,na.rm = TRUE),0), min(x,na.rm = TRUE) )  
        maximo <- ifelse(roundInd, round(max(x,na.rm = TRUE),0), max(x,na.rm = TRUE) )  
        
        fit <- tslm(tsDat[,valor]~trend) # tendencia
        
        datResult[i,1] <- valor
        datResult[i,2] <- promedio
        datResult[i,3] <- desviacion
        datResult[i,4] <- minimo
        datResult[i,5] <- maximo
        
        datResult[i,6] <- fit$coefficients[2]
        datResult[i,7] <- summary(fit)[['coefficients']][2,4]
     
    }
    
    ####################################################################
    
    if (idsDecreasing) {
        datResult <- datResult %>%  arrange(desc(PROMEDIO), desc(DESVIACION))
    }else{
        datResult <- datResult %>%  arrange(PROMEDIO, DESVIACION)
    }
    
    return(datResult)
}

