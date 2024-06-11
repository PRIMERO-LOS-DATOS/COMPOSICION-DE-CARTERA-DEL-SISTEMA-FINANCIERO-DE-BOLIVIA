if (!('handles' %in% ls())) { handles <- list() }
# PARA PODER EXPLORAR LAS CUENTAS DE MANERA INDIVIDUAL
#
# CARACTERÍSTICAS:
#   - Depende de otras funciones.
#   - Frecuencia Mensual


handles$calcVector_myLog <- function(x){
    
    result <- log(x)
    result <- ifelse(is.infinite(result), NA, result)
    result <- ifelse(is.nan(result), NA, result)
    result <- ifelse(is.na(result), 0, result)
    
    return(result)
    
    
}
