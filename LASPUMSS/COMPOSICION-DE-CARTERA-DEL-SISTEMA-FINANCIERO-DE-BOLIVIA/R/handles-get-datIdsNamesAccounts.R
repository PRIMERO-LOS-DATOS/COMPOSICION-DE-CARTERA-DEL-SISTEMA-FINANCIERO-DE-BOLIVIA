if (!('handles' %in% ls())) { handles <- list() }

handles$getDatIdsNamesCamelIndicadores <- function() {
    
    rootFile <- 'ALGOTIMOS-MODELOS/HANDLES/data/dat_names_ids_accounts_for_stats.xlsx'
    
    indicadoresCamelId <- openxlsx::read.xlsx(rootFile)
    
    indicadoresCamelId$DECRECIENTE <- ifelse(indicadoresCamelId$DIRECCION=='ASCENDENTE', TRUE, FALSE)
    
    
    return(indicadoresCamelId)
    
}
