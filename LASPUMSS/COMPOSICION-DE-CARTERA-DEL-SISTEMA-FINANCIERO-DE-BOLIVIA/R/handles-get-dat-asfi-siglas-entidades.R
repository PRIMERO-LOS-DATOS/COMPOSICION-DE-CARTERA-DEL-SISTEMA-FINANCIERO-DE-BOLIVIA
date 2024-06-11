if (!('handles' %in% ls())) { handles <- list() }

handles$getDatNameEntidadesAsfiByIds <- function() {
    
    require(openxlsx)
    
    rootsFiles <- list.files('FUENTES-DATOS/ESTADOS-FINANCIEROS-ASFI-WEB/data/ESTADOS-FINANCIEROS-ASFI-WEB-SIGLAS', full.names = T)
    
    dat <- data.frame()
    
    for (root in rootsFiles) {
        
        print(root)
        datAux <- openxlsx::read.xlsx(root)
        
        dat <- rbind(dat, datAux)
        rm(datAux)
        
    }
    

    return(dat)
}
