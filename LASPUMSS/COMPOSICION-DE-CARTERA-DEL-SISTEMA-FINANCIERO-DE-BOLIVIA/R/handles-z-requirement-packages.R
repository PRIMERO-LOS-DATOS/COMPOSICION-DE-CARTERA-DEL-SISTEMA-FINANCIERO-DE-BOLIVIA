if (!('handles' %in% ls())) { handles <- list() }

handles$installRequirePackagesForHandles <- function(){
    
    # PAQUETES NECESARIAS CRAN
    
    pkgs_required <- c('rmarkdown', 'officedown', 'officer')
    
    pkgs <-  data.frame(utils::installed.packages())[,'Package']
    pkgs_miss <- pkgs_required[!(pkgs_required %in% pkgs)]
    
    if (length(pkgs_miss)>=1) {
        
        print('Se instalara los paquetes de CRAN necesarios.')
        print(pkgs_miss)
        install.packages(pkgs_miss, verbose=T, keep_outputs=T)
        
    }else{
        
        print('Ya existe todos los paquete necesarios.')
        
    }
    
    lapply(pkgs_required, require, character.only = TRUE)
    
    rm(list = c('pkgs','pkgs_required','pkgs_miss'))
    
}