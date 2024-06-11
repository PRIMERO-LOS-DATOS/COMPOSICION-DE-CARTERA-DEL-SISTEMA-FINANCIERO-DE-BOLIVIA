if (!('handles' %in% ls())) { handles <- list() }

handles$renderPlotBasic <- function(plot, width = 5, height=7) {
    
    require(ggplot2)
    require(magick)
    require(knitr)
    
    gcn_memory <- function(n=2) {replicate(n, gc())}
    
    ########################################################
    
    pathDir <- 'tmp_files_imgs'
    if (!dir.exists(pathDir)) dir.create(pathDir)
    
    nImg <- length(list.files(pathDir)) + 1
    path = paste0(pathDir,'/img',nImg,'.png')
    
    ggsave(filename = path, 
           plot = plot,
           width = width, 
           height = height, 
           dpi=700)
    
    gcn_memory()
    
    # Asegurar el formato de la imagen
    error_file = magick::image_read(path)
    
    gcn_memory()
    
    right_png <- magick::image_convert(error_file, "png")
    
    gcn_memory()
    
    magick::image_write(right_png, path = path, format = "png")
    
    gcn_memory()

    rootPlot <- normalizePath(path, winslash = "/")
    
    gcn_memory()
    
    return(knitr::include_graphics(rootPlot))
  
}
