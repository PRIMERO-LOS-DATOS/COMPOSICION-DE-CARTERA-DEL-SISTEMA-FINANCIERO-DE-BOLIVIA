#https://stackoverflow.com/questions/76058523/setting-row-height-in-huxtable
if (!('handles' %in% ls())) { handles <- list() }

handles$renderListTablesAccountsWordDocument <- function(listResult , nSepEnt = NULL, fontSize=8, set_top_padding=3, set_bottom_padding=0.3, ids=NULL, colNamesRender=NULL, changeColNamesRender=NULL) {
    
    stopifnot(!is.null(nSepEnt))
    
    require(dplyr)
    require(knitr)
    require(kableExtra)
    require(huxtable)
    require(stringr)
    
    #######################################################################################
    ### AGRUPAR TODAS LAS TABLAS
    #######################################################################################
    
    listTables <- list()
    datTrendIndTotal <- data.frame()
    
    if (is.null(ids)) {
        n <- length(listResult)
        ids <- names(listResult)
    }else{
        n <- length(ids)
    }
    
    
    for (i in seq(n)) {
        
        id <- listResult[[ ids[i] ]][['id']]
        datTrendInd <- listResult[[ ids[i] ]][['datTrendInd']]
        
        datTrendIndTotal <-  bind_rows(datTrendIndTotal,datTrendInd)
    }
    
    if (!is.null(colNamesRender)) {
        datTrendIndTotal <- 
            datTrendIndTotal %>% select(all_of(colNamesRender))
    }
    
    if (!is.null(changeColNamesRender)) {
        names(datTrendIndTotal) <- changeColNamesRender
    }
    
    names(datTrendIndTotal) <- gsub('_',' ', names(datTrendIndTotal))
    
    #######################################################################################
    ### AGREGAR TÍTULOS POR GRUPO DE CUENTAS
    #######################################################################################
    
    tableResult <- as_hux(datTrendIndTotal) 
    
    nCn <- 1
    
    for (i in seq(n)) {
        
        nameIndById <- listResult[[ ids[i] ]][['nameIndById']]
        nameIndById <- str_to_upper(nameIndById)
        
        new_row <- c(nameIndById, rep('', ncol(tableResult) - 1) )
        
        tableResult <-  
            tableResult %>%
            insert_row(new_row, after = nCn)  %>%
            set_bold(row = nCn+1, col = everywhere) 
        
  
        colspan(tableResult)[nCn+1, 1] <- ncol(tableResult)
        align(tableResult)[nCn+1, 1] <- 'center'
        
        nCn <- nCn + nSepEnt +1
    }
    
    #######################################################################################
    ### FORMATOS DE PRESENTACIÓN
    #######################################################################################
    
    tableResult <- 
        tableResult %>%
        set_number_format(value=list(function(x) {
                                  formatC(x, 
                                          format = "f", 
                                          big.mark = "  ", 
                                          digits = 4, 
                                          decimal.mark = ".",
                                          drop0trailing = FALSE)
                              })) %>%
        set_bold(row = 1, col = everywhere) %>%
        set_top_border(row = 1, col = everywhere) %>%
        set_bottom_border(row = 1, col = everywhere) %>%
        set_bottom_border(row = nrow(tableResult), col = everywhere) %>% 
        set_top_padding(set_top_padding) %>%
        set_bottom_padding(set_bottom_padding)
    
    font_size(tableResult) <- fontSize
    
    tableResult <- 
        tableResult %>% 
        as_flextable() %>%
        flextable::font(fontname = "Times New Roman")
    
    return(tableResult)
}


handles$renderListTablesAccountsWordDocument_v2 <- function(listResult , nSepEnt = NULL, fontSize=8, set_top_padding=3, set_bottom_padding=0.3, ids=NULL, colNamesRender=NULL, changeColNamesRender=NULL, captionTable='caption', digitsColumns=4) {
    
    stopifnot(!is.null(nSepEnt))
    
    require(dplyr)
    require(knitr)
    require(kableExtra)
    require(huxtable)
    require(stringr)
    
    #######################################################################################
    ### AGRUPAR TODAS LAS TABLAS
    #######################################################################################
    
    listTables <- list()
    datTrendIndTotal <- data.frame()
    
    if (is.null(ids)) {
        n <- length(listResult)
        ids <- names(listResult)
    }else{
        n <- length(ids)
    }
    
    
    for (i in seq(n)) {
        
        id <- listResult[[ ids[i] ]][['id']]
        datTrendInd <- listResult[[ ids[i] ]][['datTrendInd']]
        
        datTrendIndTotal <-  bind_rows(datTrendIndTotal,datTrendInd)
    }
    
    if (!is.null(colNamesRender)) {
        datTrendIndTotal <- 
            datTrendIndTotal %>% select(all_of(colNamesRender))
    }
    
    if (!is.null(changeColNamesRender)) {
        names(datTrendIndTotal) <- changeColNamesRender
    }
    
    names(datTrendIndTotal) <- gsub('_',' ', names(datTrendIndTotal))
    
    #######################################################################################
    ### AGREGAR TÍTULOS POR GRUPO DE CUENTAS
    #######################################################################################
    
    
    #######################################################################################
    tableResult <- as_hux(datTrendIndTotal) 
    
    nCn <- 1
    
    for (i in seq(n)) {
        
        nameIndById <- listResult[[ ids[i] ]][['nameIndById']]
        nameIndById <- str_to_upper(nameIndById)
        
        tableResult <- set_contents(tableResult, i+1, 1, nameIndById)
        
        #new_row <- c(nameIndById, rep('', ncol(tableResult) - 1) )
        
        #tableResult <-  
         #   tableResult %>%
          #  insert_row(new_row, after = nCn)  %>%
           # set_bold(row = nCn+1, col = everywhere) 
        
        
        #colspan(tableResult)[nCn+1, 1] <- ncol(tableResult)
        #align(tableResult)[nCn+1, 1] <- 'center'
        
        #nCn <- nCn + nSepEnt +1
    }
    
    #######################################################################################
    ### FORMATOS DE PRESENTACIÓN
    #######################################################################################
    
    tableResult <- 
        tableResult %>%
        set_number_format(value=list(function(x) {
            formatC(x, 
                    format = "f", 
                    big.mark = "  ", 
                    digits = digitsColumns, 
                    decimal.mark = ".",
                    drop0trailing = FALSE)
        })) %>%
        set_bold(row = 1, col = everywhere) %>%
        set_top_border(row = 1, col = everywhere) %>%
        set_bottom_border(row = 1, col = everywhere) %>%
        set_bottom_border(row = nrow(tableResult), col = everywhere) %>% 
        set_top_padding(set_top_padding) %>%
        set_bottom_padding(set_bottom_padding) %>% 
        
        set_number_format(0, row=everywhere, col=1)
    
    font_size(tableResult) <- fontSize
    
    tableResult <- 
        tableResult %>% 
        as_flextable() %>%
        flextable::font(fontname = "Arial")  %>%
        flextable::set_caption(
            flextable::as_paragraph(
                flextable::as_chunk(captionTable, 
                                    props = flextable::fp_text_default(font.family = "Arial"))
            ), 
            word_stylename = "Table Caption"
        )
    
    return(tableResult)
}

