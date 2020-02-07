#lavaan output to tikz large model
lav2tikz <- function(fit, standardized=TRUE, large=FALSE,
                     size=c("tiny","scriptsize"), auto=FALSE) {
  library(lavaan)
  library(stringr)
  library(tidyr)
  library(dplyr)

  header <- "
  \\documentclass[dvipdfmx, tikz,border=10pt]{standalone}\n
  \\usetikzlibrary{shapes, arrows, positioning, backgrounds}\n
  \\tikzset{cor/.style ={latex-latex}}\n
  \\tikzset{latent/.style ={ellipse, draw,minimum width=0.8cm, minimum height=0.5cm}}\n
  \\tikzset{observed/.style ={rectangle, fill=white, draw, minimum width=0.8cm, minimum height=0.5cm}}\n
  \\tikzset{value/.style ={draw=none, fill=white, inner sep=1pt}}\n
  \\begin{document}\n
  \\begin{tikzpicture}[-latex, draw,shorten >=1pt, font=\\sffamily]\n
  "
  footer <- "\\end{tikzpicture}\n\\end{document}"
  #\\pgfdeclarelayer{bg}\n
  #\\pgfsetlayers{bg, main}\n
  pars <- parameterEstimates(fit, standardized = standardized)
  temp <- inspect(fit)$lambda
  latent_names <- colnames(temp) 
  observed_names <- rownames(temp)
  
  if(large == TRUE){ # divide observed variablew to two layer
    n_latent <- length(latent_names)
    n_observed <- length(observed_names)
    index_1stlayer <- seq(1, n_latent, by=2)
    index_2ndlayer <- setdiff(1:n_latent, index_1stlayer)
    latent_1stlayer <- latent_names[index_1stlayer]
    latent_2ndlayer <- latent_names[index_2ndlayer]
    temp01 <- filter(pars, op == "=~", lhs %in% latent_1stlayer)
    observed_1stlayer <- temp01$rhs
    idx_obs_1st <- is.element(observed_names, observed_1stlayer) 
    n_observed_1st <- nrow(temp01)
    temp02 <- dplyr::filter(pars, op == "=~", lhs %in% latent_2ndlayer)
    n_observed_2nd <- nrow(temp02) 
    observed_2ndlayer <- temp02$rhs
    idx_obs_2nd <- is.element(observed_names, observed_2ndlayer) 
    
    pertitions <- seq(0, n_observed_1st, length=n_latent+1)
    temp <- 0.5 *(n_observed/n_latent - median(1:(n_observed/n_latent)))
    posx_latent <- (pertitions-temp)[-1]
  } else {
    n_latent <- length(latent_names)
    n_observed <- length(observed_names)
    pertitions <- seq(0, n_observed, length=n_latent+1)
    temp <- n_observed/n_latent - median(1:(n_observed/n_latent))
    posx_latent <- (pertitions-temp)[-1]
  }

  skip_index <- integer()
  
  # node latent and observed 
  if(large==TRUE){
    node_latent <- str_c('  \\node (',latent_names,') at (', posx_latent, ',1.5) [latent] {', latent_names,'};')
    
    #今作業中
    space <- n_observed_1st/length(latent_1stlayer)/2
    #posx_observed_1st <- 1:n_observed_1st
    #addval <- 0:(length(latent_1stlayer)-1) * space
    #temp <- filter(pars, op=="=~", lhs %in% latent_1stlayer) %>% pull(lhs)
    #addval <- as.numeric(as.character(factor(temp, levels = laten_1stlayer,labels=addval)))
    #posx_observed_1st <- posx_observed_1st + addval

    posx_observed_2nd <- 1:n_observed_2nd
    addval <- 0:(length(latent_2ndlayer)-1) * space
    temp <- filter(pars, op=="=~", lhs %in% latent_2ndlayer) %>% pull(lhs)
    addval <- as.numeric(as.character(factor(temp,levels=latent_2ndlayer, labels=addval)))
    posx_observed_2nd <- posx_observed_2nd + addval
    posx_observed_2nd <- posx_observed_2nd + space-1
    
    posx_observed_1st <- 1:n_observed_1st
    
    node_observed_1st <- str_c('  \\node (',observed_1stlayer,') at (', posx_observed_1st, ', 0) [observed] {', observed_1stlayer,'};')
    node_observed_2nd <- str_c('  \\node (',observed_2ndlayer,') at (', posx_observed_2nd, ', -2) [observed] {', observed_2ndlayer,'};')
    node_observed <- c(node_observed_1st, node_observed_2nd)
  } else {
    node_latent <- str_c('  \\node (',latent_names,') at (', posx_latent, ',1.5) [latent] {', latent_names,'};')
    node_observed <- str_c('  \\node (',observed_names,') at (', 1:n_observed, ', 0) [observed] {', observed_names,'};')
  }
  
  # factor loadigs
  if(large==T){
    temp01 <- filter(pars, op == "=~")[idx_obs_1st,]
    path_loadings_1st <- str_c('  \\path (',temp01$lhs,') edge
                         node [value, pos=0.65] {\\',size, round(temp01$std.all,2), '}
                         (', temp01$rhs, ');')
    temp01 <- filter(pars, op == "=~")[idx_obs_2nd,]
    path_loadings_2nd <- str_c('  \\path (',temp01$lhs,') edge
                         node [value, pos=0.85] {\\', size, round(temp01$std.all,2), '}
                         (', temp01$rhs, ');')
    layer_start <- "\\begin{scope}[on background layer, thick]"
    layer_end <- "\\end{scope}"
    path_loadings <- c(path_loadings_1st,layer_start, path_loadings_2nd,layer_end)
  } else{
    temp01 <- dplyr::filter(pars, op == "=~")
    path_loadings <- str_c('  \\path (',temp01$lhs,') edge
                         node [value] {\\', size, round(temp01$std.all,2), '}
                         (', temp01$rhs, ');') 
  }

  # factor regression
  temp01 <- dplyr::filter(pars, op == "~")
  if (nrow(temp01)==0){
    #no_regression <- TRUE
    skip_index <- c(skip_index, -7)
  } else {
    path_regression <- str_c('  \\path (',temp01$lhs,') edge
                             node [value] {\\', size, round(temp01$std.all,2), '}
                             (', temp01$rhs, ');') 
  }
  
  # factor correlation
  temp11 <- dplyr::filter(pars, op == "~~", lhs %in% latent_names, lhs != rhs)
  if (nrow(temp11)==0){
    #no_factorcorrelation=TRUE
    skip_index <- c(skip_index, -6)
  } else{
    path_factorcor <- str_c('  \\path [cor](',temp11$lhs,') edge [bend left]
                            node [value] {\\', size, round(temp11$std.all,2), '}
                            (', temp11$rhs, ');')
  }
  
  # error variance
  temp11 <- filter(pars, op == "~~", lhs %in% observed_names, lhs==rhs)
  nodename_error <- paste("e_", temp11$lhs, sep="")
  if (large==TRUE){
    temp12 <- filter(temp11, lhs %in% observed_1stlayer)
    #node_error_1st <- str_c('  \\node [draw=none](',nodename_error[idx_obs_1st],') at (', 1:nrow(temp12), ', -0.5) {\\', size,round(temp12$std.all,2),'};')
    node_error_1st <- str_c('  \\node [value, below=10pt of ',observed_1stlayer,'](',nodename_error[idx_obs_1st],')  {\\', size,round(temp12$std.all,2),'};')
    temp13 <- filter(temp11, lhs %in% observed_2ndlayer)
    #node_error_2nd <- str_c('  \\node [draw=none](',nodename_error[idx_obs_2nd],') at (', 1:nrow(temp13), ', -2) {\\', size,round(temp13$std.all,2),'};')
    node_error_2nd <- str_c('  \\node [value, below=10pt of ',observed_2ndlayer,'](',nodename_error[idx_obs_2nd],') {\\', size,round(temp13$std.all,2),'};')
    node_error <- c(node_error_1st, node_error_2nd)
    
    path_errorvar <- str_c('  \\path (',nodename_error,') edge (', temp11$rhs, ');')

  }else{
  node_error <- str_c('  \\node [draw=none](',nodename_error,') at (', 1:nrow(temp11), ', -1) {\\', size,round(temp11$std.all,2),'};')
  path_errorvar <- str_c('  \\path (',nodename_error,') edge (', temp11$rhs, ');')
  }
  
  # error covariacne 
  temp13 <- dplyr::filter(pars, op == "~~", lhs %in% observed_names, lhs!=rhs)
  temp_left <- paste("e_", temp13$lhs, sep="")
  temp_right <- paste("e_", temp13$rhs, sep="")
  if (nrow(temp13)==0){
    #no_errorcov <- TRUE
    skip_index <- c(skip_index, -8)
  } else {
    path_errorcov <- str_c('  \\path [cor](',temp_left,') edge [bend right] 
                           node [value] {\\', size, round(temp13$std.all,2), '}
                           (', temp_right, ');')
  }
  
  temp <- c("node_latent", "node_observed", "node_error",
            "path_loadings", "path_errorvar", "path_factorcor",
            "path_regression", "path_errorcov")
  temp <- (temp[skip_index])
  tikz_content <- c()
  
  for (ite in temp){
    tikz_content <- c(tikz_content, get(ite))
  }
  
  output <- c(header, tikz_content, footer)
  return(output)
  }