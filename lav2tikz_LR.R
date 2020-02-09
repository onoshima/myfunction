#lavaan output to tikz (LtoR ver)
lav2tikz_LR <- function(fit,size="scriptsize", auto=FALSE,
                     observed_order=NULL) {
  library(lavaan)
  library(stringr)
  library(tidyr)
  library(dplyr)

  header <- "\\documentclass[dvipdfmx, tikz,border=10pt]{standalone} \n
\\usetikzlibrary{shapes, arrows, positioning, backgrounds}
\\tikzset{cor/.style ={latex-latex, draw}}
\\tikzset{latent/.style ={ellipse, draw,minimum width=0.8cm, minimum height=0.5cm}}
\\tikzset{observed/.style ={rectangle, fill=white, draw, minimum width=0.8cm, minimum height=0.5cm}}
\\tikzset{value/.style ={draw=none, fill=white, inner sep=1pt}}\n
\\begin{document}
\\begin{tikzpicture}[-latex, draw,shorten >=1pt, shorten <=1pt, font=\\sffamily]"
  
  footer <- "\\end{tikzpicture}\n\\end{document}"

  pars <- parameterEstimates(fit, standardized = TRUE)
  temp <- inspect(fit)$lambda
  latent_names <- colnames(temp) 
  observed_names <- rownames(temp)
  n_latent <- length(latent_names)
  n_observed <- length(observed_names)
  
  # forced order of observed variables
  if(!is.null(observed_order)){
    idx <- order(factor(observed_names, levels=observed_order))
  } else {
    idx <- 1:n_observed
  }
  
  ## Higer-Order factor 
  HO <- !is.null(inspect(fit)$beta)
  latent_1st <- latent_names[apply(temp,2, sum)!=0]
  latent_2nd <- latent_names[apply(temp,2, sum)==0]
  
  n_latent <- length(latent_1st)
  n_observed <- length(observed_names)
  pertitions <- seq(0, n_observed, length=n_latent+1)
  temp <- n_observed/n_latent - median(1:(n_observed/n_latent))
  posy_latent <- -0.8*(pertitions-temp)[-1]

  skip_index <- integer()
  
  # node latent and observed 
  node_latent <- str_c('\\node (',latent_1st,') at (2.5,', posy_latent, ') [latent] {', latent_1st,'};')
  node_observed <- str_c('\\node (',observed_names[idx],') at (0,', 1:n_observed * -0.8, ') [observed] {', observed_names[idx],'};')

  # factor loadigs
  temp <- filter(pars, op == "=~", lhs %in% latent_1st)
  path_loadings <- str_c('\\path (',temp$lhs,') edge node [value] {\\',
                          size, round(temp$std.all,2),'} (', temp$rhs, '.east);') 
  # HO
  if(HO){
    temp <- filter(pars, op == "=~", lhs==latent_2nd)
    node_HO <- str_c('\\node (',latent_2nd,') at (5.5,', n_observed/-2 *0.8 ,') [latent] {', latent_2nd,'};')
    path_loadings_HO <- str_c('\\path (',temp$lhs,') edge node [value] {\\',
                              size, round(temp$std.all,2),'} (', temp$rhs,'.east);') 
    temp <- filter(pars, op == "~~", lhs %in% latent_1st)
    nodename_error_latent <- paste("e_", latent_1st, sep="")
    node_error_latent <- str_c('\\node [value, above=10pt of ',latent_1st,'](',nodename_error_latent,
                            ') {\\', size,round(temp$std.all,2),'};')
    path_error_latent <- str_c('\\path (',nodename_error_latent,') edge (', latent_1st, ');')
  } else{
    skip_index <- c(skip_index, -9, -10, -11, -12)
  }

  # regression
  temp <- filter(pars, op == "~")
  if (nrow(temp)==0){
    skip_index <- c(skip_index, -7)
  } else {
    path_regression <- str_c('\\path (',temp$lhs,') edge node [value] {\\',
                              size, round(temp$std.all,2), '} (', temp$rhs, ');') 
  }
  
  # factor correlation
  temp <- filter(pars, op == "~~", lhs %in% latent_names, lhs != rhs)
  if (nrow(temp)==0){
    skip_index <- c(skip_index, -6)
  } else{
    # dividing 2 steps to avoid drawing arrows on values 
    arrow <- str_c('\\path (',temp$lhs,'.east) edge [cor,bend left] (',temp$rhs,'.east);')
    value <- str_c('\\path [draw=none](',temp$lhs,'.east) edge [draw=none, bend left] node [value] {\\',
      size, round(temp$std.all,2), '} (', temp$rhs,'.east);')
    path_factorcor <- c(arrow, value)
  }
  
  # error variance
  temp <- filter(pars, op == "~~", lhs %in% observed_names, lhs==rhs)
  nodename_error <- paste("e_", observed_names[idx], sep="")
  node_error <- str_c('\\node [draw=none, left = 15pt of ', observed_names[idx],
                      '](',nodename_error,') {\\', 
                      size, round(temp$std.all[idx],2),'};')
  path_errorvar <- str_c('\\path (',nodename_error,') edge (',
                         observed_names[idx], ');')
  
  # error covariacne 
  temp <- filter(pars, op == "~~", lhs %in% observed_names, lhs!=rhs)
  temp_left <- paste("e_", temp$lhs, sep="")
  temp_right <- paste("e_", temp$rhs, sep="")
  if (nrow(temp)==0){
    skip_index <- c(skip_index, -8)
  } else {
    path_errorcov <- str_c('\\path [cor](',temp_left,') edge [draw ,bend right] 
                           node [fill=white, inner sep=1pt] {\\', size, round(temp$std.all,2), '}
                           (', temp_right, ');')
  }
  
  temp <- c("node_latent", "node_observed", "node_error",
            "path_loadings", "path_errorvar", "path_factorcor",
            "path_regression", "path_errorcov", "node_HO", "path_loadings_HO",
            "node_error_latent", "path_error_latent")
  temp <- (temp[skip_index])
  tikz_content <- c()
  
  for (ite in temp){
    tikz_content <- c(tikz_content, get(ite))
  }
  
  output <- c(header, tikz_content, footer)
  return(output)
  }