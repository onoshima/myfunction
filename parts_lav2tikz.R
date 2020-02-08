source(file='~/GoogleDrive/R/myfunction/lav2tikz.R')
write(lav2tikz(res_ho,large = F, size="scriptsize"), file="Holz_HO.tex")
mo_HO <- '
visual =~ x1+x2+x3
textual =~ x4+x5+x6
speed =~ x7+x8+x9
g =~ visual+textual+speed
'

res_ho <- cfa(mo_HO, data=HolzingerSwineford1939)
summary(res_ho, standardized=T)
is.null(a)
a <- c("SI", "VO", "IN", "CO", "BD", "MR", "VP", "FW", "PC", "DS", "AR", "LN", "SS", "CD", "CA")
b <-observed_names
b[order(factor(b, levels=a))]
idx <- order(factor(a, levels=order))
observed_names[idx]
res.mo1 <- cfa(mo1, sample.cov=input, sample.nobs=n, estimator="GLS")
summary(res.mo1, standardized=T)

# HOか判断するパーツ
temp <- inspect(fit)$lambda
latent_names <- colnames(temp) 
HO <- !is.null(inspect(res.mo1)$beta)

latent_1st <- latent_names[apply(temp,2, sum)!=0]
latent_2nd <- latent_names[apply(temp,2, sum)==0]

if(HO){
  temp <- filter(pars, op == "=~", lhs==latent_2nd)
  node_HO <- str_c('  \\node (',latent_2nd,') at (',n_observed/2 , ',2.5) [latent] {', latent_2nd,'};')
  path_loadings_HO <- str_c('  \\path (',temp$lhs,') edge
                         node [value] {\\', size, round(temp$std.all,2), '}
                         (', temp$rhs, ');') 
} else{
  skip_index <- c(skip_index, -9, -10)
}