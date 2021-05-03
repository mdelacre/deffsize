# old density calcs

d_CI <- function(conf.level,fct, # fct = either shieh_CI or glass_CI
                 m1,m2, # sample means
                 sd1,sd2, # sample sd
                 n1,n2, # sample sizes
                 unbiased, # if TRUE = hedges' correction
                 alternative # "two.sided", "greater" or "less"
){ 
  res <- sapply(conf.level,fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased, alternative=alternative) 
  return(res)
  
}

d_CI2 <- function(conf.level,fct, # fct = cohen_CI
                  m1,m2, # sample means
                  sd1,sd2, # sample sd
                  n1,n2, # sample sizes
                  var.equal, # if TRUE = homoscedasticity assumed
                  unbiased, # if TRUE = hedges' correction
                  alternative #"two.sided", "greater" or "less"
){ 
  res <- sapply(conf.level,fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased, alternative=alternative) 
  return(res)
  
}


plot_smd_curv = function(conf.level = .95,
                         fct,
                         df,
                         lambda,
                         smd_label = "Cohen's d",
                         ci_shades = c(.5, .90, .95, .99),
                         var.equal,
                         mult, # relation between t stat and estimator
                         m1,m2,
                         sd1,sd2,
                         n1,n2,
                         unbiased,alternative
){
  
  ci_shade1 = sort(ci_shades, decreasing = TRUE)
  
  if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
    d <- d_CI2(conf.level=conf.level,fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$ES
  } else {
    d <- d_CI(conf.level=conf.level,fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$ES  
  }
  
  x_dt <- seq(qt(.0001, df, lambda), qt(.9999, df, lambda), by = 0.001)
  y_dt <- dt(x_dt, df = df, ncp = lambda)
  x <- mult * x_dt # mult = sqrt(1/n1+1/n2) for Cohen's d
  y <- y_dt / mult # mult = sqrt(1/n1+1/n2) Cohen's d
  
  if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
    ci_linerange = d_CI2(conf.level=conf.level,fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$CI
  } else {
    ci_linerange = d_CI(conf.level=conf.level,fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$CI
  }
  
  if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
    ci_shaderange1 = d_CI2(ci_shade1[1],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$CI
  } else {
    ci_shaderange1 = d_CI(ci_shade1[1],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$CI
  }
  
  #if(ci_linerange[1]==-Inf){
  #  LL_linerange <- -5*d#-9999
  #} else {
  LL_linerange <- ci_linerange[1]
  #}
  
  #if(ci_linerange[2]==Inf){
  #UL_linerange <- 5*d #9999
  #} else {
  UL_linerange <- ci_linerange[2]
  #}
  
  #if(ci_shaderange1[1]==-Inf){
  #  LL_shaderange1 <- -10*d#9999
  #} else {
  LL_shaderange1 <- ci_shaderange1[1]
  #}
  
  #if(ci_shaderange1[2]==Inf){
  #  UL_shaderange1 <- 10*d#9999
  #} else {
  UL_shaderange1 <- ci_shaderange1[2]
  #}
  
  df_xy = data.frame(x = x,
                     y = y)
  p1 = ggplot(df_xy,
              aes(x = x, y = y)) +
    geom_line() +
    annotate("segment",
             x = LL_linerange,
             xend = UL_linerange,
             y = -.1, yend = -.1,
             colour = "black") +
    geom_point(data = data.frame(y = -.1,
                                 x = d),
               aes(x = x, y = y),
               size = 3.5) +
    # subset(df_xy, x >= LL_shaderange1 & x <= UL_shaderange1)
    geom_area(data = subset(df_xy, x >= LL_shaderange1 & x <= UL_shaderange1),
              aes(x = x, y = y, fill = as.character(ci_shade1[1])),
              colour = "royalblue4") +
    labs(x = '', y = '',
         fill = "Confidence Interval")
  
  if(length(ci_shade1> 1)){
    
    if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
      ci_shaderange2 = d_CI2(ci_shade1[2],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$CI
    } else {
      ci_shaderange2 = d_CI(ci_shade1[2],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$CI
    }
    
    #if(ci_shaderange2[1]==-Inf){
    #  LL_shaderange2 <- -9999
    #} else {
    LL_shaderange2 <- ci_shaderange2[1]
    #}
    
    #if(ci_shaderange2[2]==Inf){
    #  UL_shaderange2 <- 9999
    #} else {
    UL_shaderange2 <- ci_shaderange2[2]
    #}
    
    p2 = p1 +
      geom_area(data = subset(df_xy, x >= LL_shaderange2 & x <= UL_shaderange2),
                aes(x = x, y = y, fill = as.character(ci_shade1[2])),
                colour = "royalblue4")
  } else {
    p2 = p1
  }
  
  if(length(ci_shade1>2)){
    if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
      ci_shaderange3 = d_CI2(ci_shade1[3],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$CI
    } else {
      ci_shaderange3 = d_CI(ci_shade1[3],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$CI
    }
    
    #if(ci_shaderange3[1]==-Inf){
    #  LL_shaderange3 <- -9999
    #} else {
    LL_shaderange3 <- ci_shaderange3[1]
    #}
    
    #if(ci_shaderange3[2]==Inf){
    #  UL_shaderange3 <- 9999
    #} else {
    UL_shaderange3 <- ci_shaderange3[2]
    #}
    
    p2 = p2 +
      geom_area(data = subset(df_xy, x >= LL_shaderange3 & x <= UL_shaderange3),
                aes(x = x, y = y, fill = as.character(ci_shade1[3])),
                colour = "royalblue4")
  }
  
  if(length(ci_shade1>3)){
    
    if(smd_label=="Cohen's d"|smd_label=="Hedges' g"|smd_label=="Hedges' d*"|smd_label=="Hedges' g*"){
      ci_shaderange4 = d_CI2(ci_shade1[4],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,var.equal=var.equal,unbiased=unbiased,alternative=alternative)[,1]$CI
    } else {
      ci_shaderange4 = d_CI(ci_shade1[4],fct=fct,m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,unbiased=unbiased,alternative=alternative)[,1]$CI
    }
    
    #if(ci_shaderange4[1]==-Inf){
    #  LL_shaderange4 <- -9999
    #} else {
    LL_shaderange4 <- ci_shaderange4[1]
    #}
    
    #if(ci_shaderange4[2]==Inf){
    #  UL_shaderange4 <- 9999
    #} else {
    UL_shaderange4 <- ci_shaderange4[2]
    #}
    
    p2 = p2 +
      geom_area(data = subset(df_xy, x >= LL_shaderange4 & x <= UL_shaderange4),
                aes(x = x, y = y, fill = as.character(ci_shade1[4])),
                colour = "royalblue4")
  }
  
  p2 = p2 +
    scale_fill_brewer(direction = -1,
                      na.translate = FALSE) +
    theme_tidybayes() + 
    labs(x = "",y="") + 
    facet_wrap(~as.character(smd_label))+
    theme(legend.position="top",
          strip.text = element_text(face="bold", size=10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(p2)
}
