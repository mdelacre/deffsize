plot_cohen_curv = function(cdf_dat,
                          smd_label,
                          m1,
                          m2,
                          sd1,
                          sd2,
                          n1,
                          n2,
                          corr = TRUE,
                          var.equal = FALSE,
                          ci_shades = c(.5, .90, .95, .99),
                          ci_line = .95){
  ci_shade1 = sort(ci_shades, decreasing = TRUE)
  
  ci_linerange = cohen_CI(m1 = m1,
                          m2 = m2,
                          sd1 = sd1,
                          sd2 = sd2,
                          n1 = n1,
                          n2 = n2,
                          var.equal = var.equal,
                          unbiased = corr,
                          alternative = "two.sided",
                          conf.level = ci_line)$CI
  
  d = cohen_CI(
    m1 = m1,
    m2 = m2,
    sd1 = sd1,
    sd2 = sd2,
    n1 = n1,
    n2 = n2,
    var.equal = var.equal,
    unbiased = corr,
    alternative = "two.sided",
    conf.level = ci_line
  )$ES
  
  ci_shaderange1 = cohen_CI(m1 = m1,
                            m2 = m2,
                            sd1 = sd1,
                            sd2 = sd2,
                            n1 = n1,
                            n2 = n2,
                            var.equal = var.equal,
                            unbiased = corr,
                            alternative = "two.sided",
                            conf.level = ci_shade1[1])$CI
  cdf_dat2 = cdf_dat$x
  
  x.dens  <- density(cdf_dat2)
  df.dens <- data.frame(x=x.dens$x, y=x.dens$y)
  
  p1 = ggplot(data = cdf_dat) +
    geom_density(aes(x = x, y = ..density..),
                 color = "black") +
    geom_area(data = subset(df.dens, x >= ci_shaderange1[1] & x <= ci_shaderange1[2]),
              aes(x = x, y = y, fill = as.character(ci_shade1[1]))) +
    scale_fill_brewer(direction = -1,
                      na.translate = FALSE) +
    labs(x = '', y = '',
         fill = "Confidence Interval")
  
  if(length(ci_shade1 > 1)){
    ci_shaderange2 = cohen_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              var.equal = var.equal,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[2])$CI
    p2 = p1 +
      geom_area(data = subset(df.dens, x >= ci_shaderange2[1] & x <= ci_shaderange2[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[2])))
  } else {
    p2 = p1
  }
  
  if(length(ci_shade1 > 2)){
    ci_shaderange3 = cohen_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              var.equal = var.equal,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[3])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange3[1] & x <= ci_shaderange3[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[3])))
  }
  
  if(length(ci_shade1 > 3)){
    ci_shaderange4 = cohen_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              var.equal = var.equal,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[4])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange4[1] & x <= ci_shaderange4[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[4])))
  }
  
  p2 = p2 +
    geom_point(data = data.frame(y = 0,
                                 x = d),
               aes(x = x, y = y),
               size = 3) +
    annotate("segment",
             x = ci_linerange[1],
             xend = ci_linerange[2],
             y = 0, yend = 0,
             size = 1.5,
             colour = "black") +
    facet_wrap(~as.character(smd_label)) +
    theme_bw()
  
  
  return(p2)
}



plot_cohen_curv = function(cdf_dat,
                           smd_label,
                           m1,
                           m2,
                           sd1,
                           sd2,
                           n1,
                           n2,
                           corr = TRUE,
                           var.equal = FALSE,
                           ci_shades = c(.5, .90, .95, .99),
                           ci_line = .90)

plot_glass_curv = function(cdf_dat,
                          smd_label,
                          m1,
                          m2,
                          sd1,
                          sd2,
                          n1,
                          n2,
                          corr = TRUE,
                          ci_shades = c(.5, .90, .95, .99),
                          ci_line = .90){
  ci_shade1 = sort(ci_shades, decreasing = TRUE)
  d = glass_CI(
    m1 = m1,
    m2 = m2,
    sd1 = sd1,
    sd2 = sd2,
    n1 = n1,
    n2 = n2,
    unbiased = corr,
    alternative = "two.sided",
    conf.level = ci_line
  )$ES
  ci_linerange = glass_CI(m1 = m1,
                          m2 = m2,
                          sd1 = sd1,
                          sd2 = sd2,
                          n1 = n1,
                          n2 = n2,
                          unbiased = corr,
                          alternative = "two.sided",
                          conf.level = ci_line)$CI
  ci_shaderange1 = glass_CI(m1 = m1,
                            m2 = m2,
                            sd1 = sd1,
                            sd2 = sd2,
                            n1 = n1,
                            n2 = n2,
                            unbiased = corr,
                            alternative = "two.sided",
                            conf.level = ci_shade1[1])$CI
  
  cdf_dat2 = cdf_dat$x
  
  x.dens  <- density(cdf_dat2)
  df.dens <- data.frame(x=x.dens$x, y=x.dens$y)
  
  p1 = ggplot(data = cdf_dat) +
    geom_density(aes(x = x, y = ..density..),
                 color = "black") +
    geom_area(data = subset(df.dens, x >= ci_shaderange1[1] & x <= ci_shaderange1[2]),
              aes(x = x, y = y, fill = as.character(ci_shade1[1]))) +
    scale_fill_brewer(direction = -1,
                      na.translate = FALSE) +
    labs(x = '', y = '',
         fill = "Confidence Interval")
  
  if(length(ci_shade1 > 1)){
    ci_shaderange2 = glass_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[2])$CI
    p2 = p1 +
      geom_area(data = subset(df.dens, x >= ci_shaderange2[1] & x <= ci_shaderange2[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[2])))
  } else {
    p2 = p1
  }
  
  if(length(ci_shade1 > 2)){
    ci_shaderange3 = glass_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[3])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange3[1] & x <= ci_shaderange3[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[3])))
  }
  
  if(length(ci_shade1 > 3)){
    ci_shaderange4 = glass_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[4])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange4[1] & x <= ci_shaderange4[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[4])))
  }
  
  p2 = p2 +
    geom_point(data = data.frame(y = 0,
                                 x = d),
               aes(x = x, y = y),
               size = 3) +
    annotate("segment",
             x = ci_linerange[1],
             xend = ci_linerange[2],
             y = 0, yend = 0,
             size = 1.5,
             colour = "black") +
    facet_wrap(~as.character(smd_label)) +
    theme_bw()
  
  
  return(p2)
}

plot_shieh_curv = function(cdf_dat,
                          smd_label,
                          m1,
                          m2,
                          sd1,
                          sd2,
                          n1,
                          n2,
                          corr = TRUE,
                          ci_shades = c(.5, .90, .95, .99),
                          ci_line = .90){
  ci_shade1 = sort(ci_shades, decreasing = TRUE)
  d = shieh_CI(
    m1 = m1,
    m2 = m2,
    sd1 = sd1,
    sd2 = sd2,
    n1 = n1,
    n2 = n2,
    unbiased = corr,
    alternative = "two.sided",
    conf.level = ci_line
  )$ES
  ci_linerange = shieh_CI(m1 = m1,
                          m2 = m2,
                          sd1 = sd1,
                          sd2 = sd2,
                          n1 = n1,
                          n2 = n2,
                          unbiased = corr,
                          alternative = "two.sided",
                          conf.level = ci_line)$CI
  ci_shaderange1 = shieh_CI(m1 = m1,
                            m2 = m2,
                            sd1 = sd1,
                            sd2 = sd2,
                            n1 = n1,
                            n2 = n2,
                            unbiased = corr,
                            alternative = "two.sided",
                            conf.level = ci_shade1[1])$CI
  
  cdf_dat2 = cdf_dat$x
  
  x.dens  <- density(cdf_dat2)
  df.dens <- data.frame(x=x.dens$x, y=x.dens$y)
  
  p1 = ggplot(data = cdf_dat) +
    geom_density(aes(x = x, y = ..density..),
                 color = "black") +
    geom_area(data = subset(df.dens, x >= ci_shaderange1[1] & x <= ci_shaderange1[2]),
              aes(x = x, y = y, fill = as.character(ci_shade1[1]))) +
    scale_fill_brewer(direction = -1,
                      na.translate = FALSE) +
    labs(x = '', y = '',
         fill = "Confidence Interval")
  
  if(length(ci_shade1 > 1)){
    ci_shaderange2 = shieh_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[2])$CI
    p2 = p1 +
      geom_area(data = subset(df.dens, x >= ci_shaderange2[1] & x <= ci_shaderange2[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[2])))
  } else {
    p2 = p1
  }
  
  if(length(ci_shade1 > 2)){
    ci_shaderange3 = shieh_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[3])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange3[1] & x <= ci_shaderange3[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[3])))
  }
  
  if(length(ci_shade1 > 3)){
    ci_shaderange4 = shieh_CI(m1 = m1,
                              m2 = m2,
                              sd1 = sd1,
                              sd2 = sd2,
                              n1 = n1,
                              n2 = n2,
                              unbiased = corr,
                              alternative = "two.sided",
                              conf.level = ci_shade1[4])$CI
    p2 = p2 +
      geom_area(data = subset(df.dens, x >= ci_shaderange4[1] & x <= ci_shaderange4[2]),
                aes(x = x, y = y, fill = as.character(ci_shade1[4])))
  }
  
  p2 = p2 +
    geom_point(data = data.frame(y = 0,
                                 x = d),
               aes(x = x, y = y),
               size = 3) +
    annotate("segment",
             x = ci_linerange[1],
             xend = ci_linerange[2],
             y = 0, yend = 0,
             size = 1.5,
             colour = "black") +
    facet_wrap(~as.character(smd_label)) +
    theme_bw()
  
  
  return(p2)
}



