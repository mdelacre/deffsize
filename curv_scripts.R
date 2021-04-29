
cohen_curv = function (m1,
                       m2,
                       sd1,
                       sd2,
                       n1,
                       n2,
                       steps = 10000,
                       corr = TRUE,
                       var.equal = FALSE) {
  intrvls <- (0:steps)/steps

  # Confidence interval of the SMD from Goulet-Pelletier & Cousineau
  results <- suppressWarnings({lapply(intrvls, FUN = function(i) cohen_CI(m1 = m1,
                                                                          m2 = m2,
                                                                          sd1 = sd1,
                                                                          sd2 = sd2,
                                                                          n1 = n1,
                                                                          n2 = n2,
                                                                          var.equal = var.equal,
                                                                          unbiased = corr,
                                                                          alternative = "two.sided",
                                                                          conf.level = i)$CI)})

  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level/2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  class(df) <- c("data.frame", "concurve")
  densdf <- data.frame(c(df$lower.limit, df$upper.limit))
  colnames(densdf) <- "x"
  densdf <- head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")

  return(list(df, densdf))

}

glass_curv = function (m1,
                       m2,
                       sd1,
                       sd2,
                       n1,
                       n2,
                       steps = 10000,
                       corr = TRUE) {
  intrvls <- (0:steps)/steps
  
  # Confidence interval of the SMD from Goulet-Pelletier & Cousineau
  results <- suppressWarnings({lapply(intrvls, FUN = function(i) glass_CI(m1 = m1,
                                                                          m2 = m2,
                                                                          sd1 = sd1,
                                                                          sd2 = sd2,
                                                                          n1 = n1,
                                                                          n2 = n2,
                                                                          unbiased = corr,
                                                                          alternative = "two.sided",
                                                                          conf.level = i)$CI)})
  
  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level/2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  class(df) <- c("data.frame", "concurve")
  densdf <- data.frame(c(df$lower.limit, df$upper.limit))
  colnames(densdf) <- "x"
  densdf <- head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")
  
  return(list(df, densdf))
  
}

shieh_curv = function (m1,
                       m2,
                       sd1,
                       sd2,
                       n1,
                       n2,
                       steps = 10000,
                       corr = TRUE) {
  intrvls <- (0:steps)/steps
  
  # Confidence interval of the SMD from Goulet-Pelletier & Cousineau
  results <- suppressWarnings({lapply(intrvls, FUN = function(i) shieh_CI(m1 = m1,
                                                                          m2 = m2,
                                                                          sd1 = sd1,
                                                                          sd2 = sd2,
                                                                          n1 = n1,
                                                                          n2 = n2,
                                                                          unbiased = corr,
                                                                          alternative = "two.sided",
                                                                          conf.level = i)$CI)})
  
  df <- data.frame(do.call(rbind, results))
  intrvl.limit <- c("lower.limit", "upper.limit")
  colnames(df) <- intrvl.limit
  df$intrvl.width <- (abs((df$upper.limit) - (df$lower.limit)))
  df$intrvl.level <- intrvls
  df$cdf <- (abs(df$intrvl.level/2)) + 0.5
  df$pvalue <- 1 - intrvls
  df$svalue <- -log2(df$pvalue)
  df <- head(df, -1)
  class(df) <- c("data.frame", "concurve")
  densdf <- data.frame(c(df$lower.limit, df$upper.limit))
  colnames(densdf) <- "x"
  densdf <- head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")
  
  return(list(df, densdf))
  
}





