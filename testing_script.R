library(tidyr)
test_c= cohen_curv(m1 = 4,
                   m2 = 0,
                   sd1 = 2,
                   sd2 = 2,
                   n1 = 20,
                   n2 = 20,
                   steps = 10000,
                   corr = TRUE,
                   var.equal = FALSE)

#p1 = gg_curv_t(test_c[[1]],
#           type = "c")

p2 = gg_curv_t(test_c[[2]],
               type = "cd")

plot1 <- plot_cohen_curv(
  smd_label = "Hedges g",
  m1 = 4,
  m2 = 0,
  sd1 = 2,
  sd2 = 2,
  n1 = 20,
  n2 = 20,
  cdf_dat = test_c[[2]],
  corr = TRUE,
  var.equal = FALSE,
  ci_shades = c(.5, .90, .95, .99),
  ci_line = .95
)
