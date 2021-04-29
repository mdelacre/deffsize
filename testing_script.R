library(tidyr)
test_c= cohen_curv(m1 = 0,
                   m2 = 1,
                   sd1 = 1,
                   sd2 = 1,
                   n1 = 15,
                   n2 = 15,
                   steps = 1000,
                   corr = FALSE,
                   var.equal = TRUE)

p1 = gg_curv_t(test_c[[1]],
           type = "c")

p2 = gg_curv_t(test_c[[2]],
               type = "cd")
