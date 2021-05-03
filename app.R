#install_github("mdelacre/deffectsize")

library(shiny)
library(devtools)
library(deffectsize)
library(ggplot2)
library(distributional)
library(ggdist)
library(grid)
library(gridExtra)
library(ggpubr)

#  (c) from Aaron Caldwell

source("gg_curv_t.R")
source("plot_smd_cdf.R")
source("curv_scripts.R")
# Define UI 

ui <- fluidPage(


  fluidRow( # row 1: titles for descriptive AND estimator   
    
    column(width=10,offset=3,
           h1(strong("Standardised mean difference estimators (\\(d\\)-family) "), style = "font-size:30px;")
    )
  ),
  
  fluidRow( # row 2: titles for descriptive AND estimator   

    column(width=3,offset=2,
           h1(strong("Choose the estimator:"), style = "font-size:20px;")
    ),column(width=4,offset=2,
             h1(strong("Enter the sample parameters for each group: "), style = "font-size:20px;")
             
             
    )  
    
  ),fluidRow(br(),
             
  ), 
  
  fluidRow( # row 3   
    
    column(width=4,offset=1,
              radioButtons("var", label="Assumption", inline=T,choiceNames=list("Equal population variances not assumed","Equal population variances assumed"),choiceValues=list("No","Yes")),
    ), column(width=2,offset=1,
              numericInput(inputId="N1", label=withMathJax('\\(n_1\\) / \\(n_{control}:\\)'),value=20, min = 1, max = 1000000,step=1)
    ), column(width=2,offset=1,
              numericInput(inputId="N2", label=withMathJax('\\(n_2\\) / \\(n_{experimental}:\\)'),value=20, min = 1, max = 1000000,step=1)
    )
    
    
  ),fluidRow( # row 4   
    
    column(width=4,offset=1,
           wellPanel(
             checkboxInput("corr", "Correction for bias", TRUE),

             conditionalPanel(
               condition = "input.corr == 1 & input.var == 'No'",
               checkboxGroupInput("EffsizeUHet",label = h3("Estimator"), 
                                  choices = list("Glass's g" = 1,"Shieh's g" = 2,"Hedges' g*" = 3),
                                     selected = 3,inline=T)
             ),conditionalPanel(
               condition = "input.corr == 1 & input.var == 'Yes'",
               checkboxGroupInput("EffsizeUHom",label = h3("Estimator"), 
                                  choices = list("Hedges'g" = 4),
                                  selected = 4,inline=T)
             ),
             conditionalPanel(
               condition = "input.corr== 0 & input.var == 'No'",
               checkboxGroupInput("EffsizeBHet",label = h3("Estimator"), 
                                  choices = list("Glass's d" = 5,"Shieh's d" = 6,"Hedges' d*" = 7),
                                  selected = 7,inline=T)
             ),
             conditionalPanel(
               condition = "input.corr== 0 & input.var == 'Yes'",
               checkboxGroupInput("EffsizeBHom",label = h3("Estimator"), 
                                  choices = list("Cohen's d" = 8),
                                  selected = 8,inline=T)
             )

        ),

           
    ), column(width=2,offset=1,
              numericInput("M1", label=withMathJax('\\(\\bar{X_1}\\):'), value=0, min = -1000000, max = 1000000,step=.001),
              numericInput("S1", label=withMathJax('\\(s_1:\\)'), value=2, min = 1, max = 1000000,step=.001)

    ), column(width=2,offset=1,
              numericInput("M2", label=withMathJax('\\(\\bar{X_2}\\):'), value=0, min = -1000000, max = 1000000,step=.001),
              numericInput("S2", label=withMathJax('\\(s_2:\\)'), value=2, min = 1, max = 1000000,step=.001)
    )
    
  ),fluidRow( # row 5   
    
    column(width=2,offset=1,

           radioButtons(inputId="hyp", label="Hypothesis:", choiceNames=list(
             withMathJax(paste0('\\(\\mu_1\\)'," \\(\\neq\\) ",'\\(\\mu_2\\)')),
             withMathJax(paste0('\\(\\mu_1\\)'," < ",'\\(\\mu_2\\)')),
             withMathJax(paste0('\\(\\mu_1\\)'," > ",'\\(\\mu_2\\)'))
           ),choiceValues=list("two.sided","less","greater"))),
    
    column(width=2,offset=0,

           numericInput("alpha", label="Nominal \\(\\alpha\\) level:", value=.05, min = .001, max = 1,step=.001),
           # Add button 
           actionButton("designBut","Compute",
                        icon = icon("check-square"))

  ),fluidRow( br(),br(),
    
  ),
  
  wellPanel(
    
  fluidRow( # row 6   
      column(width=4,offset=6,
             br(),
             textOutput("text")
          )
           
    ), fluidRow(
  
      column(width=4,offset=5,
             tableOutput("table")
      )    
      
    ), fluidRow(

      column(width=12,offset=0,
             plotOutput("plot")
             
    )
  
)
  )
    
  )
  
)


# Define server logic to plot various variables against mpg ----
server = function(input, output) {

  
  output$text <-  renderText(paste((1-input$alpha)*100,"% Confidence interval:"))
  
  # Should move 
  observeEvent(input$designBut, {
    
  output$table <- renderTable(

    if(input$corr == 1 & input$var == 'No'){
      
        Glass_g <- 1       %in% input$EffsizeUHet
        Shieh_g <- 2       %in% input$EffsizeUHet
        Hedges_gprime <- 3      %in% input$EffsizeUHet

        CI_out1<-deffectsize::glass_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,unbiased=T, input$hyp)
        CI_out2<-deffectsize::shieh_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,unbiased=T, alternative=input$hyp)
        CI_out3<-deffectsize::cohen_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,var.equal=FALSE,unbiased=T, alternative=input$hyp)

        df1 <- data.frame(Estimator = "Glass's g",
                          Estimate = as.numeric(round(CI_out1$ES,3)),
                          Lower = as.numeric(round(CI_out1$CI[1],3)), 
                          Upper = as.numeric(round(CI_out1$CI[2],3)))
      
        df2 <- data.frame(Estimator = "Shieh's g",
                          Estimate = as.numeric(round(CI_out2$ES,3)),
                          Lower = as.numeric(round(CI_out2$CI[1],3)), 
                          Upper = as.numeric(round(CI_out2$CI[2],3)))
      
        df3 <- data.frame(Estimator = "Hedges' g*",
                          Estimate = as.numeric(round(CI_out3$ES,3)),
                          Lower = as.numeric(round(CI_out3$CI[1],3)), 
                          Upper = as.numeric(round(CI_out3$CI[2],3)))
  
          if (Glass_g & Shieh_g & Hedges_gprime){df <- rbind(df1,df2,df3)
          } else if (Glass_g & Shieh_g){df <- rbind(df1,df2)
          } else if (Glass_g & Hedges_gprime){df <- rbind(df1,df3)
          } else if (Shieh_g & Hedges_gprime){df <- rbind(df2,df3)
          } else if (Glass_g){df <- df1
          } else if (Shieh_g){df <- df2 
          } else if (Hedges_gprime){df <- df3 
          }
      
        format.data.frame(df, digits = 3)
        
      } else if(input$corr == 0 & input$var == 'No'){

        Glass_d <- 5       %in% input$EffsizeBHet
        Shieh_d <- 6       %in% input$EffsizeBHet
        Hedges_dprime <- 7      %in% input$EffsizeBHet
        
        CI_out5<-deffectsize::glass_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,unbiased=F, input$hyp)
        CI_out6<-deffectsize::shieh_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,unbiased=F, alternative=input$hyp)
        CI_out7<-deffectsize::cohen_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,var.equal=FALSE,unbiased=F, alternative=input$hyp)
        
        df5 <- data.frame(Estimator = "Glass's d",
                          Estimate = as.numeric(round(CI_out5$ES,3)),
                          Lower = as.numeric(round(CI_out5$CI[1],3)), 
                          Upper = as.numeric(round(CI_out5$CI[2],3)))
        
        df6 <- data.frame(Estimator = "Shieh's d",
                          Estimate = as.numeric(round(CI_out6$ES,3)),
                          Lower = as.numeric(round(CI_out6$CI[1],3)), 
                          Upper = as.numeric(round(CI_out6$CI[2],3)))
        
        df7 <- data.frame(Estimator = "Hedges' d*",
                          Estimate = as.numeric(round(CI_out7$ES,3)),
                          Lower = as.numeric(round(CI_out7$CI[1],3)), 
                          Upper = as.numeric(round(CI_out7$CI[2],3)))
        
        if (Glass_d & Shieh_d & Hedges_dprime){df <- rbind(df5,df6,df7)
        } else if (Glass_d & Shieh_d){df <- rbind(df5,df6)
        } else if (Glass_d & Hedges_dprime){df <- rbind(df5,df7)
        } else if (Shieh_d & Hedges_dprime){df <- rbind(df6,df7)
        } else if (Glass_d){df <- df5
        } else if (Shieh_d){df <- df6 
        } else if (Hedges_dprime){df <- df7 
        }

        format.data.frame(df, digits = 3)
        
      } else if (input$corr == 1 & input$var == 'Yes') {

        Hedges_g <- 4       %in% input$EffsizeUHom
        
        CI_out4<-deffectsize::cohen_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,var.equal=T,unbiased=T, alternative=input$hyp)
        
        df4 <- data.frame(Estimator = "Hedges' g",
                          Estimate = as.numeric(round(CI_out4$ES,3)),
                          Lower = as.numeric(round(CI_out4$CI[1],3)), 
                          Upper = as.numeric(round(CI_out4$CI[2],3)))
        
        if (Hedges_g){df <- df4}
        
        format.data.frame(df, digits = 3)
        
      } else if (input$corr == 0 & input$var == 'Yes') {

        Cohen_d <- 8       %in% input$EffsizeBHom
        
        CI_out8<-deffectsize::cohen_CI(m1=input$M1,m2=input$M2,sd1=input$S1,sd2=input$S2,n1=input$N1,n2=input$N2,conf.level=1-input$alpha,var.equal=T,unbiased=F, alternative=input$hyp)
        
        df8 <- data.frame(Estimator = "Cohen's d",
                         Estimate = as.numeric(round(CI_out8$ES,3)),
                         Lower = as.numeric(round(CI_out8$CI[1],3)), 
                         Upper = as.numeric(round(CI_out8$CI[2],3)))

        if (Cohen_d){df <- df8}
        
        format.data.frame(df, digits = 3)
        
     }
  
   ) 

  output$plot <- renderPlot(
    
    if(input$corr == 1 & input$var == 'No'){

        M1 <- input$M1
        M2 <- input$M2
        SD1 <- input$S1
        SD2 <- input$S2
        N1 <- input$N1
        N2 <- input$N2
        UNBIASED <- T
        ALT <- input$hyp

        Glass_g <- 1            %in% input$EffsizeUHet
        Shieh_g <- 2            %in% input$EffsizeUHet
        Hedges_gprime <- 3      %in% input$EffsizeUHet

        FCT1 <- glass_CI
        DF1 <- N1-1
        #DF <- input$N1-1
        LAMBDA1 <- (M1-M2)/(SD1*sqrt(1/N1+SD2^2/(N2*SD1^2)))
        #LAMBDA <- ((input$M1-input$M2)/(input$S1*sqrt(1/input$N1+input$S2^2/(input$N2*input$S1^2)))) 
        LABEL1 = "Glass's g"
        corr1 <- gamma(DF1/2)/(sqrt(DF1/2)*gamma((DF1-1)/2))
        MULT1 <- sqrt(1/N1+SD2^2/(N2*SD1^2))*corr1
        #MULT <- sqrt(1/input$N1+input$S2^2/(input$N2*input$S1^2))*corr
        CONF.LEVEL1 <-  1-input$alpha
        
        
        DF2 <- ((SD1^2/N1+SD2^2/N2)^2)/((SD1^2/N1)^2/(N1-1)+(SD2^2/N2)^2/(N2-1))
        #DF <- ((input$S1^2/input$N1+input$S2^2/input$N2)^2)/((input$S1^2/input$N1)^2/(input$N1-1)+(input$S2^2/input$N2)^2/(input$N2-1))
        LAMBDA2 <- (M1-M2)/ sqrt(SD1^2/N1+SD2^2/N2)  
        #LAMBDA <- (input$M1-input$M2)/ sqrt(input$S1^2/input$N1+input$S2^2/input$N2)  
        LABEL2 = "Shieh's g"
        FCT2 <- shieh_CI
        corr2 <- gamma(DF2/2)/(sqrt(DF2/2)*gamma((DF2-1)/2))
        MULT2 <- 1/sqrt(N1+N2)*corr2
        #MULT <- 1/sqrt(input$N1+input$N2)*corr
        CONF.LEVEL2 <-  1-input$alpha
        
        DF3 <- ((N1-1)*(N2-1)*(SD1^2+SD2^2)^2)/((N2-1)*SD1^4+(N1-1)*SD2^4)
        #DF <- ((input$N1-1)*(input$N2-1)*(input$S1^2+input$S2^2)^2)/((input$N2-1)*input$S1^4+(input$N1-1)*input$S2^4)
        LAMBDA3 <- (M1-M2)/ sqrt(SD1^2/N1+SD2^2/N2)
        #LAMBDA <- (input$M1-input$M2)/ sqrt(input$S1^2/input$N1+input$S2^2/input$N2)  
        LABEL3 = "Hedges' g*"
        FCT3 <- cohen_CI
        VAR.EQUAL=F
        corr3 <- gamma(DF3/2)/(sqrt(DF3/2)*gamma((DF3-1)/2))
        MULT3 <- sqrt(1/N1+1/N2)*corr3
        #MULT <- sqrt(1/input$N1+1/input$N2)*corr
        CONF.LEVEL3 <-  1-input$alpha
        
        cdfdat1 = cohen_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED,
                             var.equal = FALSE)

        plot1 <- plot_cohen_curv(smd_label = LABEL1,
                               m1=M1,m2=M2,
                               sd1=SD1,sd2=SD2,
                               n1=N1,n2=N2,
                               cdf_dat = cdfdat1[[2]],
                               corr = UNBIASED,
                               var.equal = FALSE,
                               ci_shades = c(.5, .90, .95, .99),
                               ci_line = CONF.LEVEL1)
        
        cdfdat2 = glass_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED)
        
        plot2 <- plot_glass_curv(smd_label = LABEL2,
                               m1=M1,m2=M2,
                               sd1=SD1,sd2=SD2,
                               n1=N1,n2=N2,
                               cdf_dat = cdfdat2[[2]],
                               corr = UNBIASED,
                               ci_shades = c(.5, .90, .95, .99),
                               ci_line = CONF.LEVEL2)
        
        cdfdat3 = shieh_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED)
        
        plot3 <- plot_shieh_curv(smd_label = LABEL3,
                               m1=M1,m2=M2,
                               sd1=SD1,sd2=SD2,
                               n1=N1,n2=N2,
                               cdf_dat = cdfdat3[[2]],
                               corr = UNBIASED,
                               ci_shades = c(.5, .90, .95, .99),
                               ci_line = CONF.LEVEL3)
        
        if (Glass_g & Shieh_g & Hedges_gprime){
        
          ggarrange(plot1, plot2, plot3, 
                    ncol = 3, nrow = 1,
                    common.legend = TRUE)

        } else if (Glass_g & Shieh_g){

          ggarrange(plot1, plot2, 
                    ncol = 2, nrow = 1,
                    common.legend = TRUE)
          
        } else if (Glass_g & Hedges_gprime){

          ggarrange(plot1, plot3, 
                    ncol = 2, nrow = 1,
                    common.legend = TRUE)

        } else if (Shieh_g & Hedges_gprime){
          
          ggarrange(plot2, plot3, 
                    ncol = 2, nrow = 1,
                    common.legend = TRUE)
          
        } else if(Glass_g){

          plot1
          
        } else if (Shieh_g){

          plot2 
          
        } else if (Hedges_gprime){

          plot3 
          
        }
      
    } else if (input$corr == 0 & input$var == 'No'){

        M1 <- input$M1
        M2 <- input$M2
        SD1 <- input$S1
        SD2 <- input$S2
        N1 <- input$N1
        N2 <- input$N2
        UNBIASED <- F
        ALT <- input$hyp
      
        Glass_d <- 5       %in% input$EffsizeBHet
        Shieh_d <- 6       %in% input$EffsizeBHet
        Hedges_dprime <- 7      %in% input$EffsizeBHet


        FCT1 <- glass_CI
        DF1 <- N1-1
        #DF <- input$N1-1
        LAMBDA1 <- (M1-M2)/(SD1*sqrt(1/N1+SD2^2/(N2*SD1^2)))
        #LAMBDA <- ((input$M1-input$M2)/(input$S1*sqrt(1/input$N1+input$S2^2/(input$N2*input$S1^2)))) 
        LABEL1 = "Glass's d"
        MULT1 <- sqrt(1/N1+SD2^2/(N2*SD1^2))
        #MULT <- sqrt(1/input$N1+input$S2^2/(input$N2*input$S1^2))
        CONF.LEVEL1 <-  1-input$alpha
        
        DF2 <- ((SD1^2/N1+SD2^2/N2)^2)/((SD1^2/N1)^2/(N1-1)+(SD2^2/N2)^2/(N2-1))
        #DF <- ((input$S1^2/input$N1+input$S2^2/input$N2)^2)/((input$S1^2/input$N1)^2/(input$N1-1)+(input$S2^2/input$N2)^2/(input$N2-1))
        LAMBDA2 <- (M1-M2)/ sqrt(SD1^2/N1+SD2^2/N2)  
        #LAMBDA <- (input$M1-input$M2)/ sqrt(input$S1^2/input$N1+input$S2^2/input$N2)  
        LABEL2 = "Shieh's d"
        FCT2 <- shieh_CI
        MULT2 <- 1/sqrt(N1+N2)
        #MULT <- 1/sqrt(input$N1+input$N2)
        CONF.LEVEL2 <-  1-input$alpha
        
        DF3 <- ((N1-1)*(N2-1)*(SD1^2+SD2^2)^2)/((N2-1)*SD1^4+(N1-1)*SD2^4)
        #DF <- ((input$N1-1)*(input$N2-1)*(input$S1^2+input$S2^2)^2)/((input$N2-1)*input$S1^4+(input$N1-1)*input$S2^4)
        LAMBDA3 <- (M1-M2)/ sqrt(SD1^2/N1+SD2^2/N2)
        #LAMBDA <- (input$M1-input$M2)/ sqrt(input$S1^2/input$N1+input$S2^2/input$N2)  
        LABEL3 = "Hedges' d*"
        FCT3 <- cohen_CI
        VAR.EQUAL=F
        MULT3 <- sqrt(1/N1+1/N2)
        #MULT <- sqrt(1/input$N1+1/input$N2)
        CONF.LEVEL3 <-  1-input$alpha
        
          
        cdfdat1 = cohen_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED,
                             var.equal = FALSE)
        
        plot1 <- plot_cohen_curv(smd_label = LABEL1,
                                 m1=M1,m2=M2,
                                 sd1=SD1,sd2=SD2,
                                 n1=N1,n2=N2,
                                 cdf_dat = cdfdat1[[2]],
                                 corr = UNBIASED,
                                 var.equal = FALSE,
                                 ci_shades = c(.5, .90, .95, .99),
                                 ci_line = CONF.LEVEL1)
        
        cdfdat2 = glass_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED)
        
        plot2 <- plot_glass_curv(smd_label = LABEL2,
                                 m1=M1,m2=M2,
                                 sd1=SD1,sd2=SD2,
                                 n1=N1,n2=N2,
                                 cdf_dat = cdfdat2[[2]],
                                 corr = UNBIASED,
                                 ci_shades = c(.5, .90, .95, .99),
                                 ci_line = CONF.LEVEL2)
        
        cdfdat3 = shieh_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED)
        
        plot3 <- plot_shieh_curv(smd_label = LABEL3,
                                 m1=M1,m2=M2,
                                 sd1=SD1,sd2=SD2,
                                 n1=N1,n2=N2,
                                 cdf_dat = cdfdat3[[2]],
                                 corr = UNBIASED,
                                 ci_shades = c(.5, .90, .95, .99),
                                 ci_line = CONF.LEVEL3)
          
          if (Glass_d & Shieh_d & Hedges_dprime){
            
            ggarrange(plot1, plot2, plot3, 
                      ncol = 3, nrow = 1,
                      common.legend = TRUE)
            
          } else if (Glass_d & Shieh_d){
            
            ggarrange(plot1, plot2, 
                      ncol = 2, nrow = 1,
                      common.legend = TRUE)
            
          } else if (Glass_d & Hedges_dprime){
            
            ggarrange(plot1, plot3, 
                      ncol = 2, nrow = 1,
                      common.legend = TRUE)
            
          } else if (Shieh_d & Hedges_dprime){
            
            ggarrange(plot2, plot3, 
                      ncol = 2, nrow = 1,
                      common.legend = TRUE)
            
          } else if(Glass_d){
            
            plot1
            
          } else if (Shieh_d){
            
            plot2 
            
          } else if (Hedges_dprime){
            
            plot3 
            
          }
          

    } else if (input$corr == 1 & input$var == 'Yes'){

      M1 <- input$M1
      M2 <- input$M2
      SD1 <- input$S1
      SD2 <- input$S2
      N1 <- input$N1
      N2 <- input$N2
      UNBIASED <- T
      ALT <- input$hyp

      Hedges_g <- 4       %in% input$EffsizeUHom
      
      if(Hedges_g){
        
        DF <- N1+N2-2
        #DF <- input$N1+input$N2-2
        LAMBDA <- (M1-M2)/ (sqrt(((N1-1)*SD1^2+(N2-1)*SD2^2)/(N1+N2-2))*sqrt(1/N1+1/N2))
        #LAMBDA <- (input$M1-input$M2)/ (sqrt(((input$N1-1)*input$S1^2+(input$N2-1)*input$S2^2)/(input$N1+input$N2-2))*sqrt(1/input$N1+1/input$N2))
        LABEL = "Hedges' g"
        FCT <- cohen_CI
        VAR.EQUAL=T
        corr <- gamma(DF/2)/(sqrt(DF/2)*gamma((DF-1)/2))
        MULT <- sqrt(1/N1+1/N2)*corr
        #MULT <- sqrt(1/input$N1+1/input$N2)*corr
        CONF.LEVEL <-  1-input$alpha
        
        cdfdat1 = cohen_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED,
                             var.equal = FALSE)
        
        plot1 <- plot_cohen_curv(smd_label = LABEL,
                                 m1=M1,m2=M2,
                                 sd1=SD1,sd2=SD2,
                                 n1=N1,n2=N2,
                                 cdf_dat = cdfdat1[[2]],
                                 corr = UNBIASED,
                                 var.equal = FALSE,
                                 ci_shades = c(.5, .90, .95, .99),
                                 ci_line = CONF.LEVEL)
        plot1
        
      } 
      
    } else if (input$corr == 0 & input$var == 'Yes'){

      M1 <- input$M1
      M2 <- input$M2
      SD1 <- input$S1
      SD2 <- input$S2
      N1 <- input$N1
      N2 <- input$N2
      UNBIASED <- F
      ALT <- input$hyp
      
      Cohen_d <- 8       %in% input$EffsizeBHom

      if(Cohen_d){
        
        DF <- N1+N2-2
        #DF <- input$N1+input$N2-2
        LAMBDA <- (M1-M2)/ (sqrt(((N1-1)*SD1^2+(N2-1)*SD2^2)/(N1+N2-2))*sqrt(1/N1+1/N2))
        #LAMBDA <- (input$M1-input$M2)/ (sqrt(((input$N1-1)*input$S1^2+(input$N2-1)*input$S2^2)/(input$N1+input$N2-2))*sqrt(1/input$N1+1/input$N2))
        LABEL = "Cohen's d"
        FCT <- cohen_CI
        VAR.EQUAL=T
        MULT <- sqrt(1/N1+1/N2)
        #MULT <- sqrt(1/input$N1+1/input$N2)
        
        CONF.LEVEL <-  1-input$alpha
        
        cdfdat1 = cohen_curv(m1=M1,m2=M2,
                             sd1=SD1,sd2=SD2,
                             n1=N1,n2=N2,
                             steps = 1000,
                             corr = UNBIASED,
                             var.equal = FALSE)
        
        plot1 <- plot_cohen_curv(smd_label = LABEL,
                                 m1=M1,m2=M2,
                                 sd1=SD1,sd2=SD2,
                                 n1=N1,n2=N2,
                                 cdf_dat = cdfdat1[[2]],
                                 corr = UNBIASED,
                                 var.equal = TRUE,
                                 ci_shades = c(.5, .90, .95, .99),
                                 ci_line = CONF.LEVEL)
        plot1
        
      } 
      
    }
    
  )   })
  

}

shinyApp(ui,server)


