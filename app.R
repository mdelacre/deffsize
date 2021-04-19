library(shiny)

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
    
    column(width=2,offset=1,
              radioButtons("var", label="Assumption", choiceNames=list("Equal population variances not assumed","Equal population variances assumed"),choiceValues=list("No","Yes")),
    ), column(width=2,offset=3,
              numericInput(inputId="n1", label=withMathJax('\\(n_1\\) / \\(n_{control}:\\)'),value=20, min = 1, max = 1000000,step=1)
    ), column(width=2,offset=1,
              numericInput(inputId="n2", label=withMathJax('\\(n_2\\) / \\(n_{experimental}:\\)'),value=20, min = 1, max = 1000000,step=1)
    )
    
    
  ),fluidRow( # row 4   
    
    column(width=3,offset=1,
           wellPanel(
             checkboxInput("corr", "Correction for bias", TRUE),

             conditionalPanel(
               condition = "input.corr == 1 & input.var == 'No'",
               radioButtons(inputId="ES1", label="Estimator", choiceNames=list(
                 withMathJax(paste0('\\(Glass\\)',"'",'\\(s\\)'," ",'\\(g_s\\)')),
                 withMathJax(paste0('\\(Shieh\\)',"'",'\\(s\\)'," ",'\\(g_s\\)')),
                 withMathJax(paste0('\\(Hedges\\)',"' ",'\\(g^*_s\\)'))
               ),choiceValues=list("Glass_g","Shieh_g","Hedgesprime_g"),inline=T)
             ),conditionalPanel(
               condition = "input.corr == 1 & input.var == 'Yes'",
               radioButtons(inputId="ES2", label="Estimator", choiceNames=list(withMathJax(paste0('\\(Hedges\\)',"' ",'\\(g_s\\)'))),choiceValues=list("Hedges_g"))
             ),
             conditionalPanel(
               condition = "input.corr== 0 & input.var == 'No'",
               radioButtons(inputId="ES3", label="Estimator", choiceNames=list(
                 withMathJax(paste0('\\(Glass\\)',"'",'\\(s\\)'," ",'\\(d_s\\)')),
                 withMathJax(paste0('\\(Shieh\\)',"'",'\\(s\\)'," ",'\\(d_s\\)')),
                 withMathJax(paste0('\\(Cohen\\)',"'",'\\(s\\)'," ",'\\(d^*_s\\)'))
               ),choiceValues=list("Glass_d","Shieh_d","Hedgesprime_d"),inline=T)
             ),
             conditionalPanel(
               condition = "input.corr== 0 & input.var == 'Yes'",
               radioButtons(inputId="ES4", label="Estimator", choiceNames=list(withMathJax(paste0('\\(Cohen\\)',"'",'\\(s\\)'," ",'\\(d_s\\)'))),choiceValues=list("Cohen_d"))
             )
             
                              

        ),

           
    ), column(width=2,offset=2,
              numericInput("m1", label=withMathJax('\\(m_1:\\)'), value=0, min = -1000000, max = 1000000,step=.001)

    ), column(width=2,offset=1,
              numericInput("m2", label=withMathJax('\\(m_2:\\)'), value=0, min = -1000000, max = 1000000,step=.001)
    )
    
  ),fluidRow( # row 5   
    
    column(width=5,offset=1,
           numericInput("alpha", label="Choose the nominal alpha level for the C.I.:", value=.05, min = .001, max = 1,step=.001)
           
    ),column(width=2,offset=0,
             numericInput("s1", label=withMathJax('\\(s_1:\\)'), value=NULL, min = 1, max = 1000000,step=.001)
             
    ),column(width=2,offset=1,
             numericInput("s2", label=withMathJax('\\(s_2:\\)'), value=NULL, min = 1, max = 1000000,step=.001)
             
    )  
    
    
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
}

shinyApp(ui,server)


?checkboxInput()
"checkbox", "Choice A", value = TRUE))
