library(shiny)

# Define UI 

ui <- fluidPage(
  
  
  fluidRow( # row 1: titles for descriptive AND estimator   
    
    column(width=3,offset=2,
           h1(strong("Choose the estimator:"), style = "font-size:20px;")
    ),column(width=4,offset=2,
             h1(strong("Enter the sample parameters for each group: "), style = "font-size:20px;")
             
             
    )  
    
  ),fluidRow(br(),
             
  ), 
  
  fluidRow( # row 2   
    
    column(width=2,offset=1,
           radioButtons("corr", label="Correction for bias", choiceNames=list("Correction applied (unbiased estimator)","Correction not applied (biased estimator)"),choiceValues=list("Yes","No"))
    ), column(width=2,
              radioButtons("var", label="Assumption", choiceNames=list("Equal population variances not assumed","Equal population variances assumed"),choiceValues=list("No","Yes"))
    ), column(width=2,offset=1,
              numericInput(inputId="n1", label=withMathJax(helpText('\\(n_1\\) / \\(n_{control}\\)')),value=20, min = 1, max = 1000000,step=1)
    ), column(width=2,offset=1,
              numericInput(inputId="n2", label=withMathJax(helpText('\\(n_2\\) / \\(n_{experimental}\\)')),value=20, min = 1, max = 1000000,step=1)
    )
    
    
  ),fluidRow( # row 3   
    
    column(width=3,offset=1,
           conditionalPanel(
             condition = "input.corr == 'Yes' & input.var == 'No'",
             radioButtons(inputId="ES", label="Estimator", choiceNames=list(
               withMathJax(helpText(paste0('\\(Glass\\)',"'",'\\(s\\)'," ",'\\(g_s\\)'))),
               withMathJax(helpText(paste0('\\(Shieh\\)',"'",'\\(s\\)'," ",'\\(g_s\\)'))),
               withMathJax(helpText(paste0('\\(Hedges\\)',"' ",'\\(g^*_s\\)')))
             ),choiceValues=list("Glass_g","Shieh_g","Hedgesprime_g"),inline=T)
           ),
           conditionalPanel(
             condition = "input.corr == 'Yes' & input.var == 'Yes'",
             radioButtons(inputId="ES", label="Estimator", choiceNames=list(withMathJax(helpText(paste0('\\(Hedges\\)',"' ",'\\(g_s\\)')))),choiceValues=list("Hedges_g"))
           ),
           conditionalPanel(
             condition = "input.corr == 'No' & input.var == 'No'",
             radioButtons(inputId="ES", label="Estimator", choiceNames=list(
               withMathJax(helpText(paste0('\\(Glass\\)',"'",'\\(s\\)'," ",'\\(d_s\\)'))),
               withMathJax(helpText(paste0('\\(Shieh\\)',"'",'\\(s\\)'," ",'\\(d_s\\)'))),
               withMathJax(helpText(paste0('\\(Cohen\\)',"'",'\\(s\\)'," ",'\\(d^*_s\\)')))
             ),choiceValues=list("Glass_d","Shieh_d","Hedgesprime_d"),inline=T)
           ),
           conditionalPanel(
             condition = "input.corr == 'No' & input.var == 'Yes'",
             radioButtons(inputId="ES", label="Estimator", choiceNames=list(withMathJax(helpText(paste0('\\(Cohen\\)',"'",'\\(s\\)'," ",'\\(d_s\\)')))),choiceValues=list("Cohen_d"))
           )
    ), column(width=2,offset=2,
              numericInput("m1", label=withMathJax(helpText('\\(m_1:\\)')), value=0, min = -1000000, max = 1000000,step=.001)
              
              
              
              
    ), column(width=2,offset=1,
              numericInput("m2", label=withMathJax(helpText('\\(m_2:\\)')), value=0, min = -1000000, max = 1000000,step=.001)
    )
    
  ),fluidRow( # row 4   
    
    column(width=5,offset=1,
           numericInput("alpha", label="Choose the nominal alpha level for the C.I.:", value=.05, min = .001, max = 1,step=.001)
           
    ),column(width=2,offset=0,
             numericInput("s1", label=withMathJax(helpText('\\(s_1:\\)')), value=NULL, min = 1, max = 1000000,step=.001)
             
    ),column(width=2,offset=1,
             numericInput("s2", label=withMathJax(helpText('\\(s_2:\\)')), value=NULL, min = 1, max = 1000000,step=.001)
             
    )  
    
    
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
}

shinyApp(ui,server)

