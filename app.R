library(shiny)

# Define UI 

ui <- fluidPage(
  
  
  fluidRow( # row 1: titles for descriptive AND estimator   
    
    column(width=3,offset=2,
           "Choose the estimator: "
    ),column(width=4,offset=2,
           "Enter the sample parameters for each group: "
    )  
    
  ),fluidRow( # row 2   

       column(width=2,offset=1,
              radioButtons("corr", label="Correction for bias", choiceNames=list("Correction applied (unbiased estimator)","Correction not applied (biased estimator)"),choiceValues=list("Yes","No"))
    ), column(width=2,
              radioButtons("var", label="Assumption", choiceNames=list("Equal population variances not assumed","Equal population variances assumed"),choiceValues=list("No","Yes"))
    ),
    column(width=2,offset=1,
           conditionalPanel(
             condition = "input.ES == 'Glass_d' | input.ES == 'Glass_g'",
             numericInput(inputId="nc", label="nc:",value=20, min = 1, max = 1000000,step=1)
           ),
           conditionalPanel(
             condition = "input.ES != 'Glass_g' & input.ES != 'Glass_d'",
             numericInput(inputId="n1", label="n1:",value=20, min = 1, max = 1000000,step=1)
           )
    ), column(width=2,offset=1,
              conditionalPanel(
                condition = "input.ES == 'Glass_g' | input.ES == 'Glass_d'",
                numericInput(inputId="ne", label="ne:",value=20, min = 1, max = 1000000,step=1)
              ),
              conditionalPanel(
                condition = "input.ES != 'Glass_g' & input.ES != 'Glass_d'",
                numericInput(inputId="n2", label="n2:",value=20, min = 1, max = 1000000,step=1)
              )
    )
    
  ),fluidRow( # row 3   
    
   column(width=2,offset=2,
              conditionalPanel(
                condition = "input.corr == 'Yes' & input.var == 'No'",
                radioButtons(inputId="ES", label="Estimator", choiceNames=list("Glass's g[s]","Shieh's g[s]","Hedges' g^*[s]"),choiceValues=list("Glass_g","Shieh_g","Hedgesprime_g"))
                ),
              conditionalPanel(
                condition = "input.corr == 'Yes' & input.var == 'Yes'",
                radioButtons(inputId="ES", label="Estimator", choiceNames="Hedges' g[s]",choiceValues="Hedges_g")
              ),
              conditionalPanel(
                condition = "input.corr == 'No' & input.var == 'No'",
                radioButtons(inputId="ES", label="Estimator", choiceNames=list("Glass's d[s]","Shieh's d[s]","Hedges' d^*[s]"),choiceValues=list("Glass_d","Shieh_d","Hedgesprime_d"))
              ),
              conditionalPanel(
                condition = "input.corr == 'No' & input.var == 'Yes'",
                radioButtons(inputId="ES", label="Estimator", choiceNames="Cohen's d[s]",choiceValues="Cohen_d")
              )


    ), column(width=2,offset=2,
                 numericInput("m1", label="m1:", value=0, min = -1000000, max = 1000000,step=.001)
    ), column(width=2,offset=1,
              numericInput("m2", label="m2:", value=0, min = -1000000, max = 1000000,step=.001)
    )
    
  ),fluidRow( # row 4   
    
  ),fluidRow( # row 5     
    
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
}

shinyApp(ui,server)

