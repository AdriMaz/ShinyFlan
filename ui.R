# require(shiny)

shinyUI(
  fluidPage(
  tabsetPanel(


## TAB 1
    tabPanel("Hypothesis testing",
      tags$title("Fluctuation Analysis Application using R package flan"),
      tags$h1("Fluctuation Analysis Application using R package flan"),
      sidebarLayout(

### SIDEBAR TAB1
      	sidebarPanel(width = 3,
      	  tags$h4(tags$strong("Data file(s)")),
      	  fluidRow(
      	    column(12,
      	      fileInput(inputId = "sample1", label = "Choose File",
            		accept = c(
            		  "text/csv",
            		  "text/comma-separated-values,text/plain",
            		  ".csv")
      	      )
      	    )
      	  ),
      	  fluidRow(
      	    column(4,
      	      checkboxInput(inputId = "header", label = "Header", value = TRUE)
      	    ),
      	    column(8,
      	      checkboxInput(inputId = "twosample", label = "Two Sample Test", value = FALSE)
      	    )
      	  ),
      	  fluidRow(
      	    column(12,
      	      conditionalPanel(condition = "input.twosample",
            		fileInput(inputId = "sample2", label = "Choose File for 2nd sample",
            		  accept = c(
            		    "text/csv",
            		    "text/comma-separated-values,text/plain",
            		    ".csv")
            		)
      	      )
      	    )
      	  ),
          tags$hr(),
      	  tags$h4(tags$strong("Settings")),
      	  fluidRow(
      	    column(6,
      	      checkboxInput(inputId = "estfit", label = "Unknown fitness", value = TRUE)
      	    ),
      	    column(6,
      	      selectInput(inputId = "model", label = "Growth model",
    		        choices = c("LD" = "LD", "H" = "H")
      	      )
      	    )
      	  ),
      	  fluidRow(
      	    column(6,
      	      selectInput(inputId = "method", label = "Estimation Method",
      		      choices = c("ML" = "ML", "GF" = "GF", "P0" = "P0")
      	      )
      	    ),
            conditionalPanel(condition = "input.method == 'ML' || (input.method == 'P0' && input.estfit)",
              column(6,
    # 	    numericInput(inputId = "winsor", "Winsor parameter",
    # 	      value = 1024, min = 0, step = 1
    # 	    )
            		textInput(inputId = "winsor", "Winsor parameter",
            		  value = 1024
            		)
              )
            )
          ),
          tags$hr(),
          conditionalPanel(condition = "input.estfit",
            tags$h4(tags$strong("Null hypotheses"))
          ),
          conditionalPanel(condition = "!input.estfit",
            tags$h4(tags$strong("Null hypothesis"))
          ),
      	  fluidRow(
      	    column(6,
      	      conditionalPanel(condition = "input.twosample && ((input.mfn1 <= 0))",
            		textInput(inputId = "mutdiff0", "Mutation number difference",
            		  value = 0
            		)
      	      ),
              conditionalPanel(condition = "!input.twosample && (input.mfn1 <= 0)",
            		textInput(inputId = "mut0", "Mutation number",
            		  value = 1
            		)
              ),
              conditionalPanel(condition = "input.twosample && (input.mfn1 > 0)",
                textInput(inputId = "mutprobdiff0", "Mutation probability difference",
                  value = 0
                )
              ),
              conditionalPanel(condition = "!input.twosample && (input.mfn1 > 0)",
              	textInput(inputId = "mutprob0", "Mutation probability",
              	  value = 1e-9
              	)
              )
            ),
            conditionalPanel(condition = "input.estfit",
              column(6,
            		conditionalPanel(condition = "input.twosample",
            		  textInput(inputId = "fitdiff0", "Fitness difference",
            		    value = 0
            		  )
            		),
            		conditionalPanel(condition = "!input.twosample",
            		  textInput(inputId = "fit0", "Fitness",
            		    value = 1
            		  )
            		)
      	      )
      	    )
      	  ),
          tags$hr(),
      	  conditionalPanel(condition = "input.estfit",
      	    tags$h4(tags$strong("Alternative hypotheses"))
      	  ),
      	  conditionalPanel(condition = "!input.estfit",
      	    tags$h4(tags$strong("Alternative hypothesis"))
      	  ),
      	  fluidRow(
      	    column(6,
      	      selectInput(inputId = "mutalt", label = "Mutation",
    		        choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
              )
      	    ),
      	    conditionalPanel(condition = "input.estfit",
      	      column(6,
    		        selectInput(inputId = "fitalt", label = "Fitness",
		              choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                )
    	        )
  	        )
          ),
          fluidRow(
      	    column(6,
          		textInput(inputId = "conflevel", label = "Confidence level",
          		  value = 0.95
          		)
          	)
          )
      	),

###### MAIN PANEL TAB 1
        mainPanel(
      	  tags$h4(tags$strong("Sample 1")),
      	  fluidRow(
      	    column(12,
      	      style = 'overflow-x: scroll; position: relative',
      	      tableOutput(outputId = "contents1")
      	    )
      	  ),
      	  fluidRow(
      	    column(2,
      	      textInput(inputId = "death1", label = "Death parameter",
      		      value = 0
      	      )
      	    ),
      	    column(2,
      	      textInput(inputId = "plateff1", label = "Plating efficiency",
      		      value = 1
      	      )
      	    ),
      	    column(2,
	            textInput(inputId = "mfn1", "Mean Final Number",
  		          value = 0
  	          )
	          ),
    	      column(3,
    	        textInput(inputId = "cvfn1", "Coef. Variation Final Number",
  		          value = 0
    	        )
      	    ),
      	    conditionalPanel(condition = "!(input.estfit)",
      	      column(2,
            		textInput(inputId = "fitvalue1", "Fitness",
            		  value = 1
            		)
      	      )
      	    )
      	  ),
      	  conditionalPanel(condition = "input.twosample",
      	    tags$h4(tags$strong("Sample 2")),
      	    fluidRow(
      	      column(12,
            		style = 'overflow-x: scroll; position: relative',
            		tableOutput(outputId = "contents2")
      	      )
      	    ),
      	    fluidRow(
      	      column(2,
            		textInput(inputId = "death2", label = "Death parameter",
            		  value = 0
            		)
      	      ),
      	      column(2,
            		textInput(inputId = "plateff2", label = "Plating efficiency",
            		  value = 1
            		)
      	      ),
      	      column(2,
            		textInput(inputId = "mfn2", "Mean Final Number",
            		  value = 0
            		)
      	      ),
      	      column(3,
            		textInput(inputId = "cvfn2", "Coef. Variation Final Number",
            		  value = 0
            		)
      	      ),
      	      conditionalPanel(condition = "!(input.estfit)",
            		column(2,
            		  textInput(inputId = "fitvalue2", "Fitness",
            		    value = 1
            		  )
            		)
      	      )
	          )
	        ),
      	  fluidRow(
      	    column(3,
              uiOutput(outputId = "launchtest")
      	    ),
    	      # column(3,
    		    #   # uiOutput(outputId = "dlbutton")
    	      # ),
    	      column(3,
    		      uiOutput(outputId = "refresh")
      	    )
      	  ),
      	  fluidRow(
      	    column(10,
      	      verbatimTextOutput(outputId = "restest")
    	      )
  	      )
        )
      )
    ),

### TAB 2
    tabPanel("Graphic representation",
      tags$h1("Fluctuation Analysis Application using R package flan"),
      sidebarLayout(

### SIDEBAR TAB 2
        sidebarPanel(width = 3,
        	tags$h4(tags$strong("Parameters")),
        	  fluidRow(
        	    column(6,
        	      textInput(inputId = "mut.plot", label = "Mutation number",
              		value = 1
         	      )
        	    ),
        	    column(6,
        	      textInput(inputId = "fit.plot", label = "Fitness",
      		        value = 1
        	      )
        	    )
        	  ),
        	  fluidRow(
        	    column(6,
        	      textInput(inputId = "death.plot", label = "Death parameter",
        		      value = 0
        	      )
        	    ),
        	    column(6,
        	      textInput(inputId = "plateff.plot", label = "Plating efficiency",
        		      value = 1
        	      )
        	    )
        	  ),
        	  fluidRow(
        	    column(6,
        	      selectInput(inputId = "model.plot", label = "Growth model",
        		      choices = c("LD" = "LD", "H" = "H")
        	      )
        	    ),
              column(6,
                checkboxInput(inputId = "plot", label = "Plot", value = TRUE)
              )
            ),
          # fluidRow(
          #   column(6,
          #     uiOutput(outputId = "Ndist")
          #     )
          #   ),
	          tags$h4(tags$strong("Graphic settings")),
          	fluidRow(
          	  column(6,
          	    numericInput(inputId = "maxX", label = "Maximal value",
          	      min = 0, value = 100)
          	  ),
          	  column(6,
          	    numericInput(inputId = "nclass", label = "Number of class",
          	      min = 1, max = 100, value = 10)
          	  )
            )
          ),

## MAIN PANEL TAB 2
      	  mainPanel(
            fluidRow(
              plotOutput("graph1")
            ),
            fluidRow(
              conditionalPanel("input.twosample",
                plotOutput("graph2")
              )
            )
        	)
        )
      )
    )
  )
)
