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
      	sidebarPanel(width = 4,
      	  tags$h4(tags$strong("Data file(s)")),
      	  fluidRow(
      	    column(12,
      	      fileInput(inputId = "sample1", label = "Choose file for sample 1",
            		accept = c(
            		  "text/csv",
            		  "text/comma-separated-values,text/plain",
            		  ".csv")
      	      )
      	    )
      	  ),
      	  fluidRow(
      	    column(4,
      	      checkboxInput(inputId = "header", label = tags$strong("Header"), value = TRUE)
      	    ),
      	    column(8,
      	      checkboxInput(inputId = "twosample", label = tags$strong("Two Sample Test"), value = FALSE)
      	    )
      	  ),
      	  fluidRow(
      	    column(12,
      	      conditionalPanel(condition = "input.twosample",
            		fileInput(inputId = "sample2", label = "Choose file for sample 2",
            		  accept = c(
            		    "text/csv",
            		    "text/comma-separated-values,text/plain",
            		    ".csv")
            		)
      	      )
      	    )
      	  ),
          # tags$hr(),
      	  tags$h4(tags$strong("Settings")),
      	  fluidRow(
      	    column(6,
              fluidRow(
      	         checkboxInput(inputId = "estfit", label = tags$strong("Unknown fitness"), value = TRUE),
                 checkboxInput(inputId = "fluct", label = tags$strong("Random final counts"), value = FALSE)
              )
      	    ),
      	    column(6,
      	      selectInput(inputId = "model", label = "Distribution of mutant lifetime",
    		        choices = c("Exponential (LD model)" = "LD", "Constant (H model)" = "H")
      	      )
      	    )
      	  ),
      	  fluidRow(
      	    column(6,
      	      selectInput(inputId = "method", label = "Estimation Method",
      		      choices = c("Maximum Likelihood (ML)" = "ML", "Generating Function (GF)" = "GF", "P0" = "P0")
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
              # uiOutput(outputId = "mut0")
      	      conditionalPanel(condition = "input.twosample & !input.fluct",
            		textInput(inputId = "mutdiff0", "Mutation number difference",
            		  value = 0
            		)
      	      ),
              conditionalPanel(condition = "!input.twosample & !input.fluct",
            		textInput(inputId = "mut0", "Mutation number",
            		  value = 1
            		)
              ),
              conditionalPanel(condition = "input.twosample & input.fluct",
                textInput(inputId = "mutprobdiff0", "Mutation probability difference",
                  value = 0
                )
              ),
              conditionalPanel(condition = "!input.twosample & input.fluct",
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
              conditionalPanel(condition = "input.fluct",
                conditionalPanel(condition = "input.twosample",
                  selectInput(inputId = "mutprobdiffalt", label = "Mutation probability difference",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
                ),
                conditionalPanel(condition = "!input.twosample",
                  selectInput(inputId = "mutprobalt", label = "Mutation probability",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
                )
              ),
              conditionalPanel(condition = "!input.fluct",
                conditionalPanel(condition = "input.twosample",
                  selectInput(inputId = "mutdiffalt", label = "Mutation number difference",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
                ),
                conditionalPanel(condition = "!input.twosample",
                  selectInput(inputId = "mutalt", label = "Mutation number",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
                )
              )
      	    ),
      	    conditionalPanel(condition = "input.estfit",
      	      column(6,
                conditionalPanel(condition = "input.twosample",
                  selectInput(inputId = "fitdiffalt", label = "Fitness difference",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
                ),
                conditionalPanel(condition = "!input.twosample",
                  selectInput(inputId = "fitalt", label = "Fitness",
                    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less")
                  )
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
      	    column(10,
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
            conditionalPanel(condition = "input.fluct",
        	    column(2,
  	            textInput(inputId = "mfn1", "Mean Final Number",
    		          value = 0
    	          )
  	          ),
      	      column(3,
      	        textInput(inputId = "cvfn1", "Coef. Variation Final Number",
    		          value = 0
      	        )
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
      	      column(10,
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
              conditionalPanel(condition = "input.fluct",
        	      column(2,
              		textInput(inputId = "mfn2", "Mean Final Number",
              		  value = 0
              		)
        	      ),
        	      column(3,
              		textInput(inputId = "cvfn2", "Coef. Variation Final Number",
              		  value = 0
              		)
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
      	    column(2,
              uiOutput(outputId = "launchtest")
      	    ),
            column(2,
    		      uiOutput(outputId = "dlbutton")
    	      ),
    	      column(2,
    		      uiOutput(outputId = "refresh")
      	    ),
            column(2,
    		      uiOutput(outputId = "cleanall")
      	    )
      	  ),
          fluidRow(
            column(10,
              # textOutput(outputId = "warn"),
              verbatimTextOutput(outputId = "warn"),
              tags$head(
                tags$style(type='text/css',
                        "#warn{color: blue;
                         font-weight: bold;
                         }"
                         )
              )
            )
          ),
      	  fluidRow(
      	    column(10,
      	      verbatimTextOutput(outputId = "restest")
    	      )
          )
          # ,
          # fluidRow(
  	      # )
        )
      )
    ),

### TAB 2
    tabPanel("Graphic representation",
      tags$h1("Fluctuation Analysis Application using R package flan"),
      sidebarLayout(

### SIDEBAR TAB 2
        sidebarPanel(width = 4,
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
        	      selectInput(inputId = "model.plot", label = "Distribution of mutant lifetime",
        		      choices = c("Exponential (LD model)" = "LD", "Constant (H model)" = "H")
        	      )
        	    ),
              column(6,
                checkboxInput(inputId = "plot", label = tags$strong("Plot distribution with chosen parameters"), value = TRUE)
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
          	    numericInput(inputId = "nclass", label = "Number of class",
          	      min = 1, value = 100)
          	  ),
              column(6,
          	    numericInput(inputId = "max.plot", label = "Maximal value",
          	      min = 0, value = 100)
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
      ),
## TAB 3
      tabPanel("Simulation",
        tags$h1("Fluctuation Analysis Application using R package flan"),
## SIDEBAR TAB 3
        sidebarLayout(
          sidebarPanel(width = 4,
            fluidRow(
              column(6,
                numericInput(inputId = "nsim", label = "Sample size",
                  min = 1, value = 100)
              ),
              column(6,
                actionButton(inputId = "sim", label = tags$strong("Sample"))
              )
            ),
            tags$hr(),
          	tags$h4(tags$strong("Parameters")),
        	  fluidRow(
        	    column(6,
        	      textInput(inputId = "mut.sim", label = "Mutation number",
              		value = 1
         	      )
        	    ),
        	    column(6,
        	      textInput(inputId = "fit.sim", label = "Fitness",
      		        value = 1
        	      )
        	    )
        	  ),
        	  fluidRow(
        	    column(6,
        	      textInput(inputId = "death.sim", label = "Death parameter",
        		      value = 0
        	      )
        	    ),
        	    column(6,
        	      textInput(inputId = "plateff.sim", label = "Plating efficiency",
        		      value = 1
        	      )
        	    )
        	  ),
        	  fluidRow(
        	    column(6,
        	      selectInput(inputId = "model.sim", label = "Distribution of mutant lifetime",
        		      choices = c("Exponential (LD model)" = "LD", "Constant (H model)" = "H", "Log-Normal" = "LN", "Gamma" = "G")
        	      )
        	    )
            ),
            fluidRow(
              column(6,
                uiOutput(outputId = "param1.sim")
        	    ),
              column(6,
                uiOutput(outputId = "param2.sim")
        	    )
            ),
            tags$h4(tags$strong("Graphic settings")),
          	fluidRow(
          	  column(6,
          	    numericInput(inputId = "nclass.sim", label = "Number of class",
          	      min = 1, value = 100)
          	  ),
              column(6,
                numericInput(inputId = "max.sim", label = "Max value",
          	      min = 1, value = 100)
              )
            ),
            fluidRow(
              column(12,
                # uiOutput(outputId = "plot.theo")
                checkboxInput(inputId = "plot.sim", label = tags$strong("Plot theoretical distribution"), value = TRUE)
              )
            )
          ),
          mainPanel(
            tags$h4(tags$strong("Sample")),
            fluidRow(
              column(10,
                # textOutput(outputId = "warn"),
                verbatimTextOutput(outputId = "warn_sim"),
                tags$head(
                  tags$style(type='text/css',
                          "#warn{color: blue;
                           font-weight: bold;
                           }"
                           )
                )
              )
            ),
        	  fluidRow(
        	    column(10,
        	      style = 'overflow-x: scroll; position: relative',
        	      tableOutput(outputId = "contents.sim")
        	    )
            ),
            fluidRow(
              plotOutput("graph.sim")
            ),
            fluidRow(
        	    column(10,
        	      verbatimTextOutput(outputId = "summary.sim")
      	      )
            )
          )
        )
      )
    )
  )
)
