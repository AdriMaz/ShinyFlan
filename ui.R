require(shinyjs, quietly = TRUE)
require(knitr, quietly = TRUE)
require(flan, quietly = TRUE)
require(xlsx, quietly = TRUE)
require(tools, quietly = TRUE)
require(markdown, quietly = TRUE)

shinyUI(
  fluidPage(
    useShinyjs(),
  tabsetPanel(


## TAB 1
    tabPanel("Hypothesis testing",
      tags$title("Fluctuation Analysis Application using R package flan"),
      tags$h1("Fluctuation Analysis Application using R package flan"),
      sidebarLayout(

### SIDEBAR TAB1
      	sidebarPanel(width = 4,
          fluidRow(
      	    column(3,
              uiOutput(outputId = "launchtest")
      	    ),
            column(3,
              uiOutput(outputId = "dlbutton")
    	      ),
            column(3,
              # uiOutput(outputId = "refresh")
              actionButton(inputId = "defval", label = tags$strong("Default values"))
      	    ),
            column(3,
              # uiOutput(outputId = "refresh")
              actionButton(inputId = "refresh", label = tags$strong("Refresh all"))
      	    )
            # ,
            # column(3,
            #   uiOutput(outputId = "showcode")
      	    # )
      	  ),
      	  tags$h4(tags$strong("Data file(s)")),
      	  fluidRow(
      	    column(12,
      	      fileInput(inputId = "sample1", label = "Choose file for sample 1",
            		accept = c(
            		  # "text/csv",
            		  # "text/comma-separated-values",
                  # "text/plain",
                  # "application/vnd.ms-excel",
                  # "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            		  ".csv", ".txt",
                  ".xls", ".xlsx"
                )
      	      )
      	    )
      	  ),
      	  fluidRow(
      	    column(12,
      	      conditionalPanel(condition = "input.twosample",
                fileInput(inputId = "sample2", label = "Choose file for sample 2",
          		    accept = c(
                    # "text/csv",
              		  # "text/comma-separated-values,text/plain",
                    # "application/vnd.ms-excel",
              		  ".csv", ".txt",
                    ".xls", ".xlsx")
                )
      	      )
      	    )
      	  ),
          fluidRow(
      	    column(4,
      	      checkboxInput(inputId = "header", label = tags$strong("Header"), value = TRUE)
      	    ),
      	    column(4,
      	       selectInput(inputId = "sep", label = "Separator",
      	                       choices = c("," = ",", ";" = ";", "tab" = " ")
      	       )     
      	    ),
      	    column(4,
      	      checkboxInput(inputId = "twosample", label = tags$strong("Two Sample Test"), value = FALSE)
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
            conditionalPanel(condition = "input.method == 'ML' | (input.method == 'P0' & input.estfit)",
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
          ),
          tags$hr(),
          tags$h4(tags$strong("Graphic settings")),
          fluidRow(
            column(6,
              numericInput(inputId = "nclass1", label = "Number of class",
                min = 1, value = 100)
            ),
            column(6,
              numericInput(inputId = "max.plot1", label = "Maximal value",
                min = 0, value = 100)
            )
          ),
          conditionalPanel("input.twosample",
            fluidRow(
              column(6,
                numericInput(inputId = "nclass2", label = "Number of class (Sample 2)",
                  min = 1, value = 100)
              ),
              column(6,
                numericInput(inputId = "max.plot2", label = "Maximal value (Sample 2)",
                  min = 0, value = 100)
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
            conditionalPanel(condition = "input.fluct",
        	    column(2,
  	            textInput(inputId = "mfn1", "Mean final number of cells",
    		          value = 1e9
    	          )
  	          ),
      	      column(3,
      	        textInput(inputId = "cvfn1", "Coef. variation final number of cells",
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
              conditionalPanel(condition = "input.fluct",
        	      column(2,
              		textInput(inputId = "mfn2", "Mean final number of cells",
              		  value = 1e9
              		)
        	      ),
        	      column(3,
              		textInput(inputId = "cvfn2", "Coef. variation final number of cells",
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
          tags$hr(),
          tags$h4(tags$strong("Visual representation")),
          fluidRow(
            column(12,
              plotOutput("graph1")
            )
          ),
          fluidRow(
            conditionalPanel("input.twosample",
              column(12,
                plotOutput("graph2")
              )
            )
          ),
          tags$hr(),
          tags$h4(tags$strong("Result of the test")),
          fluidRow(
            column(6,
              uiOutput("callstest"),
              verbatimTextOutput(outputId = "warn"),
              tags$head(
                tags$style(type='text/css',
                        "#warn{color: blue;
                         font-weight: bold;
                         }"
                         )
              )
            ),
      	    column(6,
      	      verbatimTextOutput(outputId = "restest")
    	      )
          )
        )
      )
    ),

## TAB 2
      tabPanel("Simulation",
        tags$h1("Fluctuation Analysis Application using R package flan"),
## SIDEBAR TAB 2
        sidebarLayout(
          sidebarPanel(width = 4,
            fluidRow(
              column(3,
                actionButton(inputId = "sim", label = tags$strong("Sample"))
              ),
              column(3,
                actionButton(inputId = "refresh.sim", label = tags$strong("Refresh all"))
              ),
              column(3,
                uiOutput(outputId = "est.sim")
                # actionButton(inputId = "est.sim", label = tags$strong("Estimation"))
              ),
              column(3,
                uiOutput(outputId = "dlsample")
                  # actionButton(inputId = "showcodesim", label = tags$strong("Show/Hide Code"))
              )
              # ,
              # column(3,
              #   uiOutput(outputId = "showcodesim")
              # )
              # )
            ),
            tags$hr(),
          	tags$h4(tags$strong("Parameters")),
        	  fluidRow(
              column(6,
                numericInput(inputId = "nsim", label = "Sample size",
                  min = 1, value = 100)
              ),
        	    column(6,
                conditionalPanel(condition = , "!input.fluctsim",
          	      textInput(inputId = "mut.sim", label = "Mutation number",
                		value = 1
           	      )
                ),
                conditionalPanel(condition = , "input.fluctsim",
          	      textInput(inputId = "mutprob.sim", label = "Mutation probability",
                		value = 1e-9
           	      )
                )
        	    )
        	  ),
        	  fluidRow(
              column(6,
        	      textInput(inputId = "fit.sim", label = "Fitness",
      		        value = 1
        	      )
        	    ),
        	    column(6,
        	      textInput(inputId = "death.sim", label = "Death parameter",
        		      value = 0
        	      )
        	    )
        	  ),
        	  fluidRow(
              column(6,
        	      textInput(inputId = "plateff.sim", label = "Plating efficiency",
        		      value = 1
        	      )
        	    ),
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
            fluidRow(
              # column(4,
                checkboxInput(inputId = "fluctsim", label = tags$strong("Random final counts"), value = FALSE)
              # ),
            ),
            conditionalPanel("input.fluctsim",
              fluidRow(
                column(4,
                  selectInput(inputId = "distfn.sim", label = "Distribution of final number of cells",
                    choices = c("Constant" = "C", "Log-Normal" = "LN", "Gamma" = "G")
                  )
                ),
                column(4,
                  textInput(inputId = "mfn.sim", label = "Mean final number of cells", value = 1e9)
                ),
                # conditionalPanel(condition = "input.distfn.sim != 'NO'",
                column(4,
                    # textInput(inputId = "cvfn.sim", label = "Coef. variation final number of cells", value = 0)
                  uiOutput(outputId = "cvfn.sim")
                )
              )
            ),
            tags$hr(),
            tags$h4(tags$strong("Estimation settings")),
            fluidRow(
              column(6,
        	      selectInput(inputId = "model.est", label = "Distribution of mutant lifetime",
      		        choices = c("Exponential (LD model)" = "LD", "Constant (H model)" = "H")
        	      )
              ),
              column(6,
        	      selectInput(inputId = "method.sim", label = "Estimation Method",
        		      choices = c("Maximum Likelihood (ML)" = "ML", "Generating Function (GF)" = "GF", "P0" = "P0")
        	      )
              )
            ),
            fluidRow(
              column(6,
                checkboxInput(inputId = "estfitsim", label = tags$strong("Unknown fitness"),
                  value = TRUE
                )
              ),
              conditionalPanel(condition = "input.method == 'ML' | (input.method == 'P0' & input.estfit)",
                column(6,
              		textInput(inputId = "winsor.sim", "Winsor parameter",
              		  value = 1024
              		)
                )
              )
            ),
            fluidRow(
              column(4,
                textInput(inputId = "death.est", "Death parameter",
                  value = 0
                )
              ),
              column(4,
                textInput(inputId = "plateff.est", "Plating efficiency",
                  value = 1
                )
              ),
              conditionalPanel(condition = "!input.estfitsim",
                column(4,
                  textInput(inputId = "fit.est", "Fitness parameter",
                    value = 1
                  )
                )
              )
            ),
            tags$hr(),
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
              column(6,
                uiOutput(outputId = "plot.theo")
                # conditionalPanel(condition = "input.model.sim == 'LD' | input.model.sim == 'H'",
                #   checkboxInput(inputId = "plot.sim", label = tags$strong("Plot theoretical distribution"), value = TRUE)
                # )
              ),
              column(6,
                uiOutput(outputId = "plot.est")
              )
            )
          ),
          mainPanel(
            tags$h4(tags$strong("Sample")),
            fluidRow(
              column(6,
                # hidden(
                uiOutput(outputId = "callssim")
                # )
              ),
            # ),
            # fluidRow(
              column(6,
                # textOutput(outputId = "warn"),
                verbatimTextOutput(outputId = "warn_sim"),
                tags$head(
                  tags$style(type='text/css',
                          "#warn_sim{color: blue;
                           font-weight: bold;
                           }"
                           )
                )
              )
            ),
        	  fluidRow(
        	    column(12,
        	      style = 'overflow-x: scroll; position: relative',
        	      tableOutput(outputId = "contents.sim")
        	    )
            ),
            tags$hr(),
            tags$h4(tags$strong("Visual representation")),
            fluidRow(
              column(12,
                plotOutput("graph.sim")
              )
            ),
            tags$hr(),
            tags$h4(tags$strong("Estimation")),
            fluidRow(
              column(6,
                uiOutput(outputId = "callsest"),
                verbatimTextOutput(outputId = "warn_est"),
                tags$head(
                  tags$style(type='text/css',
                          "#warn_est{color: blue;
                           font-weight: bold;
                           }"
                           )
                )
              ),
              column(6,
                verbatimTextOutput(outputId = "resest")
              )
            )
          )
        )
      ),
### TAB 4 $: Help tab
    tabPanel("Guide",
      tags$h1("Fluctuation Analysis Application using R package flan"),
      uiOutput(outputId = "guide")
      # includeMarkdown("Guide.md")
    )
    # ,
    # tabPanel("Report",
    #   htmlOutput("report2")
    # )
    )
  )
)
