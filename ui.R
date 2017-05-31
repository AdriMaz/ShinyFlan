# require(shiny)

shinyUI(fluidPage(
  tags$title("Fluctuation Analysis Application using R package flan"),
  tags$h1("Fluctuation Analysis Application using R package flan"),
  sidebarLayout(
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
# 	  tags$hr(),
	column(6,
	  checkboxInput(inputId = "estfit", label = "Unknown fitness", value = TRUE)
	),
	column(6,
	  selectInput(inputId = "model", label = "Growth model", 
	    choices = c("LD" = "LD", "H" = "H"),
	    selected = "LD"
	  )
	)
      ),
      fluidRow(
	column(6,
	  selectInput(inputId = "method", label = "Estimation Method", 
	    choices = c("ML" = "ML", "GF" = "GF", "P0" = "P0"),
	    selected = "ML"
	  )
	),
	conditionalPanel(condition = "input.method == 'ML' || (input.method == 'P0' && input.estfit)",
	  column(6,
	    numericInput(inputId = "winsor", "Winsor parameter", 
	      value = 1024, min = 0
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
	  conditionalPanel(condition = "input.twosample && (input.mfn1 < 0)",
	    numericInput(inputId = "mutations0", "Mutations number difference", 
	      value = 0, step = 0.1
	    )
	  ),
	  conditionalPanel(condition = "!input.twosample && (input.mfn1 < 0)",
	    numericInput(inputId = "mutations0", "Mutations number", 
	      value = 1, min = 0, step = 0.1
	    )
	  ),
	  conditionalPanel(condition = "input.twosample && (input.mfn1 > 0)",
	    numericInput(inputId = "mutprob0", "Mutation probability difference", 
	      value = 0, step = 1e-10
	    )
	  ),
	  conditionalPanel(condition = "!input.twosample && (input.mfn1 > 0)",
	    numericInput(inputId = "mutations0", "Mutation probability", 
	      value = 1, min = 0, step = 1e-10
	    )
	  )
	),
	conditionalPanel(condition = "input.estfit",
	  column(6,
	    conditionalPanel(condition = "input.twosample",
	      numericInput(inputId = "fitness0", "Fitness difference", 
		value = 0, step = 0.1
	      )
	    ),
	    conditionalPanel(condition = "!input.twosample",
	      numericInput(inputId = "fitness0", "Fitness", 
		value = 1, min = 0, step = 0.1
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
	  selectInput(inputId = "mutalt", label = "Mutations number",
	    choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"),
	    selected = "≠"
	  )
	),
	conditionalPanel(condition = "input.estfit",
	  column(6,
	    selectInput(inputId = "fitalt", label = "Fitness",
	      choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"),
	      selected = "≠"
	    )
	  )
	)
      ),
      conditionalPanel(condition = "input.estfit",
	tags$h4(tags$strong("Confidence levels"))
      ),
      conditionalPanel(condition = "!input.estfit",
	tags$h4(tags$strong("Confidence level"))
      ),
      fluidRow(
	column(6,
	  conditionalPanel(condition = "input.mfn1 > 0",
	    numericInput(inputId = "mutconf", label = "Mutation probability",
	      value = 0.95, min = 0, max = 1, step = 0.01
	    )
	  ),
	  conditionalPanel(condition = "input.mfn1 < 0",
	    numericInput(inputId = "mutconf", label = "Mutations number",
	      value = 0.95, min = 0, max = 1, step = 0.01
	    )
	  )
	),
	conditionalPanel(condition = "input.estfit",
	  column(6,
	    numericInput(inputId = "fitconf", label = "Fitness",
	      value = 0.95, min = 0, max = 1, step = 0.01
	    )
	  )
	)
      )
    ),
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
  	  numericInput(inputId = "death1", label = "Death parameter",
	    value = 0, min = 0, max = 0.3, step = 0.01
  	  )
  	),
  	column(2,
  	  numericInput(inputId = "plateff1", label = "Plating efficiency",
	    value = 1, min = 0, max = 1, step = 0.01
  	  )
  	),
	column(2,
	  numericInput(inputId = "mfn1", "Mean Final Number", 
	    value = -1, step = 0.1
	  )
	),
	column(3,
	  numericInput(inputId = "cvfn1", "Coef. Variation Final Number", 
	    value = -1
	  )
  	),
	conditionalPanel(condition = "!(input.estfit)",
	  column(2,
	    numericInput(inputId = "fitvalue1", "Fitness Value", 
	      value = 1, min = 0, max = 10
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
	    numericInput(inputId = "death2", label = "Death parameter",
	      value = 0, min = 0, max = 0.3
	    )
	  ),
	  column(2,
	    numericInput(inputId = "plateff2", label = "Plating efficiency",
	      value = 1, min = 0, max = 1
	    )
	  ),
	  column(2,
	    numericInput(inputId = "mfn2", "Mean Final Number", 
	      value = -1
	    )
	  ),
	  column(3,
	    numericInput(inputId = "cvfn2", "Coef. Variation Final Number", 
	      value = -1
	    )
	  ),
	  conditionalPanel(condition = "!(input.estfit)",
	    column(2,
	      numericInput(inputId = "fitvalue2", "Fitness Value", 
		value = 1, min = 0, max = 10
	      )
	    )
	  )
	)
      ),
      actionButton(inputId = "launch", label = "Launch"),
  #       
  #   ),
  #     ),
  # Print flan.test result
      fluidRow(
	column(10,
	  verbatimTextOutput(outputId = "restest")
	)
      )
    )
  )
)
)
