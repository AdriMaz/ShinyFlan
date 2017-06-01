# require(shiny)



# server <- function(input, output, session) {
shinyServer(function(input, output, session) {

require(flan)
#   inFile <- reactive(input$file)

  RV <- reactiveValues(res = c(), fit = 1)
#   , inFile = input$file)

  inFile1 <- reactive(input$sample1)
  inFile2 <- reactive(input$sample2)


  output$contents1 <- renderTable({
    if (is.null(inFile1())) return(NULL)

    data <- read.csv(inFile1()$datapath, header = input$header)

    if(length(data) == 1){
      updateNumericInput(session, "mfn1", value = -1)
      updateNumericInput(session, "cvfn1", value = -1)
    } else if(length(data) == 2) {
      fn  <- data[[2]]
      updateNumericInput(session, "mfn1", value = mean(fn))
      updateNumericInput(session, "cvfn1", value = sd(fn)/mean(fn))
    }

    return(t(data))

  }, colnames = FALSE, rownames = TRUE)

  output$contents2 <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    if (is.null(inFile2())) return(NULL)

    data <- read.csv(inFile2()$datapath, header = input$header)

    if(length(data) == 1){
      updateNumericInput(session, "mfn2", value = -1)
      updateNumericInput(session, "cvfn2", value = -1)
    } else if(length(data) == 2) {
      fn  <- data[[2]]
      updateNumericInput(session, "mfn2", value = mean(fn))
      updateNumericInput(session, "cvfn2", value = sd(fn)/mean(fn))
    }

    return(t(data))

  }, colnames = FALSE, rownames = TRUE)



observeEvent(input$launch, {
#     inFile <- input$file
    if (is.null(inFile1())) return(NULL)

    data1 <- read.csv(inFile1()$datapath, header = input$header)
    alt <- input$mutalt
    clevel <- input$conflevel

    if(!input$twosample){
      fit <- if(input$estfit) NULL else input$fitvalue1
      fit0 <- if(is.null(fit)) input$fitness0 else NULL

      if(is.null(fit)) {
	# clevel <- c(clevel,input$fitconf)
	alt <- c(alt,input$fitalt)
      }

      if(length(data1) == 1){
	mc <- data1[[1]]
	mfn <- if(input$mfn1 < 0) NULL else input$mfn1
	cvfn <- if(input$cvfn1 < 0) NULL else input$cvfn1
	RV$res <- flan.test(mc = mc, mfn = mfn, cvfn = cvfn,
	                    fitness = fit, death = input$death1, plateff = input$plateff1,
	                    model = input$model,
	                    mutations0 = input$mutations0, fitness0 = fit0,
	                    conf.level = clevel,
	                    alternative = alt, method = input$method,
	                    winsor = input$winsor
	                    )
      } else {

	mc <- data1[[1]] ; fn <- data1[[2]]

# 	updateNumericInput(session, "mfn1", value = mean(fn))
# 	updateNumericInput(session, "cvfn1", value = sd(fn)/mean(fn))

	RV$res <- flan.test(mc = mc, fn = fn,
	                    fitness = fit, death = input$death1, plateff = input$plateff1,
	                    model = input$model,
	                    mutations0 = input$mutations0, fitness0 = fit0,
	                    conf.level = clevel,
	                    alternative = alt, method = input$method,
	                    winsor = input$winsor
	                    )


      }

#     } else {
#       if (is.null(inFile2())) return(NULL)
#     }
#
   } else {
    if (is.null(inFile2())) return(NULL)

    fit <- if(input$estfit) NULL else c(input$fitvalue1,input$fitvalue2)
    fit0 <- if(is.null(fit)) input$fitness0 else NULL

    alt <- input$mutalt
    if(is.null(fit)) alt <- c(alt,input$fitalt)


    death <- c(input$death1,input$death2) ; plateff <- c(input$plateff1, input$plateff2)

    data2 <- read.csv(inFile2()$datapath, header = input$header)


    if(length(data1 == 1)){
      if(length(data1) != length(data2)) return(NULL)

      mc <- list(mc1 = data1[[1]], mc2 = data2[[1]])

      mfn1 <- if(input$mfn1 < 0) NULL else input$mfn1
      cvfn1 <- if(input$cvfn1 < 0) NULL else input$cvfn1

      mfn2 <- if(input$mfn2 < 0) NULL else input$mfn2
      if(!(is.null(mfn1) & is.null(mfn2))){
	if(is.null(mfn1)) mfn1 <- mfn2 else mfn2 <- mfn1
      }

      cvfn2 <- if(input$cvfn2 < 0) NULL else input$cvfn2
      if(!(is.null(cvfn1) & is.null(cvfn2))){
	if(is.null(cvfn1)) cvfn1 <- cvfn2 else cvfn2 <- cvfn1
      }

      mfn <- c(mfn1, mfn2) ; cvfn <- c(cvfn1, cvfn2)

#       winsor <- if(input$method == "ML" | (input$method == "P0" & is.null(fit))) input$winsor else 1024

      RV$res <- flan.test(mc = mc, mfn = mfn, cvfn = cvfn,
			  fitness = fit, death = death, plateff = plateff,
			  model = input$model,
			  mutations0 = input$mutations0, fitness0 = fit0,
			  conf.level = clevel,
			  alternative = alt, method = input$method,
			  winsor = input$winsor
			  )

    } else {
      if(length(data1) != length(data2)) return(NULL)

	mc <- list(mc1 = data1[[1]], mc2 = data2[[1]])
	fn <- list(fn1 = data1[[2]], fn2 = data2[[2]])

# 	updateNumericInput(session, "mfn1", value = mean(fn[[1]]))
# 	updateNumericInput(session, "cvfn1", value = sd(fn[[1]])/mean(fn[[1]]))
#
# 	updateNumericInput(session, "mfn2", value = mean(fn[[2]]))
# 	updateNumericInput(session, "cvfn2", value = sd(fn[[2]])/mean(fn[[2]]))

	RV$res <- flan.test(mc = mc, fn = fn,
	                    fitness = fit, death = death, plateff = plateff,
	                    model = input$model,
	                    mutations0 = input$mutations0, fitness0 = fit0,
	                    alternative = alt, method = input$method,
	                    conf.level = clevel,
	                    winsor = input$winsor
	                    )
    }
   }
  }
)

  output$restest <- renderPrint({
      if(length(RV$res) > 0) print(RV$res)
   })


})
