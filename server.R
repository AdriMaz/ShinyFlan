require(knitr)
require(flan)


# server <- function(input, output, session) {
shinyServer(function(input, output, session) {

  RV <- reactiveValues(res = c(), data1 = c(), data2 = c(), show = FALSE)

## Reactive for selection file
  inFile1 <- reactive({
    file <- input$sample1
    if(is.null(file)) return(NULL) else read.csv(file$datapath, header = input$header)
  })
  inFile2 <- reactive({
    file <- input$sample2
    if(is.null(file)) return(NULL) else read.csv(file$datapath, header = input$header)
  })

## Table of Sample1
  output$contents1 <- renderTable({
    validate(need(!is.null(inFile1()), "Please load data set"))

    ## If a new sample is selected, clean the result part
    if(!is.null(RV$data1)){
      if(length(RV$data1 == 1)){
        if(length(inFile1()) != 1) {
          RV$show <- FALSE
        } else if(sum(RV$data1[[1]] != inFile1()[[1]]) > 0){
          RV$show <- FALSE
          }
        } else {
        if(length(inFile1()) != 2) {
          RV$show <- FALSE
        } else if(sum(RV$data1[[1]] != inFile1()[[1]]) > 0){
          RV$show <- FALSE
        }
        else if(sum(RV$data1[[2]] != inFile1()[[2]]) > 0){
         RV$show <- FALSE
       }
      }
    }
    RV$data1 <- inFile1()
    if(length(RV$data1) == 1){
## Default value of mfn and cvfn if sample without final count
      updateTextInput(session, "mfn1", value = 0)
      updateTextInput(session, "cvfn1", value = 0)
    } else if(length(RV$data1) == 2) {
      fn  <- RV$data1[[2]]
## Default value of mfn and cvfn if sample with final count
      updateTextInput(session, "mfn1", value = mean(fn))
      updateTextInput(session, "cvfn1", value = sd(fn)/mean(fn))
      if(!is.null(inFile2())) {
        if(as.numeric(input$mfn2) == 0) {
## If Sample 2 is non-empty and mfn2 is zero, take value of mfn1
	  updateTextInput(session, "mfn2", value = input$mfn1)
      if(as.numeric(input$cvfn2) == 0) {
## If Sample 2 is non-empty and cvfn2 is zero, take value of cvfn1
        updateTextInput(session, "cvfn2", value = input$cvfn1)
      }
        }
      }
    }

    return(t(RV$data1))

  }, colnames = FALSE, rownames = TRUE)


  output$contents2 <- renderTable({

    ### CF comments in output$contents1

    validate(need(!is.null(inFile2()), "Please load data set"))

    if(!is.null(RV$data2)){
      if(length(RV$data2 == 1)){
        if(length(inFile2()) != 1) {
          RV$show <- FALSE
        } else if(sum(RV$data2[[1]] != inFile2()[[1]]) > 0){
          RV$show <- FALSE
        }
      } else {
        if(length(inFile2()) != 2) {
          RV$show <- FALSE
        } else if(sum(RV$data2[[1]] != inFile2()[[1]]) > 0){
          RV$show <- FALSE
        }
        else if(sum(RV$data2[[2]] != inFile2()[[2]]) > 0){
         RV$show <- FALSE
        }
      }
    }
    RV$data2 <- inFile2()
    if(length(RV$data2) == 1){
      updateTextInput(session, "mfn2", value = 0)
      updateTextInput(session, "cvfn2", value = 0)
    } else if(length(RV$data2) == 2) {
      fn  <- RV$data2[[2]]
      updateTextInput(session, "mfn2", value = mean(fn))
      updateTextInput(session, "cvfn2", value = sd(fn)/mean(fn))
      if(!is.null(inFile1())) {
        if(as.numeric(input$mfn1) == 0) {
#           updateNumericInput(session, "mfn1", value = input$mfn2)
#           if(input$cvfn1 == 0) updateNumericInput(session, "cvfn1", value = input$cvfn2)
          updateTextInput(session, "mfn1", value = input$mfn2)
          if(as.numeric(input$cvfn1) == 0) updateTextInput(session, "cvfn1", value = input$cvfn2)
        }
      }
    }

    return(t(RV$data2))

  }, colnames = FALSE, rownames = TRUE)



## When hypothesis test is launched
observeEvent(input$launchtest, {
  ## Necessary conditions to perform test
  validate(
  	need(!is.null(inFile1()) || (input$twosample && !is.null(inFile1()) && !is.null(inFile2())), "Please load data set"),

  	need(as.numeric(input$plateff1) >= 0 && as.numeric(input$plateff1) <= 1 && as.numeric(input$plateff2) >=0 && as.numeric(input$plateff2) <= 1, "Plating efficiency must be non-negative and <= 1 number."),

  	need(as.numeric(input$fitvalue1) >= 0 && as.numeric(input$fitvalue2) >= 0, "Fitness must be non-negative number."),

  	need(as.numeric(input$death1) >= 0 && as.numeric(input$death1) <= 1 && as.numeric(input$death2) >=0 && as.numeric(input$death2) <= 1, "Death parameter must be a non-negative and <= 1 number."),

  	need(as.numeric(input$mfn1) >= 0 && as.numeric(input$mfn2) >= 0, "Mean Final Number must be a non-negative number."),

  	need(as.numeric(input$cvfn1) >= 0 && as.numeric(input$cvfn2) >= 0, "Coef. Variation Final Number must be a non-negative number."),

  	need(as.numeric(input$conflevel) >= 0 && as.numeric(input$conflevel) <= 1, "Confidence level must be a non-negative and <= 1 number"),

  	need(as.numeric(input$winsor) >= 0, "Winsor parameter must be a non-negative number"),

  	need(as.numeric(input$mut0) >= 0, "Null mutations number must be a non-negative number"),

  	need(as.numeric(input$mutprob0) >= 0, "Null mutation probability must be a non-negative and <= 1 number"),

  	need(as.numeric(input$fit0) >= 0, "Null fitness must be a non-negative number")
  )

  if (is.null(inFile1())) return(NULL)

  mfn <- if(as.numeric(input$mfn1) == 0) NULL else as.numeric(input$mfn1)
  cvfn <- if(as.numeric(input$cvfn1) == 0 & is.null(mfn)) NULL else as.numeric(input$cvfn1)

  fit <- if(input$estfit) NULL else as.numeric(input$fitvalue1)

  death <- as.numeric(input$death1)
  plateff <- as.numeric(input$plateff1)

  alt <- input$mutalt
  if(is.null(fit)) alt <- c(alt,input$fitalt)
  clevel <- as.numeric(input$conflevel)
  winsor <- as.numeric(input$winsor)

  if(!input$twosample){

    mc <- RV$data1[[1]]
    fn <- if(length(RV$data1) == 2) RV$data1[[2]] else NULL

    fit0 <- if(is.null(fit)) as.numeric(input$fit0) else NULL
    RV$res <- if(is.null(fn)){
            		if(is.null(mfn)){
            		  flan.test(mc = mc,
            		    fitness = fit, death = death, plateff = plateff,
            		    model = input$model,
            		    mutations0 = as.numeric(input$mut0), fitness0 = fit0,
            		    conf.level = clevel,
            		    alternative = alt, method = input$method,
            		    winsor = winsor
            		  )
            		} else {
            		  flan.test(mc = mc, mfn = mfn, cvfn = cvfn,
            		    fitness = fit, death = death, plateff = plateff,
            		    model = input$model,
            		    mutprob0 = as.numeric(input$mutprob0), fitness0 = fit0,
            		    conf.level = clevel,
            		    alternative = alt, method = input$method,
            		    winsor = winsor
            		  )
            		}
              } else {
          		  flan.test(mc = mc, fn = fn,
          		    fitness = fit, death = death, plateff = plateff,
          		    model = input$model,
          		    mutprob0 = as.numeric(input$mutprob0), fitness0 = fit0,
          		    conf.level = clevel,
          		    alternative = alt, method = input$method,
          		    winsor = winsor
        		    )
      	      }
  } else {
    if (is.null(inFile2())) return(NULL)

    fit <-  if(!input$estfit) c(fit,as.numeric(input$fitvalue2)) else NULL

    fit0 <- if(is.null(fit)) as.numeric(input$fitdiff0) else NULL

    death <- c(death,as.numeric(input$death2)) ; plateff <- c(plateff, as.numeric(input$plateff2))
    mc <- list(mc1 = RV$data1[[1]], mc2 = RV$data2[[1]])
    fn <- list(fn1 = if(length(RV$data1) == 2) RV$data1[[2]] else NULL, fn2 = if(length(RV$data2) == 2) RV$data2[[2]] else NULL)

    mfn2 <- if(as.numeric(input$mfn2) == 0) NULL else as.numeric(input$mfn2)
    cvfn2 <- if(as.numeric(input$cvfn2) == 0 & is.null(mfn2)) NULL else as.numeric(input$cvfn2)

    mfn <- list(mfn, mfn2) ; cvfn <- list(cvfn, cvfn2)

    if(is.null(fn[[1]]) && is.null(fn[[2]])) fn <- NULL
    if(is.null(mfn[[1]]) && is.null(mfn[[2]])) mfn <- NULL else mfn <- c(mfn[[1]], mfn[[2]])
    if(is.null(cvfn[[1]]) && is.null(cvfn[[2]])) cvfn <- NULL else cvfn <- c(cvfn[[1]], cvfn[[2]])

    RV$res <- if(is.null(fn)){
          	    if(is.null(mfn)){
          	      flan.test(mc = mc,
                    fitness = fit, death = death, plateff = plateff,
                    model = input$model,
                    mutations0 = as.numeric(input$mutdiff0), fitness0 = fit0,
                    conf.level = clevel,
                    alternative = alt, method = input$method,
                    winsor = winsor
                  )
        	      } else {
          	      flan.test(mc = mc, mfn = mfn, cvfn = cvfn,
                    fitness = fit, death = death, plateff = plateff,
                    model = input$model,
                    mutprob0 = as.numeric(input$mutprobdiff0), fitness0 = fit0,
                    conf.level = clevel,
                    alternative = alt, method = input$method,
                    winsor = winsor
                  )
        	      }
        	    } else {
        	      flan.test(mc = mc, fn = fn,
                        fitness = fit, death = death, plateff = plateff,
                        model = input$model,
                        mutprob0 = as.numeric(input$mutprobdiff0), fitness0 = fit0,
                        conf.level = clevel,
                        alternative = alt, method = input$method,
                        winsor = winsor
                )
              }
 }

 RV$show <- TRUE
}


)

output$launchtest <- renderUI({
  if((!input$twosample && !is.null(RV$data1)) || (input$twosample && !is.null(RV$data1) && !is.null(RV$data2))) actionButton(inputId = "launchtest", label = "Launch")
  # actionButton(inputId = "launchtest", label = "Launch")
  })


output$refresh <- renderUI(
  if(RV$show) actionButton(inputId = "refresh", label = "Default values")
)

# output$dlbutton <- renderUI(
#       if(RV$show) downloadButton(outputId = "report", label = "Download report")
# )

# observeEvent(input$refresh, {
observeEvent(input$refresh, {


  # session$reload()

  updateTextInput(session, "death1", value = 0)
  updateTextInput(session, "plateff1", value = 1)
  updateTextInput(session, "mfn1", value = 0)
  updateTextInput(session, "cvfn1", value = 0)
  updateTextInput(session, "fitvalue1", value = 1)
  updateTextInput(session, "death2", value = 0)
  updateTextInput(session, "plateff2", value = 1)
  updateTextInput(session, "mfn2", value = 0)
  updateTextInput(session, "cvfn2", value = 0)
  updateTextInput(session, "fitvalue2", value = 1)

  updateTextInput(session, "winsor", value = 1024)
  updateTextInput(session, "mut0", value = 1)
  updateTextInput(session, "fit0", value = 1)
  updateTextInput(session, "mutprob0", value = 1e-9)
  updateTextInput(session, "mutdiff0", value = 0)
  updateTextInput(session, "fitdiff0", value = 0)
  updateTextInput(session, "mutprobdiff0", value = 0)
  updateTextInput(session, "conflevel", value = 0.95)

  # updateCheckboxInput(session, "header", value = TRUE)
  # updateCheckboxInput(session, "estfit", value = TRUE)

  updateSelectInput(session, "model", label = "Growth model", choices = c("LD" = "LD", "H" = "H"))
  updateSelectInput(session, "method", label = "Estimation Method", choices = c("ML" = "ML", "GF" = "GF", "P0" = "P0"))

  updateSelectInput(session, "mutalt", label = "Mutation", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))
  updateSelectInput(session, "fitalt", label = "Fitness", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))

  RV$show <- FALSE

  # RV$dat
  }
)

# output$report <- downloadHandler(filename = "Report.pdf",
# 	content = function(file){
# 	  out <- knit2pdf(input='Input.Rnw', output="Input.tex", clean = TRUE)
#  	  file.copy(out, file)},
# 	contentType = 'application/pdf'
#       )

observeEvent(input$twosample, {
  updateTextInput(session, "mut0", value = 1)
  updateTextInput(session, "mutprob0", value = 1e-9)
  updateTextInput(session, "fit0", value = 1)
  if(RV$show <- TRUE) RV$show <- FALSE
})


observeEvent(input$estfit, {
  updateTextInput(session, "fitvalue1", value = 1)
  updateTextInput(session, "fitvalue2", value = 1)
})


observeEvent(!input$estfit, {
  updateTextInput(session, "fit0", value = 1)
})

  output$restest <- renderPrint({

      validate(
	need(as.numeric(input$plateff1) >=0 && as.numeric(input$plateff1) <= 1 && as.numeric(input$plateff2) >= 0 && as.numeric(input$plateff2) <= 1, "Plating efficiency must be a positive and <= 1 number."),

	need(as.numeric(input$death1) >=0 && as.numeric(input$death1) <= 1 && as.numeric(input$death2) >= 0 && as.numeric(input$death2) <= 1, "Death parameter must be a positive and <= 1 number."),

	need(as.numeric(input$fitvalue1) >= 0 && as.numeric(input$fitvalue2) >= 0, "Fitness value must be non-negative number."),

	need(as.numeric(input$mfn1) >= 0 && as.numeric(input$mfn2) >= 0, "Mean Final Number must be 0 or a positive number."),

	need(as.numeric(input$cvfn1) >= 0 && as.numeric(input$cvfn2) >= 0, "Coef. Variation Final Number must be a non-negative number."),

	need(as.numeric(input$conflevel) >= 0 && as.numeric(input$conflevel) <= 1, "Confidence level must be a non-negative and <= 1 number"),

	need(as.numeric(input$winsor) >= 0, "Winsor parameter must be a non-negative number"),

	need(as.numeric(input$mut0) >= 0, "Null mutations number must be a non-negative number"),

	need(as.numeric(input$mutprob0) >= 0, "Null mutation probability must be a non-negative and <= 1 number"),

	need(as.numeric(input$fit0) >= 0, "Null fitness must be a non-negative number")

      )

      if(RV$show) print(RV$res)

   })

# output$Ndist <- renderUI({
#   if(length(RV$data1) == 2){
#     selectInput(inputId = "Ndist", label = "Final count distribution",
#       choices = c("Log-normal" = "lnorm", "Gamma" = "gamma"),
#       selected = "Log-Normal")
#   }
# })


# inFileplot <- reactive(input$sample.plot)

# output$contents.plot <- renderTable({
#   validate(need(!is.null(inFileplot()), "Please load data set"))
#
#     RV$data.plot <- read.csv(inFileplot()$datapath, header = input$header)
#     # cat('length(data) = ',length(RV$data1),'\n')
#     # RV$mc1 <- data[[1]]
#
#     return(t(RV$data.plot))
#
#   }, colnames = FALSE, rownames = TRUE)

output$graph1 <- renderPlot({

  mutplot <- as.numeric(input$mut.plot)
  fitplot <- as.numeric(input$fit.plot)
  deathplot <- as.numeric(input$death.plot)
  pefplot <- as.numeric(input$plateff.plot)


  X <- 0:input$maxX


  if(!is.null(inFile1())){
    validate(need(length(RV$data1) == 1, "Graphic representation for sample with random final count is not available yet..."))
    mc <- RV$data1[[1]]
    # updateNumericInput(session, "maxX", value = max(mc))
    hist(mc, nclass = input$nclass, probability = TRUE,
       ylab = "", main = "", xlab = "Mutant count", cex.lab = 1.5,
       col = "blue3", cex.axis = 1.5)
    leg <- "Sample 1"
    col <- "blue3"
    pch <- 0
    if(RV$show) {
      if(!input$twosample){
        mut <- RV$res$estimate[1]
        fit <- if(input$estfit) RV$res$estimate[2] else as.numeric(input$fitvalue1)
      } else {
        mut <- RV$res$estimate[1,1]
        fit <- if(input$estfit) RV$res$estimate[1,2] else as.numeric(input$fitvalue1)
      }
      names(mut) <- NULL ; names(fit) <- NULL
      lines(X,dflan(X,mutations = mut, fitness = fit, death = as.numeric(input$death1), plateff = as.numeric(input$plateff1), model = input$model), col = "goldenrod3", lwd = 3)

      leg <- c(leg, "Distribution with estimates on Sample 1")
      col <- c(col, "goldenrod3")
      pch <- c(pch, "")
    }
    if(input$plot){
      lines(X,dflan(X,mutations = mutplot, fitness = fitplot, death = deathplot, plateff = pefplot, model = input$model.plot), col = "red", lwd = 3)
      leg <- c(leg, "Distribution with chosen parameters")
      col <- c(col, "red")
      pch <- c(pch, "")
    }
    legend("topright", inset = c(0.05,0.05), legend = leg, text.col = col, cex = 1.5)
  } else {
    updateCheckboxInput(session, "plot", value = TRUE)
    plot(X,dflan(X,mutations = mutplot, fitness = fitplot, death = deathplot, plateff = pefplot, model = input$model.plot),
    type = "l", ylab = "", lwd = 3, col = "red", xlab = "Mutant count", cex.lab = 1.5, cex.axis = 1.5)
    legend("topright", inset = c(0.05,0.05), legend = "Distribution with chosen parameters", text.col = "red", cex = 1.5)
  }

  })

  output$graph2 <- renderPlot({
    mutplot <- as.numeric(input$mut.plot)
    fitplot <- as.numeric(input$fit.plot)
    deathplot <- as.numeric(input$death.plot)
    pefplot <- as.numeric(input$plateff.plot)


    X <- 0:input$maxX

    validate(need(length(RV$data2) == 1, "Graphic representation for sample with random final count is not available yet..."))
    if(!is.null(inFile2())){
      mc <- RV$data2[[1]]
      # updateNumericInput(session, "maxX", value = max(c(RV$data1[[1]], mc)))
      hist(mc, nclass = input$nclass, probability = TRUE, ylab = "", xlab = "Mutant count",
      col = "chartreuse3", main ="", cex.lab = 1.5, cex.axis = 1.5)
      leg <- "Sample 2"
      col <- "chartreuse3"
      # pch <- "0"
      if(RV$show) {
        mut <- RV$res$estimate[2,1]
        fit <- if(input$estfit) RV$res$estimate[2,2] else as.numeric(input$fitvalue1)

        names(mut) <- NULL ; names(fit) <- NULL

        lines(X,dflan(X,mutations = mut, fitness = fit, death = as.numeric(input$death2), plateff = as.numeric(input$plateff2), model = input$model), col = "blue", lwd = 3)
        leg <- c(leg, "Distribution with estimates on Sample 2")
        col <- c(col, "blue")
        # pch <- c(pch, "")
      }
      if(input$plot){
        lines(X,dflan(X,mutations = mutplot, fitness = fitplot, death = deathplot, plateff = pefplot, model = input$model.plot), col = "red", lwd = 3)
        leg <- c(leg, "Distribution with chosen parameters")
        col <- c(col, "red")
        # pch <- c(pch, "")
      }
      legend("topright", inset = c(0.05,0.4), legend = leg, text.col = col, cex = 1.5)
    }
    })

})
