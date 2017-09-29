require(knitr)
require(flan)


# server <- function(input, output, session) {
shinyServer(function(input, output, session) {

  RV <- reactiveValues(file1_state = NULL, file2_state = NULL, data1 = c(), data2 = c(),
                       res = c(), show_res = FALSE, warn = c(),
                       res_sim = c(), warn_sim = c())
   withWarnings <- function(expr) {
       myWarnings <- NULL
       wHandler <- function(w) {
         myWarnings <<- c(myWarnings, list(w))
         invokeRestart("muffleWarning")
       }
       val <- withCallingHandlers(expr, warning = wHandler)
       list(value = val, warnings = myWarnings)
     }


## Reactive for selection file
  inFile1 <- reactive({
    file <- input$sample1
    if(is.null(file) | is.null(RV$file1_state)) {
      return(NULL)
    } else if(RV$file1_state == "reset"){
      return(NULL)
    } else read.csv(file$datapath, header = input$header)
  })

  inFile2 <- reactive({
    file <- input$sample2
    if(is.null(file) | is.null(RV$file2_state)){
      return(NULL)
    } else if(RV$file2_state == "reset"){
      return(NULL)
    } else read.csv(file$datapath, header = input$header)
  })

  observeEvent(input$sample1, {
    RV$file1_state <- "uploaded"
  })

  observeEvent(input$sample2, {
    RV$file2_state <- "uploaded"
  })



## Table of Sample1
  output$contents1 <- renderTable({
    # validate(need(!is.null(inFile1()), "Please load data set"))
    validate(need(RV$file1_state == "uploaded", "Please load data set"))

    ## If a new sample is selected, clean the result part
    if(!is.null(RV$data1)){
      if(length(RV$data1 == 1)){
        if(length(inFile1()) != 1) {
          RV$show_res <- FALSE
        } else if(length(RV$data1[[1]]) != length(inFile1()[[1]])){
          RV$show_res <- FALSE
        } else if(sum(RV$data1[[1]] != inFile1()[[1]]) > 0){
          RV$show_res <- FALSE
          }
        } else {
        if(length(inFile1()) != 2) {
          RV$show_res <- FALSE
        } else if(length(RV$data1[[1]]) != length(inFile1()[[1]]) | length(RV$data1[[2]]) != length(inFile1()[[2]])){
          RV$show_res <- FALSE
        } else if(sum(RV$data1[[1]] != inFile1()[[1]]) > 0){
          RV$show_res <- FALSE
        }
        else if(sum(RV$data1[[2]] != inFile1()[[2]]) > 0){
         RV$show_res <- FALSE
       }
      }
    }
    RV$data1 <- inFile1()
    # if(length(RV$data1) == 1){
    #   if(input$mfn2 > 0 & input$mfn1 == 0){
    #     updateTextInput(session, "mfn1", value = input$mfn2)
    #   }
    #   if(input$cvfn2 > 0 & input$mfn1 == 0 input$cvfn1 == 0){
    #     updateTextInput(session, "cvfn1", value = input$cvfn2)
    #   }
    # } else
    if(length(RV$data1) == 2) {
      updateCheckboxInput(session, "fluct", value = TRUE)
      fn  <- RV$data1[[2]]
## Default value of mfn and cvfn if sample with final count
      updateTextInput(session, "mfn1", value = mean(fn))
      updateTextInput(session, "cvfn1", value = sd(fn)/mean(fn))
#       if(!is.null(inFile2())) {
#         if(as.numeric(input$mfn2) == 0) {
# ## If Sample 2 is non-empty and mfn2 is zero, take value of mfn1
# 	  updateTextInput(session, "mfn2", value = input$mfn1)
#   }
#       if(as.numeric(input$cvfn2) == 0) {
# ## If Sample 2 is non-empty and cvfn2 is zero, take value of cvfn1
#         updateTextInput(session, "cvfn2", value = input$cvfn1)
#       }
#       }
    }

    return(t(RV$data1))

  }, colnames = FALSE, rownames = TRUE)


  output$contents2 <- renderTable({

    ### CF comments in output$contents1

    validate(need(!is.null(inFile2()), "Please load data set"))

    if(!is.null(RV$data2)){
      if(length(RV$data2 == 1)){
        if(length(inFile2()) != 1) {
          RV$show_res <- FALSE
        } else if(length(RV$data2[[1]]) != length(inFile2()[[1]])){
          RV$show_res <- FALSE
        } else if(sum(RV$data2[[1]] != inFile2()[[1]]) > 0){
          RV$show_res <- FALSE
        }
      } else {
        if(length(inFile2()) != 2) {
          RV$show_res <- FALSE
        } else if (length(RV$data2[[1]]) != length(inFile2()[[1]]) | length(RV$data2[[2]]) != length(inFile2()[[2]])){
          RV$show_res <- FALSE
        } else if(sum(RV$data2[[1]] != inFile2()[[1]]) > 0){
          RV$show_res <- FALSE
        }
        else if(sum(RV$data2[[2]] != inFile2()[[2]]) > 0){
         RV$show_res <- FALSE
        }
      }
    }
    RV$data2 <- inFile2()
    # if(length(RV$data2) == 1){
    #   if(input$mfn1 > 0 & input$mfn2 == 0){
    #     updateTextInput(session, "mfn2", value = input$mfn1)
    #   }
    #   if(input$cvfn1 > 0 & input$cvfn2 == 0){
    #     updateTextInput(session, "cvfn2", value = input$cvfn1)
    #   }
    # } else
    if(length(RV$data2) == 2) {
      updateCheckboxInput(session, "fluct", value = TRUE)
      fn  <- RV$data2[[2]]
      updateTextInput(session, "mfn2", value = mean(fn))
      updateTextInput(session, "cvfn2", value = sd(fn)/mean(fn))
#       if(!is.null(inFile1())) {
#         if(as.numeric(input$mfn1) == 0) {
# #           updateNumericInput(session, "mfn1", value = input$mfn2)
# #           if(input$cvfn1 == 0) updateNumericInput(session, "cvfn1", value = input$cvfn2)
#           updateTextInput(session, "mfn1", value = input$mfn2)
#         }
#         if(as.numeric(input$cvfn1) == 0) updateTextInput(session, "cvfn1", value = input$cvfn2)
#       }
    }

    return(t(RV$data2))

  }, colnames = FALSE, rownames = TRUE)



## When hypothesis test is launched
observeEvent(input$launchtest, {
  ## Necessary conditions to perform test
  validate(
  	need(!is.null(inFile1()) || (input$twosample & !is.null(inFile1()) & !is.null(inFile2())), "Please load data set"),

  	need(as.numeric(input$plateff1) >= 0 & as.numeric(input$plateff1) <= 1 & as.numeric(input$plateff2) >=0 & as.numeric(input$plateff2) <= 1, "Plating efficiency must be non-negative and <= 1 number."),

  	need(as.numeric(input$fitvalue1) >= 0 & as.numeric(input$fitvalue2) >= 0, "Fitness must be non-negative number."),

  	need(as.numeric(input$death1) >= 0 & as.numeric(input$death1) <= 1 & as.numeric(input$death2) >=0 & as.numeric(input$death2) <= 1, "Death parameter must be a non-negative and <= 1 number."),

  	need(as.numeric(input$mfn1) >= 0 & as.numeric(input$mfn2) >= 0, "Mean Final Number must be a non-negative number."),

  	need(as.numeric(input$cvfn1) >= 0 & as.numeric(input$cvfn2) >= 0, "Coef. Variation Final Number must be a non-negative number."),

  	need(as.numeric(input$conflevel) >= 0 & as.numeric(input$conflevel) <= 1, "Confidence level must be a non-negative and <= 1 number"),

  	need(as.numeric(input$winsor) >= 0, "Winsor parameter must be a non-negative number"),

  	need(as.numeric(input$mut0) >= 0, "Null mutations number must be a non-negative number"),

  	need(as.numeric(input$mutprob0) >= 0 & as.numeric(input$mutprob0) < 1, "Null mutation probability must be a non-negative and <= 1 number"),

  	need(as.numeric(input$fit0) >= 0, "Null fitness must be a non-negative number")
  )

  if (is.null(inFile1())) return(NULL)

  mfn <- if(as.numeric(input$mfn1) == 0) NULL else as.numeric(input$mfn1)
  cvfn <- if(as.numeric(input$cvfn1) == 0 & is.null(mfn)) NULL else as.numeric(input$cvfn1)

  fit <- if(input$estfit) NULL else as.numeric(input$fitvalue1)

  death <- as.numeric(input$death1)
  plateff <- as.numeric(input$plateff1)

  # alt <- input$mutalt
  alt <- if(input$twosample){
    if(!input$fluct) input$mutdiffalt else input$mutprobdiffalt
  } else {
    if(!input$fluct) input$mutalt else input$mutprobalt
  }


  if(is.null(fit)) alt <- c(alt,if(input$twosample) input$fitdiffalt else input$fitalt)
  clevel <- as.numeric(input$conflevel)
  winsor <- as.numeric(input$winsor)

  if(!input$twosample){

    mc <- RV$data1[[1]]
    fn <- if(length(RV$data1) == 2) RV$data1[[2]] else NULL

    fit0 <- if(is.null(fit)) as.numeric(input$fit0) else NULL
    # RV$res
    test <- withWarnings(if(is.null(fn)){
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
      	      })
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

    if(is.null(fn[[1]]) & is.null(fn[[2]])) fn <- NULL
    if(is.null(mfn[[1]]) & is.null(mfn[[2]])) mfn <- NULL else mfn <- c(mfn[[1]], mfn[[2]])
    if(is.null(cvfn[[1]]) & is.null(cvfn[[2]])) cvfn <- NULL else cvfn <- c(cvfn[[1]], cvfn[[2]])

    # RV$res
    test <- withWarnings(if(is.null(fn)){
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
                # cat("fn = ", fn)
        	      flan.test(mc = mc, fn = fn,
                        fitness = fit, death = death, plateff = plateff,
                        model = input$model,
                        mutprob0 = as.numeric(input$mutprobdiff0), fitness0 = fit0,
                        conf.level = clevel,
                        alternative = alt, method = input$method,
                        winsor = winsor
                )
              })
 }

 RV$res <- test$val
 RV$warn <- test$warnings
 RV$show_res <- TRUE
}


)

output$launchtest <- renderUI({
  if((!input$twosample & !is.null(RV$data1)) || (input$twosample & !is.null(RV$data1) & !is.null(RV$data2))) actionButton(inputId = "launchtest", label = tags$strong("Perform test"))
  # actionButton(inputId = "launchtest", label = "Launch")
  })


output$refresh <- renderUI(
  if(RV$show_res) actionButton(inputId = "refresh", label = tags$strong("Default values"))
)

output$cleanall <- renderUI(
  if(RV$show_res) actionButton(inputId = "cleanall", label = tags$strong("Refresh all"))
)


output$dlbutton <- renderUI(
      if(RV$show_res) downloadButton(outputId = "report", label = tags$strong("Download report"))
)

observeEvent(input$refresh, {

  updateTextInput(session, "death1", value = 0)
  updateTextInput(session, "plateff1", value = 1)
  if(length(RV$data1) == 1){
    updateTextInput(session, "mfn1", value = 0)
    updateTextInput(session, "cvfn1", value = 0)
  }
  updateTextInput(session, "fitvalue1", value = 1)
  updateTextInput(session, "death2", value = 0)
  updateTextInput(session, "plateff2", value = 1)
  if(length(RV$data2) == 1){
    updateTextInput(session, "mfn2", value = 0)
    updateTextInput(session, "cvfn2", value = 0)
  }
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

  if(input$twosample){

  } else {
    if(input$fluct){
      updateSelectInput(session, "mutalt", label = "Mutation", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))
    }
    if(input$estfit) updateSelectInput(session, "fitalt", label = "Fitness", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))
  }

  RV$show_res <- FALSE
  }
)

observeEvent(input$cleanall, {

  updateTextInput(session, "death1", value = 0)
  updateTextInput(session, "plateff1", value = 1)
  if(length(RV$data1) == 1){
    updateTextInput(session, "mfn1", value = 0)
    updateTextInput(session, "cvfn1", value = 0)
  }
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

  updateCheckboxInput(session, "header", value = TRUE)
  updateCheckboxInput(session, "estfit", value = TRUE)
  updateCheckboxInput(session, "twosample", value = FALSE)
  updateCheckboxInput(session, "fluct", value = FALSE)

  updateSelectInput(session, "model", label = "Growth model", choices = c("LD" = "LD", "H" = "H"))
  updateSelectInput(session, "method", label = "Estimation Method", choices = c("ML" = "ML", "GF" = "GF", "P0" = "P0"))

  updateSelectInput(session, "mutalt", label = "Mutation number", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))
  updateSelectInput(session, "fitalt", label = "Fitness", choices = c("≠" = "two.sided", ">" = "greater", "<" = "less"))

  RV$file1_state <- "reset"
  RV$file2_state <- "reset"

  RV$show_res <- FALSE

  }
)


output$report <- downloadHandler(filename = "Report.pdf",
	content = function(file){
	  out <- knit2pdf(input='Report.Rnw', output="Report.tex", clean = TRUE)
 	  file.copy(out, file)},
	contentType = 'application/pdf'
      )

observeEvent(input$twosample, {
  updateTextInput(session, "mutdiff0", value = 0)
  updateTextInput(session, "mutprobdiff0", value = 0)
  updateTextInput(session, "fitdiff0", value = 0)
  if(RV$show_res <- TRUE) RV$show_res <- FALSE
})



observeEvent(!input$twosample, {
  updateTextInput(session, "mut0", value = 1)
  updateTextInput(session, "mutprob0", value = 1e-9)
  updateTextInput(session, "fit0", value = 1)
  if(RV$show_res <- TRUE) RV$show_res <- FALSE
})


observeEvent(input$estfit, {
  updateTextInput(session, "fitvalue1", value = 1)
  updateTextInput(session, "fitvalue2", value = 1)
})


observeEvent(!input$estfit, {
  updateTextInput(session, "fit0", value = 1)
  updateTextInput(session, "fitdiff0", value = 0)
})

observeEvent(!input$fluct, {
  if(length(RV$data1) == 1){
    updateTextInput(session, "mfn1", value = 0)
    updateTextInput(session, "cvfn1", value = 0)
  }
  if(length(RV$data2) == 1){
    updateTextInput(session, "mfn2", value = 0)
    updateTextInput(session, "cvfn2", value = 0)
  }
})
  #
  output$warn <- renderPrint({
    if(RV$show_res) {
      if(!is.null(RV$warn)){
        cat("Warning message(s) \n")
        for (w in RV$warn) cat("-",w$message,"\n")
      }
    }
  })

  # output$warn <- renderText({
  #   txt <- c()
  #   if(RV$show_res) {
  #     if(!is.null(RV$warn)){
  #       txt <- c("Warning message(s)", "\n")
  #       for (w in RV$warn) txt <- c(txt,"-",w$message,"\n")
  #     }
  #   }
  #   paste(txt)
  # })


  output$restest <- renderPrint({

      validate(
	need(as.numeric(input$plateff1) >=0 & as.numeric(input$plateff1) <= 1 & as.numeric(input$plateff2) >= 0 & as.numeric(input$plateff2) <= 1, "Plating efficiency must be a positive and <= 1 number."),

	need(as.numeric(input$death1) >=0 & as.numeric(input$death1) <= 1 & as.numeric(input$death2) >= 0 & as.numeric(input$death2) <= 1, "Death parameter must be a positive and <= 1 number."),

	need(as.numeric(input$fitvalue1) >= 0 & as.numeric(input$fitvalue2) >= 0, "Fitness value must be non-negative number."),

	need(as.numeric(input$mfn1) >= 0 & as.numeric(input$mfn2) >= 0, "Mean Final Number must be 0 or a positive number."),

	need(as.numeric(input$cvfn1) >= 0 & as.numeric(input$cvfn2) >= 0, "Coef. Variation Final Number must be a non-negative number."),

	need(as.numeric(input$conflevel) >= 0 & as.numeric(input$conflevel) <= 1, "Confidence level must be a non-negative and <= 1 number"),

	need(as.numeric(input$winsor) >= 0, "Winsor parameter must be a non-negative number"),

	need(as.numeric(input$mut0) >= 0, "Null mutations number must be a non-negative number"),

	need(as.numeric(input$mutprob0) >= 0 & as.numeric(input$mutprob0) <= 1 , "Null mutation probability must be a non-negative and <= 1 number"),

	need(as.numeric(input$fit0) >= 0, "Null fitness must be a non-negative number")

      )

      if(RV$show_res) {
      #   if(!is.null(RV$warn)){
      #     cat("Warning message(s) \n")
      #     for (w in RV$warn) cat(w$message,"\n")
      #   }
        print(RV$res)
      }

   })



output$graph1 <- renderPlot({



  # X <- 0:input$maxX
  validate(
    need(RV$file1_state == "uploaded", "Please load data set"),
    need(input$nclass > 0, "Number of classes must be positive number"),
    need(input$max.plot > 0, "Maximal value must be positive number"),
    need(input$mut.plot >= 0, "Mutation number must be non-negative number"),
    need(input$fit.plot >= 0, "Fitness must be non-negative number"),
    need(input$death.plot >= 0 & input$death.plot < 0.5, "Death parameter must be non-negative and < 0.5 number"),
    need(input$plateff.plot >= 0 & input$plateff.plot <= 1, "Plating efficiency must be non-negative and <= 1 number")
  )

  mutplot <- as.numeric(input$mut.plot)
  fitplot <- as.numeric(input$fit.plot)
  deathplot <- as.numeric(input$death.plot)
  pefplot <- as.numeric(input$plateff.plot)


  if(!is.null(inFile1())){
    # validate(need(length(RV$data1) == 1 | (input$mfn1 == 0 & input$cvfn1 == 0), "Graphic representation for sample with random final count is not available yet..."))
    validate(need(!input$fluct, "Graphic representation for sample with random final count is not available yet..."))
    mc <- RV$data1[[1]]
    X <- 0:max(mc)
    # updateNumericInput(session, "maxX", value = max(mc))
    hist(mc, nclass = input$nclass, probability = TRUE,
       ylab = "", main = "Empirical distribution of the sample", xlab = "Mutant count", cex.lab = 1.5,
       col = "blue3", cex.axis = 1.5, cex.main = 2, xlim = c(0,input$max.plot))
    leg <- "Sample 1"
    col <- "blue3"
    pch <- 0
    if(RV$show_res) {
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
  # } else {
  #   updateCheckboxInput(session, "plot", value = TRUE)
  #   plot(X,dflan(X,mutations = mutplot, fitness = fitplot, death = deathplot, plateff = pefplot, model = input$model.plot),
  #   type = "l", ylab = "", lwd = 3, col = "red", xlab = "Mutant count", cex.lab = 1.5, cex.axis = 1.5)
  #   legend("topright", inset = c(0.05,0.05), legend = "Distribution with chosen parameters", text.col = "red", cex = 1.5)
  }

  })

  output$graph2 <- renderPlot({

    validate(
      need(RV$file2_state == "uploaded", "Please load data set"),
      need(input$nclass > 0, "Number of classes must be positive number"),
      need(input$max.plot > 0, "Maximal value must be positive number"),
      need(input$mut.plot >= 0, "Mutation number must be non-negative number"),
      need(input$fit.plot >= 0, "Fitness must be non-negative number"),
      need(input$death.plot >= 0 & input$death.plot < 0.5, "Death parameter must be non-negative and < 0.5 number"),
      need(input$plateff.plot >= 0 & input$plateff.plot <= 1, "Plating efficiency must be non-negative and <= 1 number")
    )

    mutplot <- as.numeric(input$mut.plot)
    fitplot <- as.numeric(input$fit.plot)
    deathplot <- as.numeric(input$death.plot)
    pefplot <- as.numeric(input$plateff.plot)

    # X <- 0:input$maxX

    # validate(need(length(RV$data2) == 1 | (input$mfn2 == 0 & input$cvfn2 == 0), "Graphic representation for sample with random final count is not available yet..."))
    validate(need(!input$fluct, "Graphic representation for sample with random final count is not available yet..."))
    if(!is.null(inFile2())){
      mc <- RV$data2[[1]]
      X <- 0:input$max(mc)
      # updateNumericInput(session, "maxX", value = max(c(RV$data1[[1]], mc)))
      hist(mc, nclass = input$nclass, probability = TRUE, ylab = "", xlab = "Mutant count",
      col = "chartreuse3", main = "Empirical distribution of the sample 2",
      cex.lab = 1.5, cex.axis = 1.5, cex.main = 2, xlim = c(0,input$max.plot))
      leg <- "Sample 2"
      col <- "chartreuse3"
      # pch <- "0"
      if(RV$show_res) {
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

    output$param1.sim <- renderUI({
      if(input$model.sim == "LN"){
        textInput(inputId = "meanlog.sim", label = "Mean-log",
          value = -0.3795851
        )
      } else if (input$model.sim == "G"){
        textInput(inputId = "shape.sim", label = "Shape",
          value = 11.18049
        )
      # } else if (input$model.sim == "LD"){
      #   textInput(inputId = "rate.sim", label = "Rate",
      #     value = 1
      #   )
      # } else if (input$model.sim == "H"){
      #   textInput(inputId = "loc.sim", label = "Location",
      #     value = log(2)
      #   )
      }
    })

    output$param2.sim <- renderUI({
      if(input$model.sim == "LN"){
        textInput(inputId = "sdlog.sim", label = "Sd-log",
          value = 0.3016223
        )
      } else if(input$model.sim == "G"){
        textInput(inputId = "scale.sim", label = "Scale",
          value = 0.06395825
        )
      }
    })

    observeEvent(input$sim,{
      validate(
      	need(as.numeric(input$plateff.sim) >= 0 & as.numeric(input$plateff.sim) <= 1, "Plating efficiency must be non-negative and <= 1 number."),

      	need(as.numeric(input$fit.sim) >= 0, "Fitness must be non-negative number."),

      	need(as.numeric(input$death.sim) >= 0, "Death parameter must be a non-negative and <= 1 number.")

      	# need(as.numeric(input$mfn.sim) >= 0,n "Mean Final Number must be a non-negative number."),
        #
      	# need(as.numeric(input$cvfn/sim) >= 0, "Coef. Variation Final Number must be a non-negative number."),

      )
      switch(input$model.sim,
        LD = {lt <- list(name = "exp", rate = 1)},
        H = {lt <- list(name = "dirac", location = 1)},
        LN = {lt <- list(name = "lnorm", meanlog = as.numeric(input$meanlog.sim), sdlog = as.numeric(input$sdlog.sim))},
        G = {lt <- list(name = "gamma", shape = as.numeric(input$shape.sim), scale = as.numeric(input$scale.sim))}
      )

      sim <- withWarnings(rflan(n = input$nsim, mutations = as.numeric(input$mut.sim),
                          fitness = as.numeric(input$fit.sim), death = as.numeric(input$death.sim),
                          plateff = as.numeric(input$plateff.sim),
                          dist=lt))


      RV$res_sim <- sim$val
      RV$warn_sim <- sim$warn

    })

    output$warn_sim <- renderPrint({
      if(!is.null(RV$warn_sim)){
        cat("Warning message(s) \n")
        for (w in RV$warn_sim) cat("-",w$message,"\n")
      }
    })
    output$contents.sim <- renderTable({
      validate(
        need(!is.null(RV$res_sim), "Please simulate sample of mutant counts."),
        need(input$nclass.sim > 0, "Number of classes must be positive number"),
        need(input$max.sim > 0, "Maximal value must be positive number"),
        need(as.numeric(input$mut.sim) >= 0, "Mutation number must be non-negative number"),
        need(as.numeric(input$fit.sim) >= 0, "Fitness must be non-negative number"),
        need(as.numeric(input$death.sim) >= 0 & input$death.sim < 0.5, "Death parameter must be non-negative and < 0.5 number"),
        need(as.numeric(input$plateff.sim) >= 0 & input$plateff.sim <= 1, "Plating efficiency must be non-negative and <= 1 number")
        # need(as.numeric(input$rate.sim) >= 0, "Rate must be positive number"),
        # need(as.numeric(input$loc.sim) >= 0, "Location must be positive number"),
        # need(as.numeric(input$sdlog.sim) >= 0, "Sd-log must be non-negative number"),
        # need(as.numeric(input$shape.sim) >= 0, "Shape must be non-negative number")
      )
      return(t(RV$res_sim$mc))
    }, colnames = FALSE, rownames = FALSE)

    # output$plot.theo <- renderUI({
    #   checkboxInput(inputId = "plot.sim", label = tags$strong("Plot theoretical distribution"), value = TRUE)
    # })

    output$graph.sim <- renderPlot({

      validate(
        need(input$plot.sim | !is.null(RV$res_sim), 'Require a sample or check the plot condition'),
        need(input$nsim > 0, "Sample size must be positive number"),
        need(input$nclass.sim > 0, "Number of classes must be positive number"),
        need(input$max.sim > 0, "Maximal value must be positive number"),
        need(as.numeric(input$mut.sim) >= 0, "Mutation number must be non-negative number"),
        need(as.numeric(input$fit.sim) >= 0, "Fitness must be non-negative number"),
        need(as.numeric(input$death.sim) >= 0 & input$death.sim < 0.5, "Death parameter must be non-negative and < 0.5 number"),
        need(as.numeric(input$plateff.sim) >= 0 & input$plateff.sim <= 1, "Plating efficiency must be non-negative and <= 1 number")
        # need(as.numeric(input$rate.sim) >= 0, "Rate must be positive number"),
        # need(as.numeric(input$loc.sim) >= 0, "Location must be positive number"),
        # need(as.numeric(input$sdlog.sim) >= 0, "Sd-log must be non-negative number"),
        # need(as.numeric(input$shape.sim) >= 0, "Shape must be non-negative number")
      )

      mut <- as.numeric(input$mut.sim)
      fit <- as.numeric(input$fit.sim)
      death <- as.numeric(input$death.sim)
      pef <- as.numeric(input$plateff.sim)

      # validate(need(!input$fluct, "Graphic representation for sample with random final count is not available yet..."))
      X <- 0:input$max.sim
      if(!is.null(RV$res_sim)){
        mc <- RV$res_sim$mc
        # updateNumericInput(session, "maxX", value = max(mc))
        hist(mc, nclass = input$nclass.sim, probability = TRUE,
           ylab = "", main = "Empirical distribution of the sample", xlab = "Mutant count", cex.lab = 1.5,
           col = "blue3", cex.axis = 1.5, cex.main = 2, xlim = c(0,input$max.sim))
        leg <- "Sample"
        col <- "blue3"
        pch <- 0
      # }
        if(input$plot.sim & (input$model.sim == "LD" | input$model.sim == "H")){
          lines(X,dflan(X,mutations = mut, fitness = fit, death = death, plateff = pef, model = input$model.sim), col = "red", lwd = 3)
          leg <- c(leg, "Theoretical distribution")
          col <- c(col, "red")
          pch <- c(pch, "")
          legend("topright", inset = c(0.05,0.05), legend = leg, text.col = col, cex = 1.5)
        }

      } else {
        # cat("cond 1 =", input$plot.sim,"\n")
        # cat("cond 2 =", input$model.sim == "LD" | input$model.sim == "H","\n")
        if(input$plot.sim & (input$model.sim == "LD" | input$model.sim == "H")){
          plot(X,dflan(X,mutations = mut, fitness = fit, death = death, plateff = pef, model = input$model.sim),
          type = "l", ylab = "", lwd = 3, col = "red", xlab = "Mutant count", cex.lab = 1.5, cex.axis = 1.5)
          legend("topright", inset = c(0.05,0.05), legend = "Theoretical distribution", text.col = "red", cex = 1.5)
        }
      }

    })

    output$summary.sim <- renderPrint({
      cat("Sample summary \n")
      validate(need(!is.null(RV$res_sim), 'Require a sample.'))
      if(!is.null(RV$res_sim)){
        print(summary(RV$res_sim$mc))
      }
    })

})
