#######################################################
#TODO:
# Remember: when updating on server, use library(Cairo)
#######################################################

library(shiny)
library(ggplot2)
library(rms)
library(data.table)
#library(Cairo) # for better graphics on Linux servers
#options(shiny.usecairo= TRUE)

#  colors, from http://www.sron.nl/~pault/
# these are slightly less cb-friendly, but optimized for screen
s.red    <- "#EE3333"
s.orange <- "#EE7722"
s.yellow <- "#FFEE33"
s.green  <- "#66AA55"
s.teal   <- "#11AA99"
s.dkblue <- "#3366AA"
s.magenta<- "#992288"
s.mustard<- "#CCCC55"
myColorScale <- c(
    s.orange,
    s.dkblue,
    s.magenta,
    s.teal  ,
    s.red    ,
    s.yellow,
    s.green ,
    s.mustard
)    

# number of decimal places to use w/ propensity scores
psdig <- 2

# alpha for graphs
alpha1 <- 0.7


# Allow upload of bigger files
# from http://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
# The first number is the number of MB
options(shiny.maxRequestSize=30*1024^2)


shinyServer(function(input, output, session) {
    ############################################################
    ############################################################
    ## Datasets 
    
    inFile <- reactive({
        if (input$useExampleData == 1) return(NULL)
        input$datafile
    })
    
    output$chooseDatafile <- renderUI({
        if (input$useExampleData == 1) return(NULL)
        fileInput('datafile', 
            label= 'Upload file (.csv or .rds only):',
            accept= NULL
            # as far as I can tell, 'accept' does not actually limit anything
            #accept= c(
            #    'text/csv', 
            #    'text/comma-separated-values,text/plain', 
            #    '.csv'#,
            #    #'application/octet-stream'
            #    )
        )
    })
    dset.orig <- reactive({
        # File input from example on shiny website
        # input$datafile will be NULL initially. 
        # After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        if (input$useExampleData == 0 & is.null(inFile())) return(NULL)
    
        if (input$useExampleData == 1) {
            nt <- 300
            nc <- 700
            group <- rep(c("Exposed", "Unexposed"), times= c(nt, nc))
            height_ft <- c(rnorm(nt, 5.4, .3), rnorm(nc, 5.6, .2))
            gender <- c(rbinom(nt, 1, .66), rbinom(nc, 1, .5))
            gender[gender == 0] <- "Male"
            gender[gender == 1] <- "Female"
            age <- c(rnorm(nt, 45, 5), rnorm(nc, 50, 10))
            systolic_bp <- c(rnorm(nt, 115, 5), rnorm(nc, 110, 7)) 
            mydat <- data.table(group, height_ft, gender, age, systolic_bp)
        }  else if (!is.null(inFile())) {
            if (grepl("\\.csv\\>", inFile()$name)) {
                mydat <- fread(inFile()$datapath,
                    sep= ",",
                    header= TRUE,
                    data.table= TRUE
                )
            } else if (grepl("\\.rds\\>", inFile()$name)){
                # todo: add error handling
                mydat <- as.data.table(readRDS(inFile()$datapath))
            }    
        }
        mydat
    })
    
    idvarName <- reactive({
        # The name produced by this function will be used as
        # the name of the id var
        if(is.null(dset.orig())) return (NULL)
        
        proposedName <- "MY__ID"
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "_NEW")    
        }    
        proposedName
    })    
    corevarnames <- reactive({
        # Vector of core variable names for determining missingness.
        if (is.null(varnamesFromModel()) & is.null(varsToView())) return(NULL)

        unique(c(varsToView(), 
            # groupvarname() is part of varnamesFromModel(),
            # but this is here in case model not specified yet
            groupvarname(), varnamesFromModel()))
    })
    output$isnaCopyText <- renderText(paste(corevarnames(), collapse= ", "))
    
    nonMissingIDs <- reactive({
        if (is.null(corevarnames())) return(NULL)
        
        # todo: offer option to impute & add vars
        na.omit(dset.orig()[, c(corevarnames(), idvarName()), with= FALSE])[[idvarName()]]
    })
    
    observe({
        # Add a factor version of the treatment indicator, for plotting
        if (!is.null(groupvarFactorName())){
            if (groupvarname() %in% names(dset.orig())) {
                print("A")
                if (!is.factor(dset.orig()[[groupvarname()]])) {
                    print("A2")
                    dset.orig()[, groupvarFactorName() := factor(dset.orig()[[groupvarname()]])]
                }
                print("B")
            }
        }
    })

    observe({
        # Add an ID variable so we can match obsns w/ the PS dataset
        if (!is.null(idvarName())) { 
            if (!(idvarName() %in% names(dset.orig()))) {
                print("C")
                dset.orig()[, idvarName() := 1:nrow(dset.orig())]
                print("D")
                setkeyv(dset.orig(), idvarName())
            }
        }
    })
    
    dset.psgraphs <- reactive({
        # This dataset is used for making the PS plots.
        if (is.null(logitPS())) return(NULL)
        
        # todo: do this all using data.table
        dat1 <- data.frame(
            id       = unlist(dset.orig()[nonMissingIDs(), idvarName(), with= FALSE]),
            group    = unlist(dset.orig()[nonMissingIDs(), groupvarFactorName(), with= FALSE])
        )
        dat2 <- data.frame(
            id       = PSIDs(),
            logit.ps = logitPS(), 
            ps       = PS()
        )
        dat <- merge(dat1, dat2, by= "id")  # keep only subjects with values in both 
        names(dat)[names(dat) == "id"] <- idvarName()
        
        dat <- as.data.table(dat)  
        setkeyv(dat, idvarName())
        dat
    })    

    observe({
        # for bar plots, need to turn discrete numeric vars into factors
        if (!is.null(varsToView())) {
            for (varname in varsToView()) {
                if (is.numeric(dset.orig()[[varname]]) & 
                    length(unique(dset.orig()[[varname]])) < input$numCont) {
                    print("E")
                    # todo: make sure levels are transferring right
                    if (!is.factor(dset.orig()[[varname]])) {
                        print("E2")
                        dset.orig()[, eval(varname) := factor(dset.orig()[[varname]])]
                        print("E3")
                    }
                    print("F")
                }
            }
        }    
    })    
    
    xgraphs.ids <- reactive({
        # IDs for making the covariate plots.
        if (is.null(nonMissingIDs())) return(NULL)

        intersect(nonMissingIDs(), idsToKeepAfterPruning())
    })    
    
    
    
    ############################################################
    ############################################################
    ## Variable selection, etc.
    
    varnames.orig <- reactive({
        names(dset.orig())  
    })
        
    output$dataDimText <- renderText({
        if (is.null(dset.orig())) return(NULL)
        paste0("The dataset has ", ncol(dset.orig()), 
            " columns and ", nrow(dset.orig()), " rows.")
    })

    output$chooseGroup <- renderUI({
        if (is.null(dset.orig())) return(NULL)
        selectizeInput('treatmentVarName', 
            #'Which variable is the treatment indicator?', 
            label= NULL, 
            choices= varnames.orig()[sapply(dset.orig(), 
                function(vec) length(unique(vec)) == 2)], 
            selected= NULL,
            multiple= FALSE
            )
    })
    
    groupvarname <- reactive({
        input$treatmentVarName
    })
    
    output$groupLevelText <- renderText({
        if (is.null(groupvarname())) return(NULL)
        
        # the as.character lets this print right if var is already a factor
        as.character(sort(unique(dset.orig()[[groupvarname()]])))
    })

    groupvarFactorName <- reactive({
        # The name produced by this function will be used as
        # the name of the treatment group var 
        if (is.null(dset.orig())) return(NULL)
        if (is.null(groupvarname())) return(NULL)
        
        if (is.factor(dset.orig()[[groupvarname()]])) {
            return(groupvarname())    
        }    
        proposedName <- paste0(groupvarname(), '.factor')
        while(proposedName %in% names(dset.orig())) {
            proposedName <- paste0(proposedName, "NEW")    
        }    
        proposedName
    })    


    possVarsToRestrict <- reactive({
        if (is.null(groupvarname())) return(NULL)
        # We don't want to allow restriction of the treatment var 
        setdiff(varnames.orig(), 
            c(groupvarname(), groupvarFactorName(), idvarName()))  
    })    
    
    output$chooseVarsToRestrict <- renderUI({
        selectizeInput('varsToRestrict', 
            NULL, 
            choices= possVarsToRestrict(), 
            selected= if (is.null(varnamesFromModel())) NULL else 
                setdiff(varnamesFromModel(), groupvarname()),
            multiple= TRUE
        )
    })
 
    varsToViewText <- reactive({
        input$varsToRestrict
    })
    
    varsToView <- reactive({
        #dependencies
        if (input$selVarsButton == 0) return(NULL)            

        isolate(varsToViewText())
    })
    
    selVarsNotChecked <- reactive({
        if (input$selVarsButton == 0 | is.null(varsToViewText()) 
            | is.null(varsToView()) ) return (TRUE)

        if(!identical(varsToViewText(), varsToView())) TRUE else FALSE
    })

    output$selVarsNeedCheckingText <- renderText({
        if (selVarsNotChecked()) {
            "Remember to click the button when you're done!"
        } else NULL   
    })


    numvarsToView <- reactive({
        length(varsToView())    
    })    
    
    output$dataNonmissingDimText <- renderText({
        if (is.null(nonMissingIDs())) return(NULL)
        paste0("After removal of rows with missing values for the variables selected for the PS and/or for viewing, ",
            "the dataset has ", length(nonMissingIDs()), " rows.")
    })
    
    output$novarsToViewText <- renderText({
        if (is.null(varsToView())) {
            "To select variables, please return to the Specify tab."
        } else NULL
    })

    

    ############################################################
    ############################################################
    ## Propensity score calculation
    output$getFormula <- renderUI({
        if (is.null(dset.orig()) | is.null(groupvarname())) return(NULL)
        textInput('formulaRHS', 
            label= paste0(groupvarname(), ' ~ '), 
            value= ' '
        )
    })

    formRHS <- reactive({
        # Do nothing if button has never been clicked or if no dset
        # Dependencies
        if (input$psButton == 0 | is.null(dset.orig())) return(NULL) 
        
        isolate(input$formulaRHS)
    })
    
    stringFormula <- reactive({
        paste0(groupvarname(), ' ~ ', formRHS())
    })    
    
    
    psForm <- reactive({
        tryCatch(as.formula(stringFormula()),
            error= function(e) {return(NULL)})
    })    
 
    output$noPSText <- renderText({
        if (is.null(psForm())) {
            "To specify a PS model, please return to the Specify tab."
        } else NULL
    })

    output$psHelpText <- renderText({
        "    age + gender"
    })
    
    psNotChecked <- reactive({
        if (input$psButton == 0 |
            paste0(groupvarname(), ' ~ ', input$formulaRHS) != stringFormula()) TRUE else FALSE
    })

    output$psFormulaProblemText <- renderText({
        # dependencies
        if (psNotChecked()) return(" ")

        if (is.null(psForm())) {
            "That is not a usable RHS. Please try again."   
        } else {
            "Passed"   
        }
    })

    PSIDs <- reactive({
        # dependencies
        if (input$PSCalcUpdateButton == 0) return(nonMissingIDs()) 
        input$psButton # in case model changed after pruning
            
        intersect(nonMissingIDs(), isolate(idsToKeepAfterPruning()))
    })

    varnamesFromModel <- reactive({
        if (is.null(psForm())) return(NULL)
        
        allvars <- all.vars(psForm())
        if (all(allvars %in% names(dset.orig()))) allvars else NULL
    })    

    output$psVarsProblemText <- renderText({
        # dependencies
        if (psNotChecked()) return(" ")

        if (is.null(isolate(varnamesFromModel()))) {
            "The formula uses variables that are not in the dataset. Please try again."   
        } else {
            "Passed"
        }    
    })

    output$psNeedsCheckingText <- renderText({
        if (psNotChecked()) {
            "Remember to click the button when you're done!"
        } else NULL   
    })

    lrmfit <- reactive({
        if (is.null(psForm()) | is.null(varnamesFromModel())) return(NULL)
        
        tryCatch({lrm(psForm(), 
            data= dset.orig()[PSIDs()])},
            error= function(e) {return(NULL)})
    })

    output$psCopyText <- renderText({
        # dependencies
        if(is.null(lrmfit())) return(NULL)
    
        isolate(stringFormula())
    })

    output$psFitProblemTextPrePruning <- renderText({
        # dependencies
        if (psNotChecked() | is.null(varsToView())) return(" ")

        if (is.null(isolate(lrmfit()))) {
            "The propensity score formula can't be fit using the current dataset. Please modify the model and/or the variables selected for viewing."   
        } else {
            "PS model successfully fit."
        }    
    })
    output$psFitProblemTextPostPruning <- renderText({
        # dependencies
        if (psNotChecked() | is.null(varsToView()) | 
            input$PSCalcUpdateButton  == 0) return (NULL)

        if (is.null(isolate(lrmfit()))) {
            "The propensity score formula can't be fit using the current dataset. Please modify the model, the variables selected for viewing, and/or the pruning criteria."   
        } else {
            NULL
        }    
    })

    logitPS <- reactive({
        if (is.null(lrmfit())) return(NULL)
        
        lrmfit()$linear.predictors
    })

    PS <- reactive({
        if (is.null(logitPS())) return(NULL)
        
        exp(logitPS()) / (1 + exp(logitPS()))
    })
    
    
    ############################################################
    ############################################################
    ## Reactive text, etc. for PS graphs
    
    useLogit <- reactive({
        !(input$useProbScale)
    })
    
    psbrushmin <- reactive({
        if (is.null(dset.psgraphs()) |
            identical(buttonvalues$lastActionRug, 'choosePlot')) return(NULL)
        if (useLogit()) input$logitpsPlot_brush$xmin else input$psPlot_brush$xmin
    })
    
    psbrushmax <- reactive({
        if (is.null(dset.psgraphs()) |
            identical(buttonvalues$lastActionRug, 'choosePlot')) return(NULL)
        if (useLogit()) input$logitpsPlot_brush$xmax else input$psPlot_brush$xmax
    })
    
    scorename <- reactive({
        ifelse(useLogit(), "logit.ps", "ps")
    })
    idsForRug <- reactive({
        if (is.null(dset.psgraphs())) return(NULL)
        
        ids <- dset.psgraphs()[round(get(scorename()), psdig) < psbrushmin() |
            round(get(scorename()), psdig) > psbrushmax()][[idvarName()]]
        
        intersect(ids, xgraphs.ids())
    })  
    

    ############################################################
    ############################################################
    ## Reactive text related to covariate graphs
        
    varIsContinuous <- reactive({
        if (is.null(varsToView())) return(NULL)
        
        myvec <- rep(FALSE, numvarsToView())
        
        for(i in 1:numvarsToView()) {
            varname <- varsToView()[i]
            
            if (is.numeric(dset.orig()[[varname]]) & 
                length(unique(dset.orig()[[varname]])) >= 
                input$numCont) myvec[i] <- TRUE
        }
        myvec
    })    

    # number of decimal places to use w/ covariate graphs
    xdig <- reactive({
        input$xDigits
    })

    # from http://stackoverflow.com/questions/18816666/shiny-change-data-input-of-buttons
    # Create a reactiveValues object, to let us use settable reactive values
    buttonvalues <- reactiveValues()
    # To start out, lastActionX == NULL, meaning nothing clicked yet
    buttonvalues$lastActionX <- NULL
    # An observe block for each button, to record that the action happened
    observe({
        if (input$xgraphsUpdateButton != 0 | input$PSCalcUpdateButton != 0) {
            buttonvalues$lastActionX <- 'prune'
        }
    })
    observe({
        if (input$psButton != 0 | input$selVarsButton != 0) {
            buttonvalues$lastActionX <- 'specify'
        }
    })
    # and now one for the rug plots
    buttonvalues$lastActionRug <- NULL
    observe({
        if (useLogit() | !useLogit()) {
            buttonvalues$lastActionRug <- 'choosePlot'
        }
    })
    observe({
        if (!is.null(input$logitpsPlot_brush$xmin) | 
            !is.null(input$psPlot_brush$xmin) | 
            !is.null(input$logitpsPlot_brush$xmax) | 
            !is.null(input$psPlot_brush$xmax)) { 
            buttonvalues$lastActionRug <- 'brush'
        }
    })


    pruneValRawList <- reactive({
        if (is.null(varsToView())) return(NULL)

        mylist <- vector("list", numvarsToView())

        for (i in 1:numvarsToView()) {
            mylist[[i]]  <- input[[paste0("pruningChoices_", i)]]
        }
        mylist
    })

    pruneValTextList <- reactive({
        # dependencies
        if (is.null(varsToView())) return(NULL)
        if ((input$xgraphsUpdateButton == 0 & input$PSCalcUpdateButton == 0) | 
            identical(buttonvalues$lastActionX, 'specify'))  {
            return(NULL)
        }
        
        mylist <- vector("list", numvarsToView())
        
        for(i in 1:numvarsToView()) {
            myvals  <- isolate(pruneValRawList())[[i]]
            varname <- varsToView()[i]
            
            # We are coding for the ones to KEEP
            mylist[[i]] <- if (varIsContinuous()[i]) {
                myvals_numeric <- suppressWarnings(as.numeric(
                    unlist(strsplit(as.character(myvals), " "))))
                if (length(na.omit(myvals_numeric)) == 2) {
                    paste0(varname, " >= ", myvals_numeric[1], 
                    " & ", varname, " <= ", myvals_numeric[2])
                } else { # user entered invalid text
                    TRUE
                }
            } else { # var is not continuous
                if (is.numeric(dset.orig()[[varname]])) {
                    paste0(varname, " %in% c(", paste(myvals, collapse= ","),")") 
                } else { # character var
                    paste0("(", paste(varname, " == ", 
                        paste0("'", myvals, "'"), collapse= " | "),
                        ")")
                }    
            }
        } # next i
        mylist
    })    

    exprToKeepAfterPruning <- reactive({
        if (is.null(pruneValTextList())) return(NULL)
        
        do.call("paste", list(pruneValTextList(), collapse= " & " ))
    })    
    
    output$keepAfterPruningCopyText <- renderUI({
        if (is.null(pruneValTextList())) return(NULL)

        HTML(do.call("paste", list(pruneValTextList(), collapse= " & <br/>" )))
    })    
    
    idsToKeepAfterPruning <- reactive({
        if (is.null(nonMissingIDs())) return(NULL)

        if (is.null(exprToKeepAfterPruning())) return(nonMissingIDs())
        
        # TODO: I don't remember why I have this error handling here.
        #    Doesn't seem like the best idea.
        tryCatch(dset.orig()[nonMissingIDs()][eval(parse(text= 
            exprToKeepAfterPruning()))][[idvarName()]],
            error= function(e) nonMissingIDs())
    })    
    
    output$pruneTable <- renderTable({
        if (is.null(nonMissingIDs())) return(NULL)

        dat <- data.frame(addmargins(table(
            unlist(dset.orig()[xgraphs.ids(), groupvarFactorName(), with= FALSE]), 
            dnn= groupvarname())))
        rownames(dat) <- dat[, 1]
        rownames(dat)[rownames(dat) == "Sum"] <- "Total"
        names(dat)[2] <- "n"
        dat[, 2, drop= FALSE]
    }, display= c("s", "d"))

    ############################################################
    ############################################################
    ## Plotting 
    
    colorScale.mod <- reactive({
        if (is.null(nonMissingIDs())) return(NULL)
        myColorScale[1:length(unique(dset.orig()[[groupvarFactorName()]]))]
    })
    
    output$psPlot <- renderPlot({
        # todo: change if I eliminate dset.psgraphs
        if (is.null(dset.psgraphs())) return(NULL)
        
        p <- ggplot(data= dset.psgraphs(),
            aes(x= ps)) +
            geom_histogram(
                alpha    = alpha1, 
                position = 'identity', 
                aes(fill= group)) +
            scale_fill_manual(groupvarname(), values= colorScale.mod()) +
            xlab(paste0("PS (n= ", nrow(dset.psgraphs()), ")")) +
            theme(legend.position= "right")

        # it is very important to have just p here, not print(p)!
        p 
    })    
    
    output$psPlotui <- renderUI({
        if (is.null(dset.psgraphs())) return(NULL)

        plotOutput("psPlot", 
            height = 300,
            brush  = if (useLogit() == FALSE) {
                brushOpts(
                    id = "psPlot_brush",
                    delay = 300,
                    delayType = "debounce",
                    direction = "x",
                    resetOnNew = TRUE
                )
            } else NULL
        )
    })
    
    output$logitpsPlot <- renderPlot({
        if (is.null(dset.psgraphs())) return(NULL)

        p <- ggplot(data= dset.psgraphs(),
            aes(x= logit.ps)) +
            geom_histogram(
                alpha    = alpha1, 
                position = 'identity', 
                aes(fill= group)) +
            scale_fill_manual(groupvarname(), values= colorScale.mod()) +
            xlab(paste0("Logit PS (n= ", nrow(dset.psgraphs()), ")")) +
            theme(legend.position= "none")
            
        # it is very important to have just p here, not print(p)!
        p 
    })    
    
    output$logitpsPlotui <- renderUI({
        if (is.null(dset.psgraphs())) return(NULL)

        plotOutput("logitpsPlot", 
            height = 300,
            brush  = if (useLogit() == TRUE) {
                brushOpts(
                    id = "logitpsPlot_brush",
                    delay = 300,
                    delayType = "debounce",
                    direction = "x",
                    resetOnNew = TRUE
                )
            } else NULL
        )
    })
    

    # modified from https://gist.github.com/wch/5436415/, with
    # help from a SO post I forgot to get the URL for
    # also from http://stackoverflow.com/questions/19130455/create-dynamic-number-of-input-elements-with-r-shiny
    observe({
        for (i in 1:numvarsToView()) {
            # My sources say:
            # Need local so that each item gets its own number. 
            # Without it, the value # of i in the renderPlot() 
            # will be the same across all instances, 
            # because of when the expression is evaluated.
            local({
                my_i <- i
                varname <- varsToView()[my_i]
                plotname <- paste0("plot", my_i)
                prunername <- paste0("pruner", my_i)
                inputname <- paste0("pruningChoices_", my_i)
                textCheckName <- paste0("textcheck", my_i)
     
                # Call renderPlot for each selected variable. 
                output[[plotname]] <- renderPlot({
                    p <- ggplot(
                        data= dset.orig()[xgraphs.ids()],
                        aes_string(
                            x      = varname,
                            fill   = groupvarFactorName(),
                            colour = groupvarFactorName()
                        )) +
                        scale_fill_manual(groupvarname(), 
                            values= colorScale.mod()) +
                        scale_colour_manual(groupvarname(), 
                            values= colorScale.mod(), 
                            guide= FALSE)
                        xlab(paste0(varname, 
                            " (n= ", length(xgraphs.ids()), ")"))
                    
                    # Histogram or bar chart
                    if (varIsContinuous()[my_i]) {    
                        p <- p + geom_histogram(
                            alpha    = alpha1, 
                            position = 'identity') 
                    } else {
                        p <- p + geom_bar(
                            alpha= alpha1, 
                            position= position_dodge()) #+
                            #scale_x_discrete(breaks= mylevels,
                            #    labels= mylevels)
                    }    
                    
                    # legend
                    if (my_i == 1) {
                        p <- p + theme(legend.position= "top")
                    } else {
                        p <- p + theme(legend.position= "none")
                    }  
                    
                    # add the rug plots
                    if (!is.null(idsForRug())) { 
                        p <- p + geom_rug(
                            data= dset.orig()[idsForRug()],  # keep aes() from above
                            sides= "b") 
                    }    
                    # just p here!  not print(p)!
                    p
                }) # end renderPlot

                # Create input function for each variable
                output[[prunername]] <- renderUI({
                    if (varIsContinuous()[my_i]) {
                        textInput(
                            inputname, 
                            paste0(varname, ": Keep only observations in this range (inclusive).",
                                " Separate the min and max by a space:"),
                            # make min & max slightly more extreme than rounded min and max in data, so that we don't get accidental pruning using the default values
                            value= paste(
                                floor(10^xdig() *   min(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]))) / 10^xdig(),
                                ceiling(10^xdig() * max(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]))) / 10^xdig(),
                                collapse= ", ")
                        )
                    } else { # we have categorical var, either factor or char
                        checkboxGroupInput(
                            inputname, 
                            paste0(varname, ': Keep only observations with the following value(s):'),
                            # as.character corrects the printing of factor levels
                            choices=  as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE])))),
                            selected= as.character(sort(unique(unlist(dset.orig()[nonMissingIDs(), eval(varname), with= FALSE]))))
                        )
                    }
                 }) # end renderUI

                # Check the textInput for each variable
                output[[textCheckName]] <- renderText({
                    if (is.null(pruneValTextList())) return(NULL)
                    if (varIsContinuous()[my_i]) {
                        if (pruneValTextList()[[my_i]] == TRUE) {
                            "Please type min and max, separated by one space."
                        } else { # no problem
                            return(NULL)
                        }
                    } else { # categorical var, nothing to check
                        return(NULL)
                    }
                 }) # end renderText
            }) # end local
        } # end for
    }) # end observe    
    
    # Now put them all together
    output$univariatePlotsAndInputs <- renderUI({
        if (is.null(varsToView())) return(NULL)
        
        plot_and_input_list <- vector("list", numvarsToView())
        for(i in 1:numvarsToView()) {
            plotname <- paste0("plot", i)
            prunername <- paste0("pruner", i)
            textcheckname <- paste0("textcheck", i)
            
            plot_and_input_list[[i]] <-
                fluidRow(
                    column(6, 
                        plotOutput(plotname, 
                            # first plot taller to accomodate legend
                            height = ifelse(i == 1, 320, 280), 
                            width  = 400#,
                        ) # end plotOutput   
                    ), # end column
                    column(6, 
                        uiOutput(prunername),
                        uiOutput(textcheckname)
                    ) # end column
                )# end fluidRow
        } 
        plot_and_input_list
    })
 
    
    ############################################################
    ############################################################
    # Session Information
    output$sessionInf <- renderPrint({
        si <- sessionInfo() 
        si$loadedOnly <- NULL
        print(si, locale=FALSE)
    })
    
    # server.R code for display
    output$serverCode <- renderPrint({
        cat(readLines(con= "server.R"), sep= "\n")
    })

    # ui.R code for display
    output$uiCode <- renderPrint({
        cat(readLines(con= "ui.R"), sep= "\n")
    })
}) # end shiny server
    
