server <- function(input, output, session){
    require(shinydashboard)
    require(AER)
    require(ggplot2)
    require(ggpubr)
    require(dplyr)
    require(tidyr)
    require(DT)
    require(tibble)
    require(lmerTest)
    require(pheatmap)
    require(gtools)
    require(cowplot)
    require(MASS)
    dft <- function(dft) {
        tdf = t(df)
        colnames(tdf) = rownames(df)
        rownames(tdf) = colnames(df)
        return(data.frame(tdf, check.names=FALSE, stringsAsFactors=FALSE))
    }

    ############################ Rendering Menu Tabs Conditionally on Data Upload and Weight Selection  ############################
    output$menuexp_summ <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("Experimental Summary", tabName = "exp_summ", icon = icon("clipboard-check"))
        }
    })
    output$menuscore_sheet <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("Score Sheet", tabName = "score_sheet", icon = icon("book"))
        }
    })
    output$menuscore_curve <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("Score Curve", tabName = "score_curve", icon = icon("chart-area"))
        }
    })
    output$menuheatmaps <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("Heatmap Clustering", tabName = "heatmaps", icon = icon("bars"))
        }
    })
    output$menuhistogram <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("High Scoring Histogram", tabName = "histogram", icon = icon("chart-bar"))
        }
    })
    output$menuanova <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("ANOVA Analysis", tabName = "anova", icon = icon("asterisk"))
        }
    })
    output$menuind_mouse <- renderMenu({
        if(fileUploadedforTabs() == TRUE || input$testdata == T){
            menuItem("Individual Mouse Curves", tabName = "ind_mouse", icon = icon("chart-area"))
        }
    })
    output$menuweight_sheet <- renderMenu({
        if(input$weightanalysis == TRUE){
            menuItem("Weight Sheet", tabName = "weight_sheet", icon = icon("book"))
        }
    })
    output$menuweight_graph <- renderMenu({
        if(input$weightanalysis == TRUE){
            menuItem("Weight Analysis", tabName = "weight_graph", icon = icon("chart-area"))
        }
    })

    ############################ Table for EAE Summary Page  ############################
    output$scoreguidelinetable <- renderTable({
        out <- read.csv("EAEScoringGuidelineTable.csv")
        out$Score <- as.character(out$Score)
        out
    })

    ############################ Output for Score Data Tab  ############################
    ## This is used for condition renders on UI side
    output$fileUploaded <- reactive({
        return(!is.null(filedata()))
    })
    ## This is the EAE Score table that is printed out to the user
    output$filedata <- DT::renderDataTable({
        infile <- input$datafile
        # if (is.null(infile) || input$testdata == F) {
        #     # User has not uploaded a file yet
        #     return(NULL)
        # }
        if(input$testdata == T){
            if(input$testdataselected == "Test Data Set 1"){
                med <- ("testdataset1.csv")
            } else if (input$testdataselected == "Test Data Set 2"){
                med <- read.csv("testdataset2.csv")
            } else if (input$testdataselected == "Test Data Set 3"){
                med <- read.csv("testdataset3.csv")
            }
        } else if (input$testdata == F){
            med <- read.csv(infile$datapath, fileEncoding="UTF=8-BOM")
        }
        colnames(med) <- gsub('X', 'Day ', colnames(med))
        med <- med[,sapply(med, function(x) { sum(!is.na(x)) > 0 })]
        ##Sub out Xs for 5s and convert to numeric except for the metadata columns
        store1 <- apply(med[,-c(1:(input$metadatacols + 1))], 2, function(x){gsub("X", 5, x)})
        store2 <- apply(store1, 2, function(x){as.numeric(x)})
        ## restitch
        med <- cbind(med[,1:(input$metadatacols + 1)], store2)

        if (input$dropzeros == T){
            #
            sickmice <- sapply(med[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            dropped <- rowSums(sickmice) < 1
            med <- add_column(med, Dropped = dropped, .after = "Mouse_ID")
            datatable(med, options = list(paging=FALSE)) %>%
                formatStyle('Dropped', target = 'row', backgroundColor = styleEqual(c(T, F), c('#A51E01', '')))
        } else {
            datatable(med, options = list(paging=FALSE))
        }
    })
    ## This is used for the conditional renders of tabs
    fileUploadedforTabs <- reactive({
        return(!is.null(filedata()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    activemice <- reactive({
        med <- filedata()
        colnames(med) <- gsub('X', 'Day ', colnames(med))
        med <- med[,sapply(med, function(x) { sum(!is.na(x)) > 0 })]
        ##Sub out Xs for 5s occured in filedata()
        if (input$dropzeros == T){
            sickmice <- sapply(med[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            dropped <- rowSums(sickmice) < 1
            med[dropped == FALSE, 1]
            #med[sickmice == TRUE, 1]
        } else {
            print(med$Mouse_ID)
        }
    })
    dropped <- reactive({
        med <- filedata()
        colnames(med) <- gsub('X', 'Day ', colnames(med))
        med <- med[,sapply(med, function(x) { sum(!is.na(x)) > 0 })]
        ##Sub out Xs for 5s occurred in filedata()
        if (input$dropzeros == T){
            sickmice <- sapply(med[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            dropped <- rowSums(sickmice) < 1
        } else {
            dropped <- rep(NA, 1)
        }
        dropped
    })
    ## Function for later use on server side
    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile) & input$testdata == F) {
            # User has not uploaded a file yet
            return(NULL)
        }
        ## Reads csv and pulls out empty columns
        if(input$testdata == T){
            if(input$testdataselected == "Test Data Set 1"){
                store <- read.csv("testdataset1.csv")
            } else if (input$testdataselected == "Test Data Set 2"){
                store <- read.csv("testdataset2.csv")
            } else if (input$testdataselected == "Test Data Set 3"){
                store <- read.csv("testdataset3.csv")
            }
        } else if (input$testdata == F){
            store <- read.csv(infile$datapath, fileEncoding="UTF=8-BOM")
        }
        store <- store[,sapply(store, function(x) { sum(!is.na(x)) > 0 })]
        ## Subs out Xs for 5s
        store1 <- apply(store[,-c(1:(input$metadatacols + 1))], 2, function(x){gsub("X", 5, x)})
        store2 <- apply(store1, 2, function(x){as.numeric(x)})
        out <- cbind(store[,1:(input$metadatacols + 1)], store2)
        as.data.frame(out)
    })
    startday <- reactive({
        data <- filedata()
        colnames(data) <- gsub('X', '', colnames(data))
        as.numeric(colnames(data)[(input$metadatacols + 2)])
    })

    ############################ Conditional UI Renders for Any Groups and Groupings ############################
    output$ScoreCurveGroup <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            ## originally combinations
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "ScoreCurveGroup", label = "What grouping do you want to use?", choices = out)
    })
    output$AuCstart <- renderUI({
        numericInput(inputId = "AuCstart", label = "Start Day of AuC Calculation", value = startday())
    })
    output$AuCend <- renderUI({
        numericInput(inputId = "AuCend", label = "End Day of AuC Calculation", value = (startday() + 4))
    })
    output$ScoreCurveGroupMax <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            ## originally combinations
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "ScoreCurveGroupMax", label = "What grouping do you want to use to view summary plots?", choices = out)
    })
    output$SummaryGroup <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            ## originally combinations
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "SummaryGroup", label = "What grouping do you want to use to view the summary?", choices = out)
    })
    output$WeightGroup <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            ## originally combinations
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "WeightGroup", label = "What grouping do you want to use?", choices = out)
    })
    output$ScoreMaxgrouping <- renderUI({
        EAEdata <- filedata()
        inputselection <- input$ScoreCurveGroupMax
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == input$ScoreCurveGroupMax]
        TGSgroupingout <- unique(EAEdata$groupselected)
        checkboxGroupInput("ScoreMaxgrouping", "Choose which groups to display", TGSgroupingout, selected = TGSgroupingout)
    })
    output$ScoreCurvegrouping <- renderUI({
        EAEdata <- filedata()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == input$ScoreCurveGroup]
        TGSgroupingout <- unique(EAEdata$groupselected)
        checkboxGroupInput("ScoreCurvegrouping", "Choose which groups to display", TGSgroupingout, selected = TGSgroupingout)
    })
    output$ScoreCurvegrouping1 <- renderUI({
        EAEdata <- filedata()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == input$ScoreCurveGroup]
        TGSgroupingout <- unique(EAEdata$groupselected)
        checkboxGroupInput("ScoreCurvegrouping1", "Choose which groups to display", TGSgroupingout, selected = TGSgroupingout)
    })
    output$Weightgrouping <- renderUI({
        EAEdata <- filedata()
        inputselection <- input$WeightGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == input$WeightGroup]
        TGSgroupingout <- unique(EAEdata$groupselected)
        checkboxGroupInput("Weightgrouping", "Choose which groups to display", TGSgroupingout, selected = TGSgroupingout)
    })

    ############################ Color, Shape, and More Selection ############################
    output$length <- renderText({
        as.numeric(length(input$ScoreCurvegrouping))
    })

    output$colors <- renderUI({
        lapply(1:(length(input$ScoreCurvegrouping)), function(i) {
            groups <- input$ScoreCurvegrouping
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('colors', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$ScoreCurveerrorbars <- renderUI({
        lapply(1:(length(input$ScoreCurvegrouping)), function(i) {
            groups <- input$ScoreCurvegrouping
            selectInput((paste0('ScoreCurveerrorbars', i)), label = (paste0('What error bars for ',
                                                                            groups[[i]], "?")), choices = c("Both", "Upper", "Lower"), selected = "Both")
        })
    })

    output$colors1 <- renderUI({
        lapply(1:(length(input$ScoreCurvegrouping1)), function(i) {
            groups <- input$ScoreCurvegrouping1
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('colors', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$ScoreCurveGroupMaxcolors <- renderUI({
        lapply(1:(length(input$ScoreMaxgrouping)), function(i) {
            groups <- input$ScoreMaxgrouping
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('ScoreCurveGroupMaxcolors', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$colorsweight <- renderUI({
        lapply(1:(length(input$Weightgrouping)), function(i) {
            groups <- input$Weightgrouping
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('colorsweight', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$shapes <- renderUI({
        lapply(1:(length(input$ScoreCurvegrouping)), function(i) {
            groups <- input$ScoreCurvegrouping
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            selectInput((paste0('shapes', i)), label = (paste0('What shape for ',  groups[[i]], "?")),
                        choices = c(
                            "open square" = 0,
                            "open circle" = 1,
                            "open triangle" = 2,
                            "plus" = 3,
                            "cross" = 4,
                            "open diamond" = 5,
                            "open triangle down" = 6,
                            "square cross" = 7,
                            "asterisk" = 8,
                            "diamond plus" = 9,
                            "circle plus" = 10,
                            "star" = 11,
                            "square plus" = 12,
                            "circle cross" = 13,
                            "square triangle" = 14,
                            "square" = 15,
                            "small circle" =16,
                            "triangle" = 17,
                            "diamond" = 18,
                            "circle" = 19,
                            "bullet" = 20
                        ),
                        selected = 19)
        })
    })

    output$shapesweight <- renderUI({
        lapply(1:(length(input$Weightgrouping)), function(i) {
            groups <- input$Weightgrouping
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            selectInput((paste0('shapesweight', i)), label = (paste0('What shape for ',  groups[[i]], "?")),
                        choices = c(
                            "open square" = 0,
                            "open circle" = 1,
                            "open triangle" = 2,
                            "plus" = 3,
                            "cross" = 4,
                            "open diamond" = 5,
                            "open triangle down" = 6,
                            "square cross" = 7,
                            "asterisk" = 8,
                            "diamond plus" = 9,
                            "circle plus" = 10,
                            "star" = 11,
                            "square plus" = 12,
                            "circle cross" = 13,
                            "square triangle" = 14,
                            "square" = 15,
                            "small circle" =16,
                            "triangle" = 17,
                            "diamond" = 18,
                            "circle" = 19,
                            "bullet" = 20
                        ),
                        selected = 19)
        })
    })

    output$newnames <- renderUI({
        lapply(1:(length(input$ScoreCurvegrouping)), function(i) {
            groups <- input$ScoreCurvegrouping
            textInput((paste0('newnames', i)), label = (paste0('Relabel ',  groups[[i]], " as?")), value = groups[[i]])
        })
    })

    ############################ Experimental Summary Page ############################
    output$dropped <-renderText({
        EAEdata <- filedata()
        micetotal <- nrow(EAEdata)
        colnames(EAEdata) <- gsub('X', '', colnames(EAEdata))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdata[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            dropped <- EAEdata[rowSums(sickmice) < 1 , 1]
            if (length(dropped) < 1){
                dropped = "No mice were dropped from the study, every mouse met the set criteria to be included. Giving a total percentage sick of 100%."
            } else if (length(dropped) == 1) {
                dropped <- paste0("There was ", length(dropped)," mouse dropped from the study. This mouse had the ID ", paste0(dropped, collapse = ","), ". Giving a total percentage sick of ", paste0( round((((micetotal - length(dropped)) / micetotal)*100), digits =3), collapse = ""), "%.", collapse = "")
            } else {
                dropped <- paste0("There were ", length(dropped)," mice dropped from the study. These mice had IDs ", paste0(dropped, collapse = ","), ". Giving a total percentage sick of ", paste0( round((((micetotal - length(dropped)) / micetotal)*100), digits =3), collapse = ""), "%.", collapse = "")
            }
        } else {
            dropped = "No mice were dropped from the study, please select the drop zero function if you would like this feature."
        }
        dropped
    })

    MaxScoregraph <- reactive({
        EAEdata <- filedata()
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdata[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdata <- EAEdata[rowSums(sickmice) > 0 , ]
        }

        EAEdata$MaxScore <- apply(EAEdata[,(input$metadatacols + 2):ncol(EAEdata)], 1, max)
        EAEdata <- as.data.frame(EAEdata)
        inputselection <- input$ScoreCurveGroupMax
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        colnames(EAEdata) <- gsub(input$ScoreCurveGroupMax, "variable", colnames(EAEdata))

        groups <- input$ScoreMaxgrouping
        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreMaxgrouping)){
          colorset1[i] <- input[[(paste0('ScoreCurveGroupMaxcolors', i))]]
        }

        ggout <- ggboxplot(EAEdata, x = "variable", y = "MaxScore",
                           color = "variable",
                           add = "dotplot")
        ggout <- ggout + ylab("Max Score Reached\n by Each Mouse") + xlab(input$ScoreCurveGroupMax)
        ggout <- ggout + scale_colour_manual(values = colorset1) + scale_fill_manual(values = colorset1)
        ggout <- ggout + theme(
            axis.text.x.bottom = element_text(size=0),
            # axis.text.y = element_text(size=input$AuCyaxistext),
            # text = element_text(size=input$AuCLabels),
            legend.title = element_blank(),
            legend.position = "none")
        ggout
    })

    DaysToStartgraph <- reactive({
        DaysAligned <- DaysAligned()
        inputselection <- input$ScoreCurveGroupMax
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            DaysAligned$combinedgrouping <- apply(DaysAligned[,(colnames(DaysAligned) %in% groups)], 1, paste0, collapse = " " )
            colnames(DaysAligned) <- gsub("combinedgrouping", inputselection, colnames(DaysAligned))
        }
        colnames(DaysAligned) <- gsub(input$ScoreCurveGroupMax, "variable", colnames(DaysAligned))

        groups <- input$ScoreMaxgrouping
        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreMaxgrouping)){
            colorset1[i] <- input[[(paste0('ScoreCurveGroupMaxcolors', i))]]
        }

        ggout <- ggboxplot(DaysAligned, x = "variable", y = "start",
                           color = "variable",
                           add = "dotplot")
        ggout <- ggout + scale_colour_manual(values = colorset1) + scale_fill_manual(values = colorset1)
        ggout <- ggout + ylab("Days to First Symptoms") + xlab(input$ScoreCurveGroupMax)
        ggout <- ggout + theme(
            axis.text.x.bottom = element_text(size=0),
            # axis.text.y = element_text(size=input$AuCyaxistext),
            # text = element_text(size=input$AuCLabels),
            legend.title = element_blank(),
            legend.position = "none")
        ggout
    })

    PeakScoreDaygraph <- reactive({
        PeakScoreDay <- PeakScoreDay()
        inputselection <- input$ScoreCurveGroupMax
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            PeakScoreDay$combinedgrouping <- apply(PeakScoreDay[,(colnames(PeakScoreDay) %in% groups)], 1, paste0, collapse = " " )
            colnames(PeakScoreDay) <- gsub("combinedgrouping", inputselection, colnames(PeakScoreDay))
        }
        colnames(PeakScoreDay) <- gsub(input$ScoreCurveGroupMax, "variable", colnames(PeakScoreDay))

        groups <- input$ScoreMaxgrouping
        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreMaxgrouping)){
            colorset1[i] <- input[[(paste0('ScoreCurveGroupMaxcolors', i))]]
        }

        ggout <- ggboxplot(PeakScoreDay, x = "variable", y = "Day",
                           color = "variable",
                           add = "dotplot")
        ggout <- ggout + scale_colour_manual(values = colorset1) + scale_fill_manual(values = colorset1)
        ggout <- ggout + ylab("Days to First\n Occurance of Peak Score") + xlab(input$ScoreCurveGroupMax)
        ggout <- ggout + theme(
            axis.text.x.bottom = element_text(size=0),
            # axis.text.y = element_text(size=input$AuCyaxistext),
            # text = element_text(size=input$AuCLabels),
            legend.title = element_blank(),
            legend.position = "none")
        ggout
    })

    output$MaxScore <- renderPlot({
        a <- MaxScoregraph()
        a
    })

    output$DaysToStart <- renderPlot({
        DaysToStartgraph()
    })

    output$PeakScoreDay <- renderPlot({
        PeakScoreDaygraph()
    })

    combinedGraph <- reactive({
        ## Junk to create a proper legend for bottom right
        PeakScoreDay <- PeakScoreDay()
        inputselection <- input$ScoreCurveGroupMax
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            PeakScoreDay$combinedgrouping <- apply(PeakScoreDay[,(colnames(PeakScoreDay) %in% groups)], 1, paste0, collapse = " " )
            colnames(PeakScoreDay) <- gsub("combinedgrouping", inputselection, colnames(PeakScoreDay))
        }
        colnames(PeakScoreDay) <- gsub(input$ScoreCurveGroupMax, "variable", colnames(PeakScoreDay))
        groups <- input$ScoreMaxgrouping
        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreMaxgrouping)){
            colorset1[i] <- input[[(paste0('ScoreCurveGroupMaxcolors', i))]]
        }

        ggout <- ggboxplot(PeakScoreDay, x = "variable", y = "Day",
                           color = "variable",
                           shape = "variable")
        ggout <- ggout + ylab("Days to First\n Occurance of Peak Score") + xlab(input$ScoreCurveGroupMax)
        ggout <- ggout + scale_colour_manual(values = colorset1) + scale_fill_manual(values = colorset1)
        ggout <- ggout + theme(
            axis.text.x.bottom = element_text(size=0),
            legend.title = element_blank(),
            legend.position = "right",
            legend.text=element_text(size=input$MaxLegendSize))
        ggout
        ## End of Legend Junk

        a <- MaxScoregraph()
        b <- DaysToStartgraph()
        c <- PeakScoreDaygraph()

        legend <- cowplot::get_legend(ggout)

        plot_grid(a, b, c, legend,
                  labels = c("A", "B", "C"),
                  ncol = 2, nrow = 2)
    })

    output$combinedGraph <- renderPlot({
        combinedGraph()
    })

    output$downloadgraphCombo <- downloadHandler(
        filename = function() { paste(input$title, 'Combograph.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = combinedGraph(), device = input$ScoreCurveImageType, width = input$ComboWidth, height = input$ComboHeight, dpi = 300, units = input$ComboUnits)
        }
    )

    output$sickfreq <-renderTable({
        EAEdata <- filedata()
        micetotal <- nrow(EAEdata)
        colnames(EAEdata) <- gsub('X', '', colnames(EAEdata))

        inputselection <- input$SummaryGroup

        if (input$dropzeros == T){
            sickmice <- sapply(EAEdata[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            dropped <- EAEdata[rowSums(sickmice) < 1 , 1:(input$metadatacols + 1)]
            droppedlength <- nrow(dropped)
        } else {
            droppedlength <- 0
        }
        if (droppedlength > 0){
            if(grepl(":", inputselection, fixed = TRUE) == TRUE){
                groups <- unlist(strsplit(inputselection, ":"))
                EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
                colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
                dropped$combinedgrouping <- apply(dropped[,(colnames(dropped) %in% groups)], 1, paste0, collapse = " " )
                colnames(dropped) <- gsub("combinedgrouping", inputselection, colnames(dropped))
            }
            EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == inputselection]
            dropped$groupselected <- dropped[,colnames(dropped) == inputselection]
            #dropped$TGS <- apply(dropped[ ,2:(input$metadatacols + 1)] , 1 , paste , collapse = " " )
            #EAEdata$TGS <- apply(EAEdata[ ,2:(input$metadatacols + 1)] , 1 , paste , collapse = " " )
            droppedmice <- as.data.frame(table(dropped$groupselected))
            original <- as.data.frame(table(EAEdata$groupselected))
            colnames(original)[1] <- "All Grouping Variables"
            colnames(droppedmice)[1] <- "All Grouping Variables"
            out <- merge(droppedmice, original, by= "All Grouping Variables", all = T)
            colnames(out)[2] <- "Mice Dropped"
            colnames(out)[3] <- "Total Mice"
            out[is.na(out)] <- 0
            out$`Percentage Sick` <- (((as.numeric(out$`Total Mice`)) - as.numeric(out$`Mice Dropped`)) / ((as.numeric(out$`Total Mice`)))) *100
            ### This is an awful solution, but it works (note for some reason the out[is.na(out)] line above prevents round() from working)
            out$`Mice Dropped` <- as.factor(out$`Mice Dropped`)
            #out$new <- signif(out$`Mice Dropped`, digits =0)
            out
            #str(out)
        } else {
            if(grepl(":", inputselection, fixed = TRUE) == TRUE){
                groups <- unlist(strsplit(inputselection, ":"))
                EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
                colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
            }
            EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == inputselection]
            out <- as.data.frame(table(EAEdata$groupselected))
            colnames(out)[1] <- "All Grouping Variables"
            colnames(out)[2] <- "Total Mice"
            out
        }
    }, align = "c")

    ############################ EAE centralized long data ############################

    EAElong <- reactive({
        EAEdata <- filedata()
        #EAEdata <- add_rownames(EAEdata)
        colnames(EAEdata) <- gsub('X', '', colnames(EAEdata))

        if (input$dropzeros == T){
            sickmice <- sapply(EAEdata[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdata <- EAEdata[rowSums(sickmice) > 0 , ]
        }

        # EAEdata$TGS <- paste(EAEdata$Treatment, EAEdata$Sex)
        EAElong <- EAEdata %>% gather('Day', 'Score', -c(1:(input$metadatacols + 1)))
        EAElong$Day <- as.numeric(EAElong$Day)
        EAElong$Score <- as.numeric(EAElong$Score)
        ((EAElong))
    })

    ##################################################### EAE Score Curve by Selection ##################################################
    ##################### Stats #####
    resTselected <- reactive({
        data <- EAElong()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            data$combinedgrouping <- apply(data[,(colnames(data) %in% groups)], 1, paste0, collapse = " " )
            colnames(data) <- gsub("combinedgrouping", inputselection, colnames(data))
        }
        data$groupselected <- data[,colnames(data) == input$ScoreCurveGroup]
        data <- data[data$groupselected %in% input$ScoreCurvegrouping,]
        if(length(input$ScoreCurvegrouping) < 2){

        } else if(length(input$ScoreCurvegrouping) == 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(Score ~ groupselected, data, method = 'wilcox.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(Score ~ groupselected, data, method = 't.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resTselect)
        } else if(length(input$ScoreCurvegrouping) > 2){
            if (input$stats == "Non-parametric"){
                resB <- compare_means(Score ~ groupselected, data, method = 'kruskal.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resB <- compare_means(Score ~ groupselected, data, method = 'anova', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resB)
        }
    })
    ##################### Sigs #####
    output$SignificanceTGSselected <- renderTable({
        resTselected()
    })
    ##################### Graph #####
    output$testinga <- renderTable({
        EAElongrm1 <- as.data.frame(EAElongAlign())

        EAEdatap <- as.data.frame(filedata())
        colnames(EAEdatap) <- gsub('X', '', colnames(EAEdatap))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatap[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatap <- EAEdatap[rowSums(sickmice) > 0 , ]
        }
        #EAEdatap$TGS <- paste(EAEdatap$Treatment, EAEdatap$Sex)
        rownames(EAEdatap) <- EAEdatap$Mouse_ID

        ############ Graphing
        AlignedEAElongtest1 = EAElongrm1[ , c('Day (offset)', 'Mouse_ID', 'Score')] %>%
            pivot_wider(names_from = c("Mouse_ID"),values_from = Score) %>%
            as.data.frame()
        AlignedEAElongtest1 <- AlignedEAElongtest1[order(AlignedEAElongtest1$`Day (offset)`),]
        AlignedEAElongtest2 <- AlignedEAElongtest1[,-1]
        rownames(AlignedEAElongtest2) <- AlignedEAElongtest1[,1]

        t(as.data.frame(AlignedEAElongtest2))
    })

    GraphScoreCurveselected <- reactive({
        resA <- resTselected()
        EAElong <- EAElong()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAElong$combinedgrouping <- apply(EAElong[,(colnames(EAElong) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAElong) <- gsub("combinedgrouping", inputselection, colnames(EAElong))
        }
        EAElong$groupselected <- EAElong[,colnames(EAElong) == input$ScoreCurveGroup]
        EAElong <- EAElong[EAElong$groupselected %in% input$ScoreCurvegrouping,]

        if(input$errorbars == "Standard Error of the Mean"){
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+se)
        } else if (input$errorbars == "Standard Deviation") {
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+sd)
        } else {
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+sd)
        }

        if(length(input$ScoreCurvegrouping) > 1){
            MaxScores <- aggregate(Score ~ Day, data = Mean, max)
            resA <- merge(resA, MaxScores, by=c('Day', 'Day'))
            colnames(resA)[colnames(resA)=="Score"] <- "max"
            resA <- resA[resA$p.adj != 'NaN', ]
            if(input$ScorePLabels == "p"){
                resA$p.adj.star1 <- ifelse(resA$p < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$ScoreStar1
                resA$p.adj.star2 <- ifelse(resA$p < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$ScoreStar2
                resA$p.adj.star3 <- ifelse(resA$p < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$ScoreStar3
            } else {
                resA$p.adj.star1 <- ifelse(resA$p.adj < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$ScoreStar1
                resA$p.adj.star2 <- ifelse(resA$p.adj < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$ScoreStar2
                resA$p.adj.star3 <- ifelse(resA$p.adj < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$ScoreStar3
            }

            resA$Day <- as.numeric(resA$Day)}

        EAElong$Day <- as.numeric(EAElong$Day)
        if(input$errorbars == "Standard Error of the Mean"){
            ggob = ggline(EAElong,
                          y = "Score",
                          x = "Day", group = "groupselected", add = "mean_se", width = 5,
                          color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected", point.size = input$ScoreCurvePointSize)
        } else if (input$errorbars == "Standard Deviation"){
            ggob = ggline(EAElong,
                          y = "Score",
                          x = "Day", group = "groupselected", add = "mean_sd", width = 5,
                          color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
                          point.size = input$ScoreCurvePointSize)
        } else if (input$errorbars == "Mean Confidence Interval"){
            # ggob = ggline(EAElong,
            #               y = "Score",
            #               x = "Day", group = "groupselected", add = "mean_sd", width = 5,
            #               color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
            #               point.size = input$ScoreCurvePointSize, select = "Male", error.plot = "upper_errorbar")
            # ggob = ggob + ggline(EAElong,
            #                      y = "Score",
            #                      x = "Day", group = "groupselected", add = "mean_sd", ci = 0.95, width = 5,
            #                      color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
            #                      point.size = input$ScoreCurvePointSize, select = "Female", error.plot = "lower_errorbar")
            ggob = ggplot(EAElong, aes(y = Score, x = Day, group = groupselected, color = groupselected))
            ggob = ggob + geom_line(stat = "mean") + geom_point()
        }
        groups <- input$ScoreCurvegrouping

        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreCurvegrouping)){
            colorset1[i] <- input[[(paste0('colors', i))]]
        }

        shapeset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreCurvegrouping)){
            shapeset1[i] <- as.numeric(input[[(paste0('shapes', i))]])
        }
        if(input$showrelabelettings == TRUE){
            renameset1 <- rep(NA, 1)
            for(i in 1:length(input$ScoreCurvegrouping)){
                renameset1[i] <- (input[[(paste0('newnames', i))]])
            }
            ggob = ggob + scale_shape_manual(values = shapeset1, labels = (unlist(renameset1)))
            ggob = ggob + scale_colour_manual(values = colorset1, labels = (unlist(renameset1)))
        } else {
            ggob = ggob + scale_shape_manual(values = shapeset1)
            ggob = ggob + scale_colour_manual(values = colorset1)
        }
        ggob = ggob + ylab(input$ScoreCurveylab) + xlab(input$ScoreCurvexlab)
        ggob = ggob + ggtitle(input$title)
        if(length(input$ScoreCurvegrouping) > 1){
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star1.height, label=resA$p.adj.star1, size=input$ScoreStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star2.height, label=resA$p.adj.star2, size=input$ScoreStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star3.height, label=resA$p.adj.star3, size=input$ScoreStarSize)
        }
        ggob = ggob + theme(
            axis.text.x.bottom = element_text(size=input$ScoreCurvexaxistext),
            text = element_text(size=input$ScoreCurveLabels),
            legend.title = element_blank(),
            legend.text=element_text(size=input$ScoreCurveLegendText)
        )
        if(input$AuCGo == TRUE){
            ggob = ggob + geom_vline(xintercept = (as.numeric(input$AuCstart) - as.numeric(startday()) +1), color = "green")
            ggob = ggob + geom_vline(xintercept = (as.numeric(input$AuCend) - as.numeric(startday()) +1), color = "red")
        }
        ggob
    } )

    output$GraphScoreCurveselected <- renderPlot({
        resA <- resTselected()
        EAElong <- EAElong()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAElong$combinedgrouping <- apply(EAElong[,(colnames(EAElong) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAElong) <- gsub("combinedgrouping", inputselection, colnames(EAElong))
        }
        EAElong$groupselected <- EAElong[,colnames(EAElong) == input$ScoreCurveGroup]
        EAElong <- EAElong[EAElong$groupselected %in% input$ScoreCurvegrouping,]

        if(input$errorbars == "Standard Error of the Mean"){
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+se)
        } else if (input$errorbars == "Standard Deviation") {
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+sd)
        } else {
            Mean <- EAElong %>%
                group_by(Day, groupselected) %>%
                summarize(avg = mean(Score), n = n(),
                          sd = sd(Score), se = sd/sqrt(n), Score = avg+sd)
        }

        if(length(input$ScoreCurvegrouping) > 1){
            MaxScores <- aggregate(Score ~ Day, data = Mean, max)
            resA <- merge(resA, MaxScores, by=c('Day', 'Day'))
            colnames(resA)[colnames(resA)=="Score"] <- "max"
            resA <- resA[resA$p.adj != 'NaN', ]
            if(input$ScorePLabels == "p"){
                resA$p.adj.star1 <- ifelse(resA$p < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$ScoreStar1
                resA$p.adj.star2 <- ifelse(resA$p < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$ScoreStar2
                resA$p.adj.star3 <- ifelse(resA$p < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$ScoreStar3
            } else {
                resA$p.adj.star1 <- ifelse(resA$p.adj < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$ScoreStar1
                resA$p.adj.star2 <- ifelse(resA$p.adj < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$ScoreStar2
                resA$p.adj.star3 <- ifelse(resA$p.adj < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$ScoreStar3
            }

            resA$Day <- as.numeric(resA$Day)}

        EAElong$Day <- as.numeric(EAElong$Day)
        if(input$errorbars == "Standard Error of the Mean"){
            ggob = ggline(EAElong,
                          y = "Score",
                          x = "Day", group = "groupselected", add = "mean_se", width = 5,
                          color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected", point.size = input$ScoreCurvePointSize)
        } else if (input$errorbars == "Standard Deviation"){
            ggob = ggline(EAElong,
                          y = "Score",
                          x = "Day", group = "groupselected", add = "mean_sd", width = 5,
                          color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
                          point.size = input$ScoreCurvePointSize)
        } else if (input$errorbars == "Mean Confidence Interval"){
            # ggob = ggline(EAElong,
            #               y = "Score",
            #               x = "Day", group = "groupselected", add = "mean_sd", width = 5,
            #               color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
            #               point.size = input$ScoreCurvePointSize, select = "Male", error.plot = "upper_errorbar")
            # ggob = ggob + ggline(EAElong,
            #                      y = "Score",
            #                      x = "Day", group = "groupselected", add = "mean_sd", ci = 0.95, width = 5,
            #                      color = "groupselected", size = input$ScoreCurveLineThickness, shape = "groupselected",
            #                      point.size = input$ScoreCurvePointSize, select = "Female", error.plot = "lower_errorbar")
            ggob = ggplot(EAElong, aes(y = Score, x = Day, group = groupselected, color = groupselected))
            ggob = ggob + geom_line(stat = "mean") + geom_point()
        }
        groups <- input$ScoreCurvegrouping

        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreCurvegrouping)){
            colorset1[i] <- input[[(paste0('colors', i))]]
        }

        shapeset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreCurvegrouping)){
            shapeset1[i] <- as.numeric(input[[(paste0('shapes', i))]])
        }
        if(input$showrelabelettings == TRUE){
            renameset1 <- rep(NA, 1)
            for(i in 1:length(input$ScoreCurvegrouping)){
                renameset1[i] <- (input[[(paste0('newnames', i))]])
            }
            ggob = ggob + scale_shape_manual(values = shapeset1, labels = (unlist(renameset1)))
            ggob = ggob + scale_colour_manual(values = colorset1, labels = (unlist(renameset1)))
        } else {
            ggob = ggob + scale_shape_manual(values = shapeset1)
            ggob = ggob + scale_colour_manual(values = colorset1)
        }
        ggob = ggob + ylab(input$ScoreCurveylab) + xlab(input$ScoreCurvexlab)
        ggob = ggob + ggtitle(input$title)
        if(length(input$ScoreCurvegrouping) > 1){
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star1.height, label=resA$p.adj.star1, size=input$ScoreStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star2.height, label=resA$p.adj.star2, size=input$ScoreStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(EAElong$Day) + 1, y=resA$p.adj.star3.height, label=resA$p.adj.star3, size=input$ScoreStarSize)
        }
        ggob = ggob + theme(
            axis.text.x.bottom = element_text(size=input$ScoreCurvexaxistext),
            text = element_text(size=input$ScoreCurveLabels),
            legend.title = element_blank(),
            legend.text=element_text(size=input$ScoreCurveLegendText)
        )
        if(input$AuCGo == TRUE){
            ggob = ggob + geom_vline(xintercept = (as.numeric(input$AuCstart) - as.numeric(startday()) +1), color = "green")
            ggob = ggob + geom_vline(xintercept = (as.numeric(input$AuCend) - as.numeric(startday()) +1), color = "red")
        }
        ggob
    } )

    output$downloadstatsScoreCurve <- downloadHandler(
        filename = function() { paste(input$title, 'ScoreCurve-comparisons.',"csv", sep='') },
        content = function(file) {
            write.csv(resTselected(), file)
        }
    )

    output$downloadgraphScoreCurve <- downloadHandler(
        filename = function() { paste(input$title, 'ScoreCurve.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = GraphScoreCurveselected(), device = input$ScoreCurveImageType, width = input$ScoreCurveWidth, height = input$ScoreCurveHeight, dpi = 300, units = input$ScoreCurveUnits)
        }
    )


    ############################# Area under the Curve Data ########################

    AuC <- reactive({
        EAEdatapure <- filedata()
        colnames(EAEdatapure) <- gsub('X', 'Day ', colnames(EAEdatapure))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatapure[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatapure <- EAEdatapure[rowSums(sickmice) > 0 , ]
        }
        EAEdatapure <- EAEdatapure[,-c(1:(input$metadatacols + 1))]
        ## Framing the proper sized data set, minus one column (aka this will be entirely overwritten)
        out <- EAEdatapure[,-ncol(EAEdatapure)]
        for(k in 1:(ncol(EAEdatapure)-1)){
            kplus <- k + 1
            for(l in 1:nrow(EAEdatapure)){
                out[l,k] <- mean(c(as.numeric(EAEdatapure[l,k]),as.numeric(EAEdatapure[l,kplus]) ))
            }
        }
        out
    })

    resAuCselected <- reactive({
        data <- EAElong()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            data$combinedgrouping <- apply(data[,(colnames(data) %in% groups)], 1, paste0, collapse = " " )
            colnames(data) <- gsub("combinedgrouping", inputselection, colnames(data))
        }
        data$groupselected <- data[,colnames(data) == input$ScoreCurveGroup]

        if(length(input$ScoreCurvegrouping1) < 2){

        } else if(length(input$ScoreCurvegrouping1) == 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(Score ~ groupselected, data, method = 'wilcox.test', group.by = c("Day"), paired = F, p.adjust.method = "fdr")
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(Score ~ groupselected, data, method = 't.test', group.by = c("Day"), paired = F, p.adjust.method = "fdr")
            }
            as.data.frame(resTselect)
        } else if(length(input$ScoreCurvegrouping1) > 2){
            if (input$stats == "Non-parametric"){
                resB <- compare_means(Score ~ groupselected, data, method = 'kruskal.test', group.by = c("Day"), paired = F, p.adjust.method = "fdr")
            } else if (input$stats == 'Parametric'){
                resB <- compare_means(Score ~ groupselected, data, method = 'anova', group.by = c("Day"), paired = F, p.adjust.method = "fdr")
            }
            as.data.frame(resB)
        }
    })

    AuCrowedMeta <- reactive({
        AuCdata <- AuC()
        start <- as.numeric(input$AuCstart) - as.numeric(startday()) + 1
        end <- as.numeric(input$AuCend) - as.numeric(startday())
        if(end == 1){
            AuCdata <- AuCdata[,(start)]
        } else {
            AuCdata <- AuCdata[,(start:end)]
        }

        file <- filedata()
        if (input$dropzeros == T){
            sickmice <- sapply(file[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            file <- file[rowSums(sickmice) > 0 , ]
        }

        metadata <- file[,1:(input$metadatacols + 1)]
        #metadata$TGS <- paste(metadata$Treatment, metadata$Sex)
        if(start != end) {
            AuCv <- rowSums(AuCdata)
        } else {
            AuCv <- AuCdata
        }
        middle <- cbind(metadata,AuCv)
        middle
    })

    output$AuCstattable <- renderTable({
        middle <- AuCrowedMeta()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            middle$Groups <- apply(middle[,(colnames(middle) %in% groups)], 1, paste0, collapse = " " )
            #colnames(middle) <- gsub("combinedgrouping", "Groups", colnames(middle))
        } else {
            middle$Groups <- middle[,colnames(middle) == input$ScoreCurveGroup]
        }
        tableout <- as.data.frame(as.matrix(aggregate(. ~ Groups, middle[,c((ncol(middle)-1), ncol(middle))], function(x) c(mean = mean(x), sd = sd(x)))))
        # if(grepl(":", inputselection, fixed = TRUE) == TRUE){
        #     tableout <- aggregate(. ~ Groups, middle, function(x) c(mean = mean(x), sd = sd(x)))
        # } else {
        #     tableout <- aggregate(. ~ Groups, middle, function(x) c(mean = mean(x), sd = sd(x)))
        # }
        colnames(tableout)[2] <- "Mean of AuC"
        colnames(tableout)[3] <- "Standard Deviation of AuC"
        tableout

    })

    output$resAuCstats <- renderTable({
        data <- AuCrowedMeta()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            data$combinedgrouping <- apply(data[,(colnames(data) %in% groups)], 1, paste0, collapse = " " )
            colnames(data) <- gsub("combinedgrouping", inputselection, colnames(data))
        }
        data$groupselected <- data[,colnames(data) == input$ScoreCurveGroup]
        data <- data[data$groupselected %in% input$ScoreCurvegrouping,]
        if(length(unique(data$groupselected)) < 2){

        } else if(length(unique(data$groupselected)) == 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 'wilcox.test', paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 't.test', paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resTselect)
        } else if(length(unique(data$groupselected)) > 2){
            if (input$stats == "Non-parametric"){
                resB <- compare_means(AuCv ~ groupselected, data, method = 'kruskal.test', paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resB <- compare_means(AuCv ~ groupselected, data, method = 'anova', paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resB)
        }

    })

    resAuCstatsreactive <- reactive({
        data <- AuCrowedMeta()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            data$combinedgrouping <- apply(data[,(colnames(data) %in% groups)], 1, paste0, collapse = " " )
            colnames(data) <- gsub("combinedgrouping", inputselection, colnames(data))
        }
        data$groupselected <- data[,colnames(data) == input$ScoreCurveGroup]
        data <- data[data$groupselected %in% input$ScoreCurvegrouping,]
        if(length(unique(data$groupselected)) < 2){

        } else if(length(unique(data$groupselected)) == 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 'wilcox.test', paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 't.test', paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resTselect)
        } else if(length(unique(data$groupselected)) > 2){
            if (input$stats == "Non-parametric"){
                resB <- compare_means(AuCv ~ groupselected, data, method = 'kruskal.test', paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resB <- compare_means(AuCv ~ groupselected, data, method = 'anova', paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resB)
        }

    })

    resAuCstatsbreakdown <- reactive({
        data <- AuCrowedMeta()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            data$combinedgrouping <- apply(data[,(colnames(data) %in% groups)], 1, paste0, collapse = " " )
            colnames(data) <- gsub("combinedgrouping", inputselection, colnames(data))
        }
        data$groupselected <- data[,colnames(data) == input$ScoreCurveGroup]
        data <- data[data$groupselected %in% input$ScoreCurvegrouping,]

        if(length(unique(data$groupselected)) >= 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 'wilcox.test', paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(AuCv ~ groupselected, data, method = 't.test', paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resTselect)
        }

    })

    output$resAuCstatsbreakdown <- renderTable((
        resAuCstatsbreakdown
    ))

    output$AuCErrorPlot <- renderPlot({
        AuCErrorPlot()
    })

    AuCErrorPlot <- reactive({
        middle <- AuCrowedMeta()
        inputselection <- input$ScoreCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            middle$combinedgrouping <- apply(middle[,(colnames(middle) %in% groups)], 1, paste0, collapse = " " )
            colnames(middle) <- gsub("combinedgrouping", inputselection, colnames(middle))
        }
        middle$sorting <- middle[,colnames(middle) == input$ScoreCurveGroup]

        data <- middle
        groups <- input$ScoreCurvegrouping
        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ScoreCurvegrouping)){
            colorset1[i] <- input[[(paste0('colors', i))]]
        }
        # ggout <- ggplot(data, aes(x = sorting, y = AuCv, ymin = AuCv-sd, ymax = AuCv+sd, color = sorting))
        ggout <- ggboxplot(data, x = "sorting", y = "AuCv",
                           color = "sorting",
                           add = "jitter", shape = "sorting", size = input$AuCLineSize,
                           add.params = list(size = input$AuCPointSize, jitter = input$jittersize),
                           select = input$ScoreCurvegrouping)

        ggout <- ggout + scale_colour_manual(values = colorset1)
        ggout <- ggout + ggtitle(input$title)
        #ggout <- ggout + stat_compare_means() ## Come back and add stat bars for this plot
        ggout <- ggout + theme_bw() + xlab(input$AuCErrorPlotxlab) + ylab(input$AuCErrorPlotylab)
        #-------------------------------------------------------------------------------------------------
        ggout <- ggout + theme(
            axis.text.x.bottom = element_text(size=input$AuCxaxistext),
            axis.text.y = element_text(size=input$AuCyaxistext),
            text = element_text(size=input$AuCLabels),
            legend.title = element_blank(),
            legend.text=element_text(size=input$AuCLegendSize))
        ggout
    })

    output$downloadgraphAuC <- downloadHandler(
        filename = function() { paste(input$title, 'AuC_ScoreCurve.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = AuCErrorPlot(), device = input$ScoreCurveImageType, width = input$AuCWidth, height = input$AuCHeight, dpi = 300, units = input$AuCUnits)
        }
    )

    output$downloadstatsAuC <- downloadHandler(
        filename = function() { paste(input$title, 'AuC_ScoreCurve-comparisons.',"csv", sep='') },
        content = function(file) {
            write.csv(resAuCstatsbreakdown(), file)
        }
    )

    ############################ Individual Mouse Curves / Spaghetti Plots / Shadow Curves ############################

    output$ShadowCurveGroup <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        for(i in 1:(ncol(metadata))){
            ## originally combinations
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "ShadowCurveGroup", label = "What grouping do you want to use?", choices = out)
    })

    output$ShadowCurvegrouping <- renderUI({
        EAEdata <- filedata()
        inputselection <- input$ShadowCurveGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            EAEdata$combinedgrouping <- apply(EAEdata[,(colnames(EAEdata) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAEdata) <- gsub("combinedgrouping", inputselection, colnames(EAEdata))
        }
        EAEdata$groupselected <- EAEdata[,colnames(EAEdata) == input$ShadowCurveGroup]
        TGSgroupingout <- unique(EAEdata$groupselected)
        checkboxGroupInput("ShadowCurvegrouping", "Choose which groups to display", TGSgroupingout, selected = TGSgroupingout)
    })

    output$ShadowCurvecolors <- renderUI({
        lapply(1:(length(input$ShadowCurvegrouping)), function(i) {
            groups <- input$ShadowCurvegrouping
            defaultingcolors <- c("blue", "red", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('ShadowCurvecolors', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$ShadowScoreCurve <- renderPlot({
        EAElong <- EAElong()
        EAElong$Mouse_ID <- as.character(EAElong$Mouse_ID)
        grouping <- input$ShadowCurveGroup

        if(grepl(":", grouping, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(grouping, ":"))
            EAElong$combinedgrouping <- apply(EAElong[,(colnames(EAElong) %in% groups)], 1, paste0, collapse = " " )
            colnames(EAElong) <- gsub("combinedgrouping", grouping, colnames(EAElong))
        }
        EAElong$groupselected <- EAElong[,colnames(EAElong) == grouping]

        if(input$IndividualMouse == T){
            EAElong <- subset(EAElong, Mouse_ID == input$MouseSearch)
        }
        EAElong$Day <- as.numeric(EAElong$Day)

        colorset1 <- rep(NA, 1)
        for(i in 1:length(input$ShadowCurvegrouping)){
            colorset1[i] <- input[[(paste0('ShadowCurvecolors', i))]]
        }

        ggob = ggplot(EAElong, aes(y = Score, x = Day, group = Mouse_ID, color = groupselected)) + geom_line(alpha = 0.85)
        ggob = ggob + ylab(input$ScoreCurveylab) + xlab(input$ScoreCurvexlab) + facet_grid(groupselected~.)
        ggob = ggob + ggtitle(input$title) + theme_bw()
        ggob = ggob + scale_colour_manual(values = colorset1)
        ggob = ggob + theme(
            #axis.text.x.bottom = element_text(size=input$ScoreCurvexaxistext),
            #text = element_text(size=input$ScoreCurveLabels),
            legend.title = element_blank()
        )
        ggob
    } )

    ############################ Heatmap Hierarchical Clustering, High Scoring Histogram, and Statistical Modeling ############################

    EAElongAlign <- reactive({
        EAElongrm0 <- EAElong()
        EAElongrm1 <- (EAElongrm0 %>%
                           group_by(Mouse_ID) %>%
                           summarize(start=min(as.numeric(as.character(Day[Score > 0])), na.rm=TRUE)) %>%
                           as.data.frame() %>%
                           merge(EAElongrm0, ., "Mouse_ID"))

        EAElongrm1$'Day (offset)' = as.numeric(as.character(EAElongrm1$Day)) - EAElongrm1$start
        EAElongrm1
    })

    DaysAligned <- reactive({
        EAElongrm0 <- EAElong()
        EAElongrm1 <- (EAElongrm0 %>%
                           group_by(Mouse_ID) %>%
                           summarize(start=min(as.numeric(as.character(Day[Score > 0])), na.rm=TRUE)) %>%
                           as.data.frame() %>%
                           merge(EAElongrm0, ., "Mouse_ID"))

        EAElongrm1$'Day (offset)' = as.numeric(as.character(EAElongrm1$Day)) - EAElongrm1$start
        EAElongrm1 %>%
            group_by(Mouse_ID) %>%
            filter(row_number()==1)
    })

    PeakScoreDay <- reactive({
        EAElongrm0 <- EAElong()
        EAElongrm1 <- (EAElongrm0 %>%
                           group_by(Mouse_ID) %>%
                           top_n(1, Score) %>%
                           as.data.frame())
        EAElongrm1 %>%
            group_by(Mouse_ID) %>%
            top_n(-1, Day) %>%
            as.data.frame()
    })

    ############################ High Scoring Histogram ############################
    output$HighHistGroup <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "HighHistGroup", label = "What grouping do you want to use for faceting?", choices = out)
    })

    output$HighHistColor <- renderUI({
        EAEdata <- filedata()
        metadata <- EAEdata[,2:(input$metadatacols + 1)]
        # df$blankVar <- NA
        for(i in 1:(ncol(metadata))){
            combodf <- combinations(length(colnames(metadata)), i, colnames(metadata))
            if(i > 1){
                out2 <- apply(combodf, 1, paste, collapse = ":" )
            } else if (i ==1) {
                out2 <- combodf[,1]
            }

            if(i ==1){
                out <- (out2)
            } else if (i > 1){
                out <- c(out, out2)
            }
        }
        selectInput(inputId = "HighHistColor", label = "What grouping do you want to color?", choices = out)
    })

    highCounts <- reactive({
        EAElongrm0 <- EAElongAlign()
        highCounts = EAElongrm0 %>%
            #filter(as.integer(as.character(Day)) <= 40) %>%
            group_by_at(1:(input$metadatacols + 1)) %>%
            summarize(Score = sum(Score[!is.na(Score)] >= input$scorethreshold))
        highCounts
    })

    output$Histcolors <- renderUI({
        highCounts <- highCounts()
        inputselection <- input$HighHistColor
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            highCounts$combinedgrouping <- apply(highCounts[,(colnames(highCounts) %in% groups)], 1, paste0, collapse = " " )
            colnames(highCounts) <- gsub("combinedgrouping", inputselection, colnames(highCounts))
        }
        groupscolorspre <- highCounts[,colnames(highCounts) == inputselection]
        groupscolors <- unlist(unique(groupscolorspre))
        lapply(1:(length(groupscolors)), function(i) {
            #groups <- unique(highCounts$color)
            defaultingcolors <- c("blue", "#BF5700", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown","blue", "red", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('colors-hist', i)), label = (paste0('What color for ', groupscolors[[i]],"?")), value = defaultingcolors[i])
        })

    })

    output$HighScoreHist <- renderPlot({
        highCounts <- highCounts()
        inputselection <- input$HighHistGroup

        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            highCounts$variable <- apply(highCounts[,(colnames(highCounts) %in% groups)], 1, paste0, collapse = " " )
        } else {
            colnames(highCounts) <- gsub(input$HighHistGroup, "variable", colnames(highCounts))
            highCounts$Temp <- highCounts$variable
            colnames(highCounts) <- gsub("Temp", input$HighHistGroup, colnames(highCounts))
        }

        colorselection <- input$HighHistColor
        if(grepl(":", colorselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(colorselection, ":"))
            highCounts$color <- apply(highCounts[,(colnames(highCounts) %in% groups)], 1, paste0, collapse = " " )
        } else {
            if(colorselection == inputselection){
                highCounts$color <- highCounts$variable
            } else{
                colnames(highCounts) <- gsub(input$HighHistColor, "color", colnames(highCounts))
            }
        }

        ggo = ggplot(highCounts, aes(x=Score))
        ggo = ggo + facet_wrap(~ variable, nrow=2)
        ggo = ggo + geom_histogram(aes(fill=color), position='stack')
        ggo = ggo + theme_bw() + theme(
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill='ghostwhite'),
            legend.title = element_blank()
        )
        colorsethist <- rep(NA, 1)
        for(i in 1:(length(unique(highCounts$color)))){
            colorsethist[i] <- input[[(paste0('colors-hist', i))]]
        }
        ggo = ggo + scale_fill_manual(values=colorsethist) ## Change colors
        ggo = ggo + xlab(paste0('Number of Days with EAE >= ', input$scorethreshold))
        ggo = ggo + ylab('Number of Individuals') + ggtitle(input$title)
        ggo
    })

    HighScoreHist <- reactive({
        highCounts <- highCounts()
        inputselection <- input$HighHistGroup

        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            highCounts$variable <- apply(highCounts[,(colnames(highCounts) %in% groups)], 1, paste0, collapse = " " )
        } else {
            colnames(highCounts) <- gsub(input$HighHistGroup, "variable", colnames(highCounts))
            highCounts$Temp <- highCounts$variable
            colnames(highCounts) <- gsub("Temp", input$HighHistGroup, colnames(highCounts))
        }

        colorselection <- input$HighHistColor
        if(grepl(":", colorselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(colorselection, ":"))
            highCounts$color <- apply(highCounts[,(colnames(highCounts) %in% groups)], 1, paste0, collapse = " " )
        } else {
            if(colorselection == inputselection){
                highCounts$color <- highCounts$variable
            } else{
                colnames(highCounts) <- gsub(input$HighHistColor, "color", colnames(highCounts))
            }
        }

        ggo = ggplot(highCounts, aes(x=Score))
        ggo = ggo + facet_wrap(~ variable, nrow=2)
        ggo = ggo + geom_histogram(aes(fill=color), position='stack')
        ggo = ggo + theme_bw() + theme(
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill='ghostwhite'),
            legend.title = element_blank()
        )
        colorsethist <- rep(NA, 1)
        for(i in 1:(length(unique(highCounts$color)))){
            colorsethist[i] <- input[[(paste0('colors-hist', i))]]
        }
        ggo = ggo + scale_fill_manual(values=colorsethist) ## Change colors
        ggo = ggo + xlab(paste0('Number of Days with EAE >= ', input$scorethreshold))
        ggo = ggo + ylab('Number of Individuals') + ggtitle(input$title)
        ggo
    })

    output$downloadgraphHist <- downloadHandler(
        filename = function() { paste(input$title, 'High-Scoring-Histogram.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = HighScoreHist(), device = input$ScoreCurveImageType, width = input$HistWidth, height = input$HistHeight, dpi = 300, units = input$HistUnits)
        }
    )

    ############################ Modeling for High Scoring Histogram ############################

    output$VariableTitles <- renderUI({
        EAEdata <- filedata()
        variablegroups <- colnames(EAEdata)[2:(input$metadatacols + 1)]
        checkboxGroupInput("VariableTitles", "Choose which groups to include in the models", variablegroups, selected = variablegroups)
    })

    output$modelind <- renderPrint({
        highCounts <- highCounts()
        groupselection <- input$VariableTitles
        dept <- paste(groupselection, sep="", collapse=" + ")
        f <- as.formula(paste("Score", dept, sep = "~"))
        f
        if(input$PoiNBInd == "Poisson"){
            poissonNoInteractionModel = glm(f,
                                            family = poisson(link='log'),
                                            data = highCounts)
            drop1(poissonNoInteractionModel, test='LRT')
        } else if (input$PoiNBInd == "Negative Binomial"){
            summary(m1 <-glm.nb(f, data = highCounts))
        }

    })

    output$modelint <- renderPrint({
        highCounts <- highCounts()
        groupselection <- input$VariableTitles
        dept <- paste(groupselection, sep="", collapse=" * ")
        f <- as.formula(paste("Score", dept, sep = "~"))
        f
        if(input$PoiNBInt == "Poisson"){
            poissonNoInteractionModel = glm(f,
                                            family = poisson(link='log'),
                                            data = highCounts)
            drop1(poissonNoInteractionModel, test='LRT')
        } else if (input$PoiNBInt == "Negative Binomial"){
            summary(m1 <-glm.nb(f, data = highCounts))
        }
    })

    ############################ Dispersion Testing for High Scoring Histogram Model ############################

    output$modelindoverdisp <- renderPrint({
        highCounts <- highCounts()
        groupselection <- input$VariableTitles
        dept <- paste(groupselection, sep="", collapse=" + ")
        f <- as.formula(paste("Score", dept, sep = "~"))

    poissonNoInteractionModel = glm(f,
                                            family = poisson(link='log'),
                                            data = highCounts)
    m1 <-glm.nb(f, data = highCounts)
    if(input$PoiNBInd == "Poisson"){
        #testOverdispersion(poissonInteractionModel)
        dispersiontest(poissonNoInteractionModel,trafo=1)
    } else if (input$PoiNBInd == "Negative Binomial"){
        testOverdispersion(m1)
    }
    })

    output$modelintoverdisp <- renderPrint({
        highCounts <- highCounts()
        groupselection <- input$VariableTitles
        dept <- paste(groupselection, sep="", collapse=" * ")
        f <- as.formula(paste("Score", dept, sep = "~"))

        poissonInteractionModel = glm(f,
                                            family = poisson(link='log'),
                                            data = highCounts)
        m1 <-glm.nb(f, data = highCounts)

            if(input$PoiNBInt == "Poisson"){
                #testOverdispersion(poissonInteractionModel)
                dispersiontest(poissonInteractionModel,trafo=1)
            } else if (input$PoiNBInt == "Negative Binomial"){
                testOverdispersion(m1)
            }
        ##dispersiontest(m1)
    })

    ############################ ANOVA Page ############################
    output$VariableTitlesANOVA <- renderUI({
        EAEdata <- filedata()
        variablegroups <- colnames(EAEdata)[2:(input$metadatacols + 1)]
        checkboxGroupInput("VariableTitlesANOVA", "Choose which groups to include in the models", variablegroups, selected = variablegroups)
    })

    output$modelgroupind <- renderPrint({
        EAElongAlign <- EAElongAlign()
        groupselection <- input$VariableTitlesANOVA
        dept <- paste("Day", paste(groupselection, sep="", collapse=" * "), sep = " * ")
        f <- as.formula(paste(paste("Score", dept, sep = "~"), "+ (1 | Mouse_ID)", sep = " "))

        lmeOut = lmer(f, data=EAElongAlign)
        anova(lmeOut)
    })

    output$modelgroupdep <- renderPrint({
        EAElongAlign <- EAElongAlign()
        groupselection <- input$VariableTitlesANOVA
        dept <- paste("`Day (offset)`", paste(groupselection, sep="", collapse=" * "), sep = " * ")
        f <- as.formula(paste(paste("Score", dept, sep = "~"), "+ (1 | Mouse_ID)", sep = " "))
        f
        lmeOff = lmer(f, data=EAElongAlign)
        anova(lmeOff)
    })

    ############################ Heirarchical Heatmap Clustering ############################

    output$Unalignedcolors <- renderUI({
        EAEdata <- filedata()
        out <- rep(NA,1)
        for(i in 2:(input$metadatacols + 1)){
            if(i == 2){
                out <- unique(as.character(EAEdata[,i]))
            } else {
                out <- c(out, unique(as.character(EAEdata[,i])))
            }
        }
        variables <- (out)

        lapply(1:(length(variables)), function(i) {
            groups <- variables
            defaultingcolors <- c("blue", "orange", "purple", "darkgreen", "brown", "pink", "grey", "black", "orange", "brown")
            textInput((paste0('Unalignedcolors', i)), label = (paste0('What color for ',  groups[[i]], "?")), value = defaultingcolors[i])
        })
    })

    output$UnalignedHeatmap <- renderImage({
        EAEdata <- filedata()
        out <- rep(NA,1)
        for(i in 2:(input$metadatacols + 1)){
            if(i == 2){
                out <- unique(as.character(EAEdata[,i]))
            } else {
                out <- c(out, unique(as.character(EAEdata[,i])))
            }
        }
        variables <- (out)

        EAEdatap <- as.data.frame(filedata())
        colnames(EAEdatap) <- gsub('X', '', colnames(EAEdatap))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatap[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatap <- EAEdatap[rowSums(sickmice) > 0 , ]
        }

        colorset1 <- rep(NA, 1)
        for(i in 1:length(variables)){
            colorset1[i] <- input[[(paste0('Unalignedcolors', i))]]
        }

        mycolors <- rep(NA, 1)
        for(i in 2:(input$metadatacols + 1)){
            activevar <- unique(as.character(EAEdatap[,i]))
            if (i == 2){
                mycolorstemp <- colorset1[1:length(activevar)]
                names(mycolorstemp) <- activevar
                mycolors <- list(mycolorstemp)
                j <- length(activevar)
            } else {
                mycolorstemp <- colorset1[(j+1):(j+length(activevar))]
                names(mycolorstemp) <- activevar
                listemp <- list(mycolorstemp)
                mycolors <- c(mycolors, listemp)
                j <- (j+length(activevar))
            }
        }
        names(mycolors) <- colnames(EAEdatap)[2:(input$metadatacols + 1)]

        rownames(EAEdatap) <- EAEdatap$Mouse_ID
        # if(input$pheatmapcolorscheme == "rainbow"){
        #     selectedcolor <- rainbow(100)
        # } else if (input$pheatmapcolorscheme == "base"){
        #     selectedcolor <-colorRampPalette(rev(brewer.pal(n = 7, name =
        #                                                         "RdYlBu")))(100)
        # } else if (input$pheatmapcolorscheme == "heat.colors"){
        #     selectedcolor <- heat.colors(100)
        # } else if (input$pheatmapcolorscheme == "cm.colors"){
        #     selectedcolor <- cm.colors(100)
        # } else if (input$pheatmapcolorscheme == "topo.colors"){
        #     selectedcolor <- topo.colors(100)
        # }

        if(input$mouseIDUnaligned == TRUE){
            fileout <- pheatmap(
                mat = subset(EAEdatap, select=-c(1:(input$metadatacols + 1))),
                main =  input$title,
                #color = selectedcolor,
                annotation_row = EAEdatap[,c(2:(input$metadatacols + 1))],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = FALSE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeUnaligned
            )
        } else{
            fileout <- pheatmap(
                mat = subset(EAEdatap, select=-c(1:(input$metadatacols + 1))),
                main =  input$title,
               # color = selectedcolor,
                annotation_row = EAEdatap[,c(2:(input$metadatacols + 1))],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = TRUE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeUnaligned
            )
        }

        outfile <- tempfile(fileext = '.png')

        # Reactive values that read the window size
        width  <- session$clientData$output_UnalignedHeatmap_width
        height <- session$clientData$output_UnalignedHeatmap_height
        pixelratio <- session$clientData$pixelratio

        # Generate the PNG
        png(outfile, width = width*pixelratio, height = height*pixelratio, res = 72*pixelratio)
        print(fileout)
        dev.off()

        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text")
    }, deleteFile = TRUE)

    UnalignedHeatmap <- reactive({
        EAEdata <- filedata()
        out <- rep(NA,1)
        for(i in 2:(input$metadatacols + 1)){
            if(i == 2){
                out <- unique(as.character(EAEdata[,i]))
            } else {
                out <- c(out, unique(as.character(EAEdata[,i])))
            }
        }
        variables <- (out)


        EAEdatap <- as.data.frame(filedata())
        colnames(EAEdatap) <- gsub('X', '', colnames(EAEdatap))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatap[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatap <- EAEdatap[rowSums(sickmice) > 0 , ]
        }

        colorset1 <- rep(NA, 1)
        for(i in 1:length(variables)){
            colorset1[i] <- input[[(paste0('Unalignedcolors', i))]]
        }

        mycolors <- rep(NA, 1)
        for(i in 2:(input$metadatacols + 1)){
            activevar <- unique(as.character(EAEdatap[,i]))
            if (i == 2){
                mycolorstemp <- colorset1[1:length(activevar)]
                names(mycolorstemp) <- activevar
                mycolors <- list(mycolorstemp)
                j <- length(activevar)
            } else {
                mycolorstemp <- colorset1[(j+1):(j+length(activevar))]
                names(mycolorstemp) <- activevar
                listemp <- list(mycolorstemp)
                mycolors <- c(mycolors, listemp)
                j <- (j+length(activevar))
            }
        }
        names(mycolors) <- colnames(EAEdatap)[2:(input$metadatacols + 1)]

        # if(input$pheatmapcolorscheme == "rainbow"){
        #     selectedcolor <- rainbow(100)
        # } else if (input$pheatmapcolorscheme == "base"){
        #     selectedcolor <-colorRampPalette(rev(brewer.pal(n = 7, name =
        #                                                         "RdYlBu")))(100)
        # } else if (input$pheatmapcolorscheme == "heat.colors"){
        #     selectedcolor <- heat.colors(100)
        # } else if (input$pheatmapcolorscheme == "cm.colors"){
        #     selectedcolor <- cm.colors(100)
        # } else if (input$pheatmapcolorscheme == "topo.colors"){
        #     selectedcolor <- topo.colors(100)
        # }

        rownames(EAEdatap) <- EAEdatap$Mouse_ID
        if(input$mouseIDUnaligned == TRUE){
            fileout <- pheatmap(
                mat = subset(EAEdatap, select=-c(1:(input$metadatacols + 1))),
                main =  input$title,
                #color = selectedcolor,
                annotation_row = EAEdatap[,c(2:(input$metadatacols + 1))],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = FALSE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeUnaligned
            )
        } else{
            fileout <- pheatmap(
                mat = subset(EAEdatap, select=-c(1:(input$metadatacols + 1))),
                main =  input$title,
                #color = selectedcolor,
                annotation_row = EAEdatap[,c(2:(input$metadatacols + 1))],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = TRUE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeUnaligned
            )
        }
        fileout

    })

    output$downloadgraphUnalignedHeatmap <- downloadHandler(
        filename = function() { paste(input$title, 'UnalignedHeatmap.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = UnalignedHeatmap(), device = input$ScoreCurveImageType,  width = input$HeatmapWidth, height = input$HeatmapHeight, dpi = 300, units = input$HeatmapUnits)
        }
    )

    output$AlignedHeatmap <- renderImage({
        EAEdata <- filedata()
        out <- rep(NA,1)
        for(i in 2:(input$metadatacols + 1)){
            if(i == 2){
                out <- unique(as.character(EAEdata[,i]))
            } else {
                out <- c(out, unique(as.character(EAEdata[,i])))
            }
        }
        variables <- (out)
        ############ Processing
        EAElongrm1 <- as.data.frame(EAElongAlign())

        EAEdatap <- as.data.frame(filedata())
        colnames(EAEdatap) <- gsub('X', '', colnames(EAEdatap))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatap[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatap <- EAEdatap[rowSums(sickmice) > 0 , ]
        }
        #EAEdatap$TGS <- paste(EAEdatap$Treatment, EAEdatap$Sex)
        rownames(EAEdatap) <- EAEdatap$Mouse_ID

        ############ Graphing
        AlignedEAElongtest1 = EAElongrm1[ , c('Day (offset)', 'Mouse_ID', 'Score')] %>%
            pivot_wider(names_from = c("Mouse_ID"),values_from = Score) %>%
            as.data.frame()
        AlignedEAElongtest1 <- AlignedEAElongtest1[order(AlignedEAElongtest1$`Day (offset)`),]
        AlignedEAElongtest2 <- AlignedEAElongtest1[,-1]
        rownames(AlignedEAElongtest2) <- AlignedEAElongtest1[,1]

        EAEdatap <- EAEdatap[order(rownames(EAEdatap)),]

        colorset1 <- rep(NA, 1)
        for(i in 1:length(variables)){
            colorset1[i] <- input[[(paste0('Unalignedcolors', i))]]
        }

        mycolors <- rep(NA, 1)
        for(i in 2:(input$metadatacols + 1)){
            activevar <- unique(as.character(EAEdatap[,i]))
            if (i == 2){
                mycolorstemp <- colorset1[1:length(activevar)]
                names(mycolorstemp) <- activevar
                mycolors <- list(mycolorstemp)
                j <- length(activevar)
            } else {
                mycolorstemp <- colorset1[(j+1):(j+length(activevar))]
                names(mycolorstemp) <- activevar
                listemp <- list(mycolorstemp)
                mycolors <- c(mycolors, listemp)
                j <- (j+length(activevar))
            }
        }
        names(mycolors) <- colnames(EAEdatap)[2:(input$metadatacols + 1)]
        # if(input$pheatmapcolorscheme == "rainbow"){
        #     selectedcolor <- rainbow(100)
        # } else if (input$pheatmapcolorscheme == "base"){
        #     selectedcolor <-colorRampPalette(rev(brewer.pal(n = 7, name =
        #                                                         "RdYlBu")))(100)
        # } else if (input$pheatmapcolorscheme == "heat.colors"){
        #     selectedcolor <- heat.colors(100)
        # } else if (input$pheatmapcolorscheme == "cm.colors"){
        #     selectedcolor <- cm.colors(100)
        # } else if (input$pheatmapcolorscheme == "topo.colors"){
        #     selectedcolor <- topo.colors(100)
        # }

        if(input$mouseIDAligned == TRUE){
            fileout <- pheatmap(
                mat = t(AlignedEAElongtest2),
                #color = selectedcolor,
                annotation_row = EAEdatap[,2:(input$metadatacols + 1)],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = FALSE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeAligned)
        } else{
            fileout <- pheatmap(
                mat = t(AlignedEAElongtest2),
               # color = selectedcolor,
                annotation_row = EAEdatap[,2:(input$metadatacols + 1)],
                annotation_colors = mycolors,
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = TRUE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeAligned)
        }


        outfile <- tempfile(fileext = '.png')

        # Reactive values that read the window size
        width  <- session$clientData$output_AlignedHeatmap_width
        height <- session$clientData$output_AlignedHeatmap_height
        pixelratio <- session$clientData$pixelratio

        # Generate the PNG
        png(outfile, width = width*pixelratio, height = height*pixelratio, res = 72*pixelratio)
        print(fileout)
        dev.off()

        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = width,
             height = height,
             alt = "This is alternate text")
    }, deleteFile = TRUE)

    AlignedHeatmap <- reactive({
        ############ Processing
        EAElongrm1 <- as.data.frame(EAElongAlign())

        EAEdatap <- as.data.frame(filedata())
        colnames(EAEdatap) <- gsub('X', '', colnames(EAEdatap))
        if (input$dropzeros == T){
            sickmice <- sapply(EAEdatap[,-c(1:(input$metadatacols + 1))], function(x){
                x > 0.5 })
            EAEdatap <- EAEdatap[rowSums(sickmice) > 0 , ]
        }
        #EAEdatap$TGS <- paste(EAEdatap$Treatment, EAEdatap$Sex)
        rownames(EAEdatap) <- EAEdatap$Mouse_ID

        ############ Graphing
        AlignedEAElongtest1 = EAElongrm1[ , c('Day (offset)', 'Mouse_ID', 'Score')] %>%
            pivot_wider(names_from = c("Mouse_ID"),values_from = Score) %>%
            as.data.frame()
        AlignedEAElongtest1 <- AlignedEAElongtest1[order(AlignedEAElongtest1$`Day (offset)`),]
        AlignedEAElongtest2 <- AlignedEAElongtest1[,-1]
        rownames(AlignedEAElongtest2) <- AlignedEAElongtest1[,1]

        EAEdatap <- EAEdatap[order(rownames(EAEdatap)),]
        # if(input$pheatmapcolorscheme == "rainbow"){
        #     selectedcolor <- rainbow(100)
        # } else if (input$pheatmapcolorscheme == "base"){
        #     selectedcolor <-colorRampPalette(rev(brewer.pal(n = 7, name =
        #                                                         "RdYlBu")))(100)
        # } else if (input$pheatmapcolorscheme == "heat.colors"){
        #     selectedcolor <- heat.colors(100)
        # } else if (input$pheatmapcolorscheme == "cm.colors"){
        #     selectedcolor <- cm.colors(100)
        # } else if (input$pheatmapcolorscheme == "topo.colors"){
        #     selectedcolor <- topo.colors(100)
        # }

        if(input$mouseIDAligned == TRUE){
            fileout <- pheatmap(
                mat = t(AlignedEAElongtest2),
                #color = selectedcolor,
                annotation_row = EAEdatap[,2:(input$metadatacols + 1)],
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = FALSE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeAligned)
        } else{
            fileout <- pheatmap(
                mat = t(AlignedEAElongtest2),
                #color = selectedcolor,
                annotation_row = EAEdatap[,2:(input$metadatacols + 1)],
                cluster_cols = FALSE,
                clustering_method = 'complete',
                clustering_distance_rows = 'manhattan',
                show_colnames = FALSE,
                show_rownames = TRUE,
                annotation_names_row = FALSE,
                cutree_rows = input$cuttreeAligned)
        }

        fileout
    })

    output$downloadgraphAlignedHeatmap <- downloadHandler(
        filename = function() { paste(input$title, 'AlignedHeatmap.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = AlignedHeatmap(), device = input$ScoreCurveImageType, width = input$HeatmapWidth, height = input$HeatmapHeight, dpi = 300, units = input$HeatmapUnits)
        }
    )

    ############################ Weight Analysis ############################
    ## simple data load in for the weight .csv file
    weightfiledata <- reactive({
        if(input$testdata == TRUE){
            if(input$testdataselected == "Test Data Set 1"){
                store <- read.csv("testdataset1weights.csv")
            } else if (input$testdataselected == "Test Data Set 2"){
                store <- read.csv("testdataset2weights.csv")
            } else if (input$testdataselected == "Test Data Set 3"){
                store <- read.csv("testdataset3weights.csv")
            }
        }else{
            infile <- input$weightdatafile
            if (is.null(infile)) {
                # User has not uploaded a file yet
                return(NULL)
            }
            store <- read.csv(infile$datapath)
        }
        ## Reads csv and pulls out empty columns

        ## Need to add a drop zero function to pull out dropped mice
        store <- store[,sapply(store, function(x) { sum(!is.na(x)) > 0 })]
        as.data.frame(store)
    })

    ## Output for the datatable with formating
    output$weightsheet <- renderDT({
        out <- weightfiledata()
        scoredata <- filedata()
        ## read.csv() typically induces Xs in the column headers
        colnames(out) <- gsub('X', 'Day ', colnames(out))
        if(input$WeightMeta == TRUE){
            out <- out
        } else if (input$WeightMeta == FALSE){
            out <- as.data.frame(cbind(scoredata[,1:(input$metadatacols + 1)], out[,-1]))
        }

        store1 <- apply(out[,-c(1:(input$metadatacols + 1))], 2, function(x){gsub("X", NA, x)})
        store2 <- apply(store1, 2, function(x){as.numeric(x)})
        ## restitch
        out <- cbind(out[,1:(input$metadatacols + 1)], store2)

        if (input$dropzeros == T){
            dropped <- dropped()
            out <- add_column(out, Dropped = dropped, .after = "Mouse_ID")
            datatable(out, options = list(paging=FALSE)) %>%
                formatStyle('Dropped', target = 'row', backgroundColor = styleEqual(c(T, F), c('#A51E01', '')))
        } else {
            datatable(out, options = list(paging=FALSE))
        }

    })

    ## Processing of the weight .csv to a useable format
    activeweightdata <- reactive({
        out <- weightfiledata()
        scoredata <- filedata()
        colnames(out) <- gsub('X', '', colnames(out))
        if(input$WeightMeta == TRUE){
            out <- out
        } else if (input$WeightMeta == FALSE){
            out <- as.data.frame(cbind(scoredata[,1:(input$metadatacols + 1)], out[,-1]))
        }

        store1 <- apply(out[,-c(1:(input$metadatacols + 1))], 2, function(x){gsub("X", NA, x)})
        store2 <- apply(store1, 2, function(x){as.numeric(x)})
        ## restitch
        out <- cbind(out[,1:(input$metadatacols + 1)], store2)

        if (input$dropzeros == T){
            dropped <- dropped()
            out <- add_column(out, Dropped = dropped, .after = "Mouse_ID")
            out <- out[out$Dropped == FALSE, colnames(out) != "Dropped"]
        } else {
            out <- out
        }
        as.data.frame(out)
    })

    percentWeightData<- reactive({
        fulldata <- activeweightdata()
        meta <- fulldata[,(1:(input$metadatacols + 1))]
        baseline <- as.vector(as.numeric(fulldata[,(input$metadatacols + 2)]))
        partialdata <-fulldata[,-(1:(input$metadatacols + 1))]
        diff <- sweep(partialdata,1,baseline,"-")
        percent <- t(t((diff / baseline)*100))
        as.data.frame(cbind(meta, percent))
    })

    weightstats <- reactive({
        if(input$weightmetric == "Percent Weights"){
            data <- percentWeightData()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        } else {
            data <- activeweightdata()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        }

        inputselection <- input$WeightGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            datalong$combinedgrouping <- apply(datalong[,(colnames(datalong) %in% groups)], 1, paste0, collapse = " " )
            colnames(datalong) <- gsub("combinedgrouping", inputselection, colnames(datalong))
        }
        datalong$groupselected <- datalong[,colnames(datalong) == input$WeightGroup]
        datalong <- datalong[datalong$groupselected %in% input$Weightgrouping,]
        if(length(input$Weightgrouping) < 2){

        } else if(length(input$Weightgrouping) == 2){
            if (input$stats == "Non-parametric"){
                resTselect <- compare_means(PercentWeight ~ groupselected, datalong, method = 'wilcox.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resTselect <- compare_means(PercentWeight ~ groupselected, datalong, method = 't.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resTselect)
        } else if(length(input$Weightgrouping) > 2){
            if (input$stats == "Non-parametric"){
                resB <- compare_means(PercentWeight ~ groupselected, datalong, method = 'kruskal.test', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            } else if (input$stats == 'Parametric'){
                resB <- compare_means(PercentWeight ~ groupselected, datalong, method = 'anova', group.by = c("Day"), paired = F, p.adjust.method = input$padjumethod)
            }
            as.data.frame(resB)
        }

    })

    output$weightstatstable <- renderTable({
        weightstats()
    })

    output$percentWeightGraph <- renderPlot({
        resA <- weightstats()
        if(input$weightmetric == "Percent Weights"){
            data <- percentWeightData()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        } else {
            data <- activeweightdata()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        }

        inputselection <- input$WeightGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            datalong$combinedgrouping <- apply(datalong[,(colnames(datalong) %in% groups)], 1, paste0, collapse = " " )
            colnames(datalong) <- gsub("combinedgrouping", inputselection, colnames(datalong))
        }
        datalong$groupselected <- datalong[,colnames(datalong) == input$WeightGroup]
        datalong <- datalong[datalong$groupselected %in% input$Weightgrouping,]

        datalong <- datalong[complete.cases(datalong), ]

        # if(input$X == "Standard Error of the Mean"){
        Mean <- datalong %>%
            group_by(Day, groupselected) %>%
            summarize(avg = mean(PercentWeight), n = n(),
                      sd = sd(PercentWeight), se = sd/sqrt(n), avgse = avg+se)
        # } else {
        #   Mean <- datalong %>%
        #     group_by(Day, groupselected) %>%
        #     summarize(avg = mean(PercentWeight), n = n(),
        #               sd = sd(PercentWeight), se = sd/sqrt(n), avgse = avg+sd)
        # }

        if(length(input$Weightgrouping) > 1){
            MaxScores <- aggregate(avgse ~ Day, data = Mean, max)
            resA <- merge(resA, MaxScores, by=c('Day'))
            colnames(resA)[colnames(resA)=="avgse"] <- "max"
            resA <- resA[resA$p.adj != 'NaN', ]
            if(input$WeightPLabels == "p"){
                resA$p.adj.star1 <- ifelse(resA$p < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$WeightStar1
                resA$p.adj.star2 <- ifelse(resA$p < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$WeightStar2
                resA$p.adj.star3 <- ifelse(resA$p < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$WeightStar3
            } else {
                resA$p.adj.star1 <- ifelse(resA$p.adj < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$WeightStar1
                resA$p.adj.star2 <- ifelse(resA$p.adj < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$WeightStar2
                resA$p.adj.star3 <- ifelse(resA$p.adj < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$WeightStar3
            }

            resA$Day <- as.numeric(resA$Day)}

        ggob = ggline(datalong,
                      y = "PercentWeight",
                      x = "Day", group = "groupselected", add = "mean_se", width = 5,
                      color = "groupselected", shape = "groupselected", size = input$WeightLineThickness, point.size = input$WeightPointSize)
        if(input$weightmetric == "Percent Weights" && is.na(input$Weightylab)){
            ggob = ggob +  ylab("Percent Weight Compared to Baseline")
        } else if (input$weightmetric == "Weights" && is.na(input$Weightylab)){
            ggob = ggob +  ylab("Weight (g)")
        } else {
            ggob = ggob + ylab(input$Weightylab)
        }
        colorset2 <- rep(NA, 1)
        for(i in 1:length(input$Weightgrouping)){
            colorset2[i] <- input[[(paste0('colorsweight', i))]]
        }
        shapeset2 <- rep(NA, 1)
        for(i in 1:length(input$Weightgrouping)){
            shapeset2[i] <- as.numeric(input[[(paste0('shapesweight', i))]])
        }
        ggob = ggob + scale_shape_manual(values = shapeset2)
        ggob = ggob + scale_colour_manual(values = colorset2)
        ggob = ggob + xlab(input$Weightxlab)
        if(length(input$Weightgrouping) > 1){
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star1.height, label=resA$p.adj.star1, size=input$WeightStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star2.height, label=resA$p.adj.star2, size=input$WeightStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star3.height, label=resA$p.adj.star3, size=input$WeightStarSize)
        } #
        if(input$weightBaseline == T && input$weightmetric == "Percent Weights"){
            ggob = ggob + geom_hline( yintercept = 0, linetype="dashed", color = "red")
        }
        ggob = ggob + theme(
            # axis.text.x.bottom = element_text(size=input$Weightxaxistext),
            # text = element_text(size=input$WeightLabels),
            legend.title = element_blank(),
            legend.text=element_text(size=input$WeightLegendText)
        )
        ggob
    })

    percentWeightGraph <- reactive({
        resA <- weightstats()
        if(input$weightmetric == "Percent Weights"){
            data <- percentWeightData()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        } else {
            data <- activeweightdata()
            datalong <- data %>% gather('Day', 'PercentWeight', -c(1:(input$metadatacols + 1)))
        }

        inputselection <- input$WeightGroup
        if(grepl(":", inputselection, fixed = TRUE) == TRUE){
            groups <- unlist(strsplit(inputselection, ":"))
            datalong$combinedgrouping <- apply(datalong[,(colnames(datalong) %in% groups)], 1, paste0, collapse = " " )
            colnames(datalong) <- gsub("combinedgrouping", inputselection, colnames(datalong))
        }
        datalong$groupselected <- datalong[,colnames(datalong) == input$WeightGroup]
        datalong <- datalong[datalong$groupselected %in% input$Weightgrouping,]

        datalong <- datalong[complete.cases(datalong), ]

        # if(input$X == "Standard Error of the Mean"){
        Mean <- datalong %>%
            group_by(Day, groupselected) %>%
            summarize(avg = mean(PercentWeight), n = n(),
                      sd = sd(PercentWeight), se = sd/sqrt(n), avgse = avg+se)
        # } else {
        #   Mean <- datalong %>%
        #     group_by(Day, groupselected) %>%
        #     summarize(avg = mean(PercentWeight), n = n(),
        #               sd = sd(PercentWeight), se = sd/sqrt(n), avgse = avg+sd)
        # }

        if(length(input$Weightgrouping) > 1){
            MaxScores <- aggregate(avgse ~ Day, data = Mean, max)
            resA <- merge(resA, MaxScores, by=c('Day'))
            colnames(resA)[colnames(resA)=="avgse"] <- "max"
            resA <- resA[resA$p.adj != 'NaN', ]
            if(input$WeightPLabels == "p"){
                resA$p.adj.star1 <- ifelse(resA$p < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$WeightStar1
                resA$p.adj.star2 <- ifelse(resA$p < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$WeightStar2
                resA$p.adj.star3 <- ifelse(resA$p < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$WeightStar3
            } else {
                resA$p.adj.star1 <- ifelse(resA$p.adj < 0.05, '*', "")
                resA$p.adj.star1.height <- resA$max + input$WeightStar1
                resA$p.adj.star2 <- ifelse(resA$p.adj < 0.01, '*', "")
                resA$p.adj.star2.height <- resA$max + input$WeightStar2
                resA$p.adj.star3 <- ifelse(resA$p.adj < 0.001, '*', "")
                resA$p.adj.star3.height <- resA$max + input$WeightStar3
            }

            resA$Day <- as.numeric(resA$Day)}

        ggob = ggline(datalong,
                      y = "PercentWeight",
                      x = "Day", group = "groupselected", add = "mean_se", width = 5,
                      color = "groupselected", shape = "groupselected", size = input$WeightLineThickness, point.size = input$WeightPointSize)
        if(input$weightmetric == "Percent Weights" && is.na(input$Weightylab)){
            ggob = ggob +  ylab("Percent Weight Compared to Baseline")
        } else if (input$weightmetric == "Weights" && is.na(input$Weightylab)){
            ggob = ggob +  ylab("Weight (g)")
        } else {
            ggob = ggob + ylab(input$Weightylab)
        }
        colorset2 <- rep(NA, 1)
        for(i in 1:length(input$Weightgrouping)){
            colorset2[i] <- input[[(paste0('colorsweight', i))]]
        }
        shapeset2 <- rep(NA, 1)
        for(i in 1:length(input$Weightgrouping)){
            shapeset2[i] <- as.numeric(input[[(paste0('shapesweight', i))]])
        }
        ggob = ggob + scale_shape_manual(values = shapeset2)
        ggob = ggob + scale_colour_manual(values = colorset2)
        ggob = ggob + xlab(input$Weightxlab)
        if(length(input$Weightgrouping) > 1){
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star1.height, label=resA$p.adj.star1, size=input$WeightStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star2.height, label=resA$p.adj.star2, size=input$WeightStarSize)
            ggob = ggob + annotate('text', x= resA$Day - min(as.numeric(datalong$Day)) + 1, y=resA$p.adj.star3.height, label=resA$p.adj.star3, size=input$WeightStarSize)
        } #
        if(input$weightBaseline == T && input$weightmetric == "Percent Weights"){
            ggob = ggob + geom_hline( yintercept = 0, linetype="dashed", color = "red")
        }
        ggob = ggob + theme(
            # axis.text.x.bottom = element_text(size=input$Weightxaxistext),
            # text = element_text(size=input$WeightLabels),
            legend.title = element_blank(),
            legend.text=element_text(size=input$WeightLegendText)
        )
        ggob
    })

    output$downloadgraphWeight <- downloadHandler(
        filename = function() { paste(input$title, 'Weight.',input$ScoreCurveImageType, sep='') },
        content = function(file) {
            ggsave(file, plot = percentWeightGraph(), device = input$ScoreCurveImageType, width = input$WeightWidth, height = input$WeightHeight, dpi = 300, units = input$WeightUnits)
        })

    output$downloadstatsWeight <- downloadHandler(
        filename = function() { paste(input$title, 'Weight-comparisons.',"csv", sep='') },
        content = function(file) {
            write.csv(weightstats(), file)
        })

} ## End of Server
