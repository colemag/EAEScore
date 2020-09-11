library(shinydashboard)
ui <- dashboardPage(
    dashboardHeader(title = "EAE Analysis Suite"),
    dashboardSidebar(
        sidebarMenu(
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            ##Burnt Orange Header
            tags$head(tags$style(HTML('.logo {background-color: #BF5700 !important;}
                              .navbar {background-color: #BF5700 !important;}'))),
            tags$head(tags$style(HTML('
                     .main-header .logo {
                     #font-family: "Georgia", Times, "Times New Roman", serif;
                     font-weight: bold;
                     #font-size: 24px;
                                      }'))),
            ## Where the tabs are created
            menuItem("EAE Information", tabName = "EAE", icon = icon("dashboard")),
            menuItem("Data Upload", tabName = "dashboard", icon = icon("upload")),
            menuItemOutput("menuexp_summ"),
            #menuItem("Experimental Summary", tabName = "exp_summ", icon = icon("clipboard-check")),
            menuItemOutput("menuscore_sheet"),
            #menuItem("Score Sheet", tabName = "score_sheet", icon = icon("database")),
            menuItemOutput("menuscore_curve"),
            #menuItem("Score Curve", tabName = "score_curve", icon = icon("chart-area")),
            menuItemOutput("menuheatmaps"),
            #menuItem("Heatmap Clustering", tabName = "heatmaps", icon = icon("chart-area")),
            menuItemOutput("menuhistogram"),
            #menuItem("High Scoring Histogram", tabName = "histogram", icon = icon("chart-area")),
            menuItemOutput("menuanova"),
            menuItemOutput("menuind_mouse"),
            #menuItem("Individual Mouse Curves", tabName = "ind_mouse", icon = icon("chart-area")),
            menuItemOutput("menuweight_sheet"),
            menuItemOutput("menuweight_graph"),
            menuItem("Tuitorials and Guides", tabName = "tuitorials", icon = icon("book-open")),
            menuItem("FAQs", tabName = "faqs", icon = icon("question")),
            menuItem("Citation", tabName = "citation", icon = icon("pen-fancy"))
        )## End of sidebarMenu
    ),
    dashboardBody(
        tabItems(
tabItem(tabName = "EAE",
        fluidPage(column(6,
            box(width=12, title = "Welcome", status ="primary",
                "Welcome to the EAE Score Analysis Suite!
                 This tool has been designed to help researchers quickly analyze data from the animal model Experimental Autoimmune Encephalomyelitis (EAE).
                  Head over to the data upload tab on the left navigation bar to get started!
                (There is also an option to load in test/practice data to practice with)"),
            box(width=12, title="EAE Mouse Scoring", status ="primary", collapsible = TRUE, tableOutput("scoreguidelinetable")
                )),
            box( title = "What is Experimental Autoimmune Encephalomyelitis (EAE)?", status ="primary",
                 ## first paragraph
                "Experimental autoimmune encephalomyelitis (EAE) is one of the most common models of multiple sclerosis (MS) in experimental
                animals and has been used in mice, rats, macaques, and marmosets (Bert et al., 2011; Rose et al., 1987). Typically, EAE is
                induced by injecting an emulsification of a CNS myelin antigen such as myelin basic protein (MBP), myelin oligodendrocyte
                glycoprotein (MOG), or proteolipid protein (PLP), together with an adjuvant, such as complete Freund’s adjuvant and Pertussis
                toxin. Besides the typical induction method mentioned above, EAE can also be induced with other methods such as adoptive
                transfer of immune cells or infection with viruses (Theiler’s murine encephalomyelitis virus, murine hepatitis virus).
                In addition, there are also transgenic models of EAE, such as the 2D2 mouse model, which create myelin-reactive T-cells
                that leads to the development of spontaneously optic neuritis and transverse myelitis without the need for induction
                (Glatigny and Bettelli, 2018). ", br(), br(),
                ## Second paragraph
                "In models of induced EAE, mice begin to display symptoms approximately one week following induction. The disease severity
                usually peaks around days 10-14 post induction, and depending on the mouse model and dose of adjuvants, symptoms can be chronic or go
                through cycles of remission and relapses. Clinical EAE symptoms in mice are generally scored on a scale from 0-5 with a
                brief outline from ", tags$a(href= "https://hookelabs.com/protocols/eaeAI_C57BL6.html","Hooke’s protocol and EAE scoring"), "shown to the left.", br(), br(),

tags$details(tags$summary(("References")),
"Bert, A., Gran, B., & Weissert, R. (2011). EAE: imperfect but useful models of multiple sclerosis. Trends in molecular medicine, 17(3), 119-125.", br(), br(),
"Glatigny, S., & Bettelli, E. (2018). Experimental autoimmune encephalomyelitis (EAE) as animal models of multiple sclerosis (MS). Cold Spring Harbor Perspectives in Medicine, 8(11), a028977.", br(), br(),
"Rose, L. M., Clark, E. A., Hruby, S., & Alvord Jr, E. C. (1987). Fluctuations of T-and B-cell subsets in basic protein-induced experimental allergic encephalomyelitis (EAE) in long-tailed macaques. Clinical immunology and immunopathology, 44(1), 93-106.", br(), br(),
"'Appendix A: EAE scoring guide,' Hooke catalog no. EK-2110"))
        )## End of fluidRow
        ),## End of EAE
tabItem( tabName = "dashboard",
        fluidRow(
            column(width = 6,
            box(title = "What is the EAE Analysis Suite?", width =12, status = "primary", collapsible = T, "
                The EAE Analysis Suite is a web-based tool that allows researchers to easily graph data and perform analyses,
                including traditional score curves, area under the curve, score threshold modeling, ANOVA modeling, and heatmap clustering.
                Below you will find a description of the key specifications to start the app and begin your analysis. Once you provide data
                the other tabs for data analysis will appear on the left navigation bar."),
            box(title = "Explanation of Settings", width =12, status = "primary", collapsible = T,
                strong("File Upload"), br(), "Upload your file of scores in the format found", tags$a(href="https://bit.ly/2xPdVnF", "here."), br(), "1) Make sure you save it as a csv", br(),
                "2) Have the ID column titled Mouse_ID exactly (see example)", br(),
                "3) Don't end Mouse_IDs in 0", br(),
                "4) Keep the variable columns after the Mouse_ID column but before the score columns!", br(), br(),
                "If you just want to test the analysis suite, you can use the Test Data option box to automatically pipe in your choice of three different test data sets!",
                br(), br(),
                strong("Number of Variable Columns"), br(), "The number of variables is the number
                of columns with metadata after the Mouse_ID column before the score columns (e.g. a column for sex and a column for treatment would be two variable columns).",
                br(), br(),
                strong("Title"), br(),
                "The title you enter will show up on the top of graphs.", br(),br(),
                strong("Statistical Testing Method"), br(),
                "Please select the statistical paradigm that will be used for your analyses. Choose this setting based on the distribution of your data.
                As a quick rule of thumb, a high n usually is parametric (normally distributed). Whereas with a small n non-parametric testing is the better choice.",

                br(),br(), strong("p-value Adjustment Method"), br(),
                "You can choose the p-value correction method from the most popular methods including FDR, Benjamini Hockberg, Hommel, etc. For each graph, you will be able to choose whether to mark
                significance based on the corrected or non-corrected p-value.",br(),br(), strong('Dropping "Zeros"'), br(), "The 'drop zero' setting will remove mice that never scored over a score of 0.5. These
                mice are not representative of the typical EAE disease progression and are often removed for analysis. To include all mice in your analysis, deselect this option
                (it is on as a default). Any mice that are removed are reported as such on the experimental summary page and are highlighted in red on the score sheet tab.",br(),br(), strong("Weight Analysis"), br(),
                "Select the weight analysis option if you also want to graph the weights of mice in the experiment. When uploading data for this option,
                the mice must be in the same row order as in the score sheet upload. If you include the variable columns (columns between Mouse_ID and weight columns)
                please select the additional option that pops up to indicate this (i.e. Are your variable columns also present?).",br(),br(), strong("Image Output File Type"), br(),"All graphs have a file export system and the default
                image export type is determined from this setting. The vector-based eps file format is recommended, though png and jpeg are also available.")
            ),
            box(title = "File Upload", status = "primary",
                conditionalPanel("input.testdata == false",
                fileInput("datafile", "Choose EAE Score CSV File",
                          accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))),
                checkboxInput(inputId = "testdata", label = "Use Test/Training Data instead?", value = F),
                conditionalPanel("input.testdata == true",
                                 selectInput(inputId = "testdataselected", label = "Which test data would you like to use",
                                             choices = c("Test Data Set 1", "Test Data Set 2", "Test Data Set 3")),
                                 div("Test Data Sucessfully Imported", style = "color:green")),
                conditionalPanel("input.testdata == false",
                numericInput(inputId = "metadatacols", label = "How many variable columns do you have?", value = 2)),
                textInput(inputId = "title", label = "What do you want your title to be?"),
                selectInput(inputId = "stats", label = "Parametric or Non-parametric testing?", choices = c("Parametric", "Non-parametric"), selected = "Non-parametric"),
                selectInput(inputId = "padjumethod", label = "p-value adjustment method?",
                            choices = (c("Bonferroni" ="bonferroni", "Hochberg" ="holm", "Hommel"="hommel",
                                         "FDR"="fdr", "BY"="BY")), selected = "fdr"),
                checkboxInput(inputId = "dropzeros", label = "Drop Zeros?", value = T),
                checkboxInput(inputId = "weightanalysis", label = "Run a Weight Analysis?", value = F),

                conditionalPanel("input.weightanalysis == true",
                                 conditionalPanel("input.testdata == false",
                                 checkboxInput(inputId = "WeightMeta", label = "Are your varaible columns also present on your weight.csv file?", value = F)),
                                 fileInput("weightdatafile", "Optional Weight Analysis File Upload",
                                           accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))),

                selectInput("ScoreCurveImageType", label = "What file type do you want the image output files?", choices = c("png", "jpeg", "eps"), selected = "jpeg")),

                )),## End of dashboard tab
tabItem( tabName = "exp_summ", fluidRow(conditionalPanel(condition = "output.fileUploaded",
       box(title = "Summary", status = "primary", solidHeader = TRUE, width = 12, collapsible = T,
       strong(textOutput("dropped")), br(),
       column(6,uiOutput("SummaryGroup")), column(12,tableOutput("sickfreq"))
       ),
       box( width =4, height = 300,title = "Graph Parameters", collapsible = T,uiOutput("ScoreCurveGroupMax"),
             numericInput(inputId = "MaxLegendSize", label = "Legend Size", value = 8)
       ),
       box(title = "Graphic Export Size",width = 4, height = 300,collapsible = T, numericInput(inputId = "ComboWidth", label = "Width of downloaded graphic", value = 9),  numericInput(inputId = "ComboHeight", label = "Height of downloaded graphic", value = 6)),
       box( title = "Graphic Export", width = 4, height = 300,collapsible = T, selectInput("ComboUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in"), downloadButton("downloadgraphCombo", "Download Graphic")),
       box(width =8, status = "primary", plotOutput("combinedGraph")),
       box(width =4, uiOutput("ScoreMaxgrouping"), uiOutput("ScoreCurveGroupMaxcolors"))
       )## end of condition
        )## End of fluidRow
          ),## End of exp_summ
tabItem(tabName = "score_sheet",
        #tags$details(tags$summary(strong("Need Help?")), "Did you try uploading a file to the Choose CSV File argument to the left yet? "),
        tags$details(tags$summary(strong("What is this page?")), "On this page you can find the processed input from the csv file upload on the data upload page (or test data if you selected that option). This is really useful for finding entry errors and for seeing which mice were dropped from the study (i.e. if “Drop Zeros” was selected on the data upload page any dropped mice’s rows will be highlighted in red)."),
        br(), br(), div(style = 'overflow-x: scroll', DT::dataTableOutput("filedata"))
        ),## End of score_sheet
tabItem(tabName = "score_curve",
        #tags$details(tags$summary(strong("Need Help?")), "Did you try uploading a file to the 'Choose CSV File' argument to the left yet?"),
        tags$details(tags$summary(strong("What is this page?")), "The traditional EAE score curve shows different experimental groups' disease score, degree of ascending paralysis, overtime.
        Almost all of these operate on a 5 point scale with the most common procedure the Hook’s lab which can be found here. By using the provided spreadsheet template, score curves can be easily
        and reproducibly generated with this application with a variety of easy to change options. Start by uploading the data as a comma separate value sheet (csv file) to the submit button in the left sidebar.
        At this point, the score curve should appear on this tab as well as all the possible changes. The most important setting is the grouping selection which allows you to choose from the two treatment columns
        (treatment and sex) or a combination of both. It is important to note that the column names must be exactly Treatment and Sex in the csv for this application to work. Whether you use the columns for those
        variables is up to you, either column can be repurposed for any variable for grouping.

        The rest of the features allow for custom modification of the graphs to produce high-quality customizable figures. Figures can be copied directly out of the interface.",
                     br(), br(),
        "The other main feature on this tab is area under the curve. Area under the curve is a good technique to look at a cumulative difference between groups
        for the entire score curve or for a specific portion of it. Our application lets you choose the start and end date for this calculation marked by a green and red line respectively.
        To use this setting, select it from the first box below the Score Curve graph."),
        conditionalPanel(condition = "output.fileUploaded",
        box(status = "primary", title = "Score Curve", width =12, plotOutput("GraphScoreCurveselected")),
        fluidRow(
        box( width =4,
            conditionalPanel(condition = "output.fileUploaded", uiOutput("ScoreCurveGroup")),
            selectInput(inputId = "errorbars", label = "Error Metric of Error Bars", choices = c("Standard Error of the Mean", "Standard Deviation"), selected = "Standard Error of the Mean"),
            selectInput(inputId = "ScorePLabels", label = "Mark significance using which?", choices = c("p-value" = "p", "adjusted p-value" = "p.adj"), selected = "p"),
            uiOutput("ScoreCurvegrouping"),
            conditionalPanel(condition = "output.fileUploaded", checkboxInput("AuCGo", "Area under the Curve Analysis?", value = FALSE), conditionalPanel(condition = "input.AuCGo == true", uiOutput("AuCstart"), uiOutput("AuCend")),
                checkboxInput("showadvancesettings", "Show advance graphing settings?", value = FALSE),
                conditionalPanel(condition = "input.showadvancesettings == true", checkboxInput("showrelabelettings", "Allowing group name relabeling?", value = FALSE)),
                conditionalPanel(condition = "input.showrelabelettings == true",  uiOutput("newnames"))
                                                               )),
            conditionalPanel(condition = "output.fileUploaded",
                box( width = 4,
                    uiOutput("colors"),
                          conditionalPanel(condition = "input.showadvancesettings == true",
                                textInput(inputId = "ScoreCurvexlab", label = "What do you want the x axis label to be?", value = "Day"),   textInput(inputId = "ScoreCurveylab", label = "What do you want the y axis label to be?", value = "Disease Score"),
                                numericInput(inputId = "ScoreCurveLineThickness", label = "Line Thickness", value = 0.5),
                                numericInput(inputId = "ScoreCurvePointSize", label = "Point Size", value = 1),
                                numericInput(inputId = "ScoreCurveLegendText", label = "Legend Size", value = 8),
                                numericInput(inputId = "ScoreCurvexaxistext", label = "X Axis Text Size", value = 8),
                                numericInput(inputId = "ScoreCurveLabels", label = "Axis Labels Text Size", value = 14)
                           )
                ),
                box(width = 4, uiOutput("shapes"),  conditionalPanel(condition = "input.showadvancesettings == true",
                     uiOutput("ScoreCurveerrorbars"),
                     numericInput(inputId = "ScoreStar1", label = "Distance Between Error Bar and Star 1", value = 0.25 ),
                     numericInput(inputId = "ScoreStar2", label = "Distance Between Error Bar and Star 2", value = 0.5 ),
                     numericInput(inputId = "ScoreStar3", label = "Distance Between Error Bar and Star 3", value = 0.75),
                     numericInput(inputId = "ScoreStarSize", label = "Size of Significance Stars", value = 6))
                ))
                ),
                box(width = 6, height = 300,
                      selectInput("ScoreCurveUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in"),
                      numericInput(inputId = "ScoreCurveWidth", label = "Width of downloaded graphic", value = 9),
                      numericInput(inputId = "ScoreCurveHeight", label = "Height of downloaded graphic", value = 5)),
                   box(width = 6, height = 300,br(), br(),
                      downloadButton("downloadgraphScoreCurve", "Download Score Curve Graphic"), br(), br(), br(),
                      downloadButton("downloadstatsScoreCurve", "Download Statistical Testing")),
                    br(),
                    conditionalPanel(condition = "input.AuCGo == false", box(width =12, title = "Statistical Testing Results",
                                                                             tableOutput("SignificanceTGSselected"))),

        fluidRow(
                    conditionalPanel(condition = "input.AuCGo == true", hr(),
                                     h3("Area under the Curve", align = " center"),
                                     tags$head(
                                         tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                                     ), hr(),
                                     box(width = 6, height = 300, collapsable = T, div(style = 'overflow-y: scroll',tableOutput("AuCstattable"))),
                                     box(width = 6, height = 300, collapsable = T, div(style = 'overflow-x: scroll', tableOutput("resAuCstats")#, tableOutput("resAuCstatsbreakdown")
                                                                      )),
                                     box(width = 12, plotOutput("AuCErrorPlot")),

                       box(width = 6,
                           #checkboxGroupInput(inputId = "AuCErrorPlotGroup", label = "What grouping do you want to use?", choices = c("Treatment" = "Treatment", "Sex" = "Sex", "Treatment Group and Sex" = "TGS"), selected = "Treatment Group and Sex"),
                           textInput(inputId = "AuCErrorPlotylab", label = "What do you want the y axis label to be?", value = "Area under the Score Curve"),
                           numericInput(inputId = "AuCyaxistext", label = "Y Axis Text Size", value = 12),
                           numericInput(inputId = "AuCLabels", label = "Axis Labels Text Size", value = 14),
                           numericInput(inputId = "AuCPointSize", label = "Point Size", value = 2),
                           numericInput(inputId = "AuCLineSize", label = "Line Size", value = 0.5)
                         ),
                        box(width = 6,
                            textInput(inputId = "AuCErrorPlotxlab", label = "What do you want the x axis label to be?", value = "Grouping"),
                            numericInput(inputId = "AuCxaxistext", label = "X Axis Text Size", value = 12),
                            numericInput(inputId = "jittersize", label = "Jitter Width", value = 0.2),
                            numericInput(inputId = "AuCLegendSize", label = "Legend Size", value = 8),
                            selectInput(inputId = "AuCpsystem", label = "Significance Markings", c("p-values", "star system"), selected = "p-values")
                        ),
                       box(width = 6,
                           selectInput("AucUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in"),
                           numericInput(inputId = "AuCWidth", label = "Width of downloaded graphic", value = 9),
                           numericInput(inputId = "AuCHeight", label = "Height of downloaded graphic", value = 5)),
                       box(width = 6,
                           downloadButton("downloadgraphAuC", "Download Area under the Curve Graphic"),
                           br(), br(),
                           downloadButton("downloadstatsAuC", "Download Area under the Curve Statistical Testing")),
                       box(width = 12, div(style = 'overflow-y: scroll', tableOutput("resAuCstatsbreakdown")))
                      )))
        ),##End of score_curve
tabItem(tabName = "heatmaps",
        fluidRow(conditionalPanel(condition = "output.fileUploaded",
            column(8,
               box(title = "Unaligned Heatmap", width = 12, status = "primary",
                   p("This clustering algorithm uses Manhattan distance to group similar score progression patterns.
                     A heirarchical relationship between mouse disease progression is shown on the left of the heatmap by a dendogram.
                     The id of the mouse is shown to the right, and all other variables are shown on the left as indicadated by the key.
                     The score is shown by the color of each scale as indicated by gradient scale in the legend.
                     For further statistical modeling analysis, go to ANOVA Analysis tab located in the navigation bar on the left."),
                   imageOutput("UnalignedHeatmap"),
                   column(6, checkboxInput(inputId = "mouseIDUnaligned", label = "Remove Mouse IDs?", value = F),
                          downloadButton("downloadgraphUnalignedHeatmap", "Download Heatmap Image")),
                   column(6, numericInput(inputId = "cuttreeUnaligned", label = "How many sections should the heatmap be split into?", value = 4))),
               box(title = "Aligned Heatmap", width = 12, status = "primary",
                   p("This clustering algorithm uses Manhattan distance to group similar score progression patterns, but aligns the mice by their first day of symptoms.
                   Thus, slight discrepancies in disease onset can be controled for and mitigated in some statistical modeling.
                     A heirarchical relationship between mouse disease progression is shown on the left of the heatmap by a dendogram.
                     The id of the mouse is shown to the right, and all other variables are shown on the left as indicadated by the key.
                     The score is shown by the color of each scale as indicated by gradient scale in the legend.
                     Note this function will NOT work without dropping 'zeros' on the data upload page, as zeros never have a day of symptom onset."),
                   imageOutput("AlignedHeatmap"),
                   column(6,
                          checkboxInput(inputId = "mouseIDAligned", label = "Remove Mouse IDs?", value = F),
                          downloadButton("downloadgraphAlignedHeatmap", "Download Heatmap Image")),
                   column(6,
                          numericInput(inputId = "cuttreeAligned", label = "How many sections should the mice be grouped into?", value = 4)))
               ),
               box(title = "Heatmap Colors", width = 4,
                   #selectInput("pheatmapcolorscheme", label = "What color scheme for the EAE Score?", choices = c("base", "rainbow", "heat.colors", "cm.colors",  "topo.colors"), selected = "base"),
                   uiOutput("Unalignedcolors")),
               box(title = "Graphic Export Size",width = 4, height = 300,collapsible = T,
                   selectInput("HeatmapUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in"),
                   numericInput(inputId = "HeatmapWidth", label = "Width of downloaded graphic", value = 9),
                   numericInput(inputId = "HeatmapHeight", label = "Height of downloaded graphic", value = 5))

        ))), ##End of heatmaps
tabItem(tabName = "histogram",
        tags$details(tags$summary(strong("Introduction to the High Scoring Histogram")), "The high scoring histogram is a tool useful for looking at the
                     severity reached in disease. By looking at the number of days above a certain score threshold, groups with different scoring
                     frequencies can be separated out. This technique is also useful for checking for outliers which may be driving separation in the
                     score curve."), br(), fluidRow(conditionalPanel(condition = "output.fileUploaded",
        box(width =12, status = "primary", title="High Scoring Histogram", imageOutput("HighScoreHist")),
        column(6,
        box( width = 12, height = 300, uiOutput("HighHistGroup"), uiOutput("HighHistColor"), uiOutput("VariableTitles")),
        box(width = 12,height = 300,
            numericInput(inputId = "HistWidth", label = "Width of downloaded graphic", value = 9),
            numericInput(inputId = "HistHeight", label = "Height of downloaded graphic", value = 5),
            selectInput("HistUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in"),
            downloadButton("downloadgraphHist", "Download Graphic")),
        box(title = "Count Modeling without Interactions", width =12, "The disperison testing below checks to see if the data is a proper fit for Poisson modeling. If the p-value is less than 0.05, the Poisson model is not correct for the data, and
            we recommend switching to a different the model system.", br(), verbatimTextOutput("modelindoverdisp"),

            selectInput(inputId = "PoiNBInd", label = "Poisson or Negative Binomial Modeling?", choices = c("Poisson", "Negative Binomial"), selected = "Poisson"),
            verbatimTextOutput("modelind"))
        ),
        column(6,
        box(width = 12, height = 300, numericInput(inputId = "scorethreshold", label = "What Threshold Score (>=)?", value = 2), uiOutput("Histcolors")),
        box(title = "Count-Based Data", width =12, height = 300, "Count-based data looks at the frequency of an event (e.g. number of times people go to the store,
            number of days a mouse is scored over a certain threshold). These events have a rate that may change overtime or be different
            between experimental groups. Count based-data can be characterized by its whole number data, a skewed distribution,
            and sparsity. In this application, we have built in two types of tests for this data, Poisson and Negative Binomial, for a model
            without interactions and a model with interactions."
            ),
        box(title = "Count Modeling with Interactions", width =12, "The disperison testing below checks to see if the data is a proper fit for Poisson modeling. If the p-value is less than 0.05, the Poisson model is not correct for the data, and
            we recommend switching to a different the model system.", br(), verbatimTextOutput("modelintoverdisp"),
            selectInput(inputId = "PoiNBInt", label = "Poisson or Negative Binomial Modeling?", choices = c("Poisson", "Negative Binomial"), selected = "Poisson"),
            verbatimTextOutput(("modelint")),),
        ),

        ))), ## End of histogram
tabItem(tabName = "anova",
        tags$details(tags$summary(strong("What is this page?")), "On this page you can find ANOVA modeling that looks at each variable and
                     their interactions. You can deselect terms to include or exclude at the top. Two models are ran, one looking at the raw data and
                     one looking at the same model when applied to data when the day on symptoms onset is aligned (see aligned heatmap for more info)."),
        br(), br(),
        fluidRow(conditionalPanel(condition = "output.fileUploaded",
            box(title = "ANOVA Testing", width =12, "ANOVA testing has become one of the most commonly used statistical tests due to its simple structure and ability to handle multiple variables.", br(),br(),
            "This application runs two ANOVAs with your data. The first two-way ANOVA is one with the raw provided data looking at symptoms in relation to day of EAE induction. The second two-way ANOVA corrects for day of disease onset by aligning the mice’s disease progression to the first day of symptom onset.", br(), br(),
            "In both of these model, the day is converted to a variable and should always be significant as EAE inherently changes in score over time.
            Similarly any interaction terms with day should be ignore (an interaction is indicated by a : separating two variables).", br(), br(),
            "If significance is indicated, then the model is reporting that the variable being looked at causes an overall change in scores.
            If there is an interaction present, it take precedence over main effects.", br(), br(),
            "We are working on adding linear mixed effect modeling, so continue to check back for the next update."),
        box(width=12, status = "primary", uiOutput("VariableTitlesANOVA"), strong("Model without Interactions"), verbatimTextOutput("modelgroupind"),
            strong("Model with Interactions"), verbatimTextOutput("modelgroupdep"))
        ))),## End of anova
tabItem(tabName = "ind_mouse",
        tags$details(tags$summary(strong("What is this page?")),
                     "This page shows each mouse as an individual line and is still in development. This graph is mainly useful to demonstrate
                     the deviation of mouse data and  for researchers to look at all of their mice quickly. You can even select a single
                     mouse by typing its ID in the selection box once you turn on the 'Search for Individual Mouse' setting. You can also change
                     the group by which the grids (faceting) are set up."),
        fluidRow(conditionalPanel(condition = "output.fileUploaded",
        box(width =12, status = "primary", plotOutput("ShadowScoreCurve")),
        box(width = 6, uiOutput("ShadowCurveGroup"), checkboxInput(inputId = "IndividualMouse", label = "Search for Individual Mouse?", value = F), conditionalPanel(condition = "input.IndividualMouse == true", textInput(inputId = "MouseSearch", label = "What mouse do you want to see?"))),
        box( width = 6, uiOutput("ShadowCurvegrouping"), uiOutput("ShadowCurvecolors"))
       ))),## End of ind_mouse
tabItem(tabName = "weight_sheet",
        tags$details(tags$summary(strong("What is this page?")), "On this page you can find the processed input from the optinal weight .csv file upload on the left. This is really useful for finding entry errors and for seeing which mice were dropped from the study (if “Drop Zeros” is selected in the left menu bar these mice’s rows will be highlighted in red). Be sure to indicate if the variable columns are also in the weight.csv file! Also make sure the mouse IDs are in the same order as the score file to guarantee the correct mice are being dropped!"),
        conditionalPanel(condition = "output.fileUploaded", div(style = 'overflow-x: scroll', DT::dataTableOutput("weightsheet"))
        )),## End of weight_sheet
tabItem(tabName = "weight_graph",
        tags$details(tags$summary(strong("What is this page?")), "On this page you can graph the mice's weights overtime by both percent weight and measured weight."),
        conditionalPanel(condition = "output.fileUploaded", fluidRow(
        box(width=12, plotOutput("percentWeightGraph")),
        box(width=4, selectInput(inputId = "weightmetric", label = "Use raw weight values or percent weight change?", choices = c("Weights", "Percent Weights"), selected = "Percent Weights"),
               uiOutput("WeightGroup"),
               selectInput(inputId = "WeightPLabels", label = "Mark significance using which?", choices = c("p-value" = "p", "adjusted p-value" = "p.adj"), selected = "p"),
               uiOutput("Weightgrouping"), checkboxInput("showadvancesettingsWeights", "Show advance graphing settings?", value = FALSE)),
        box(width=4, uiOutput("colorsweight"), conditionalPanel(condition = "input.showadvancesettingsWeights == true",
               textInput(inputId = "Weightxlab", label = "What do you want the x axis label to be?", value = "Day"),
               textInput(inputId = "Weightylab", label = "What do you want the y axis label to be?", value = NA),
               numericInput(inputId = "WeightLineThickness", label = "Line Thickness", value = 0.5),
               numericInput(inputId = "WeightLegendText", label = "Legend Size", value = 8),
               numericInput(inputId = "Weightxaxistext", label = "X Axis Text Size", value = 8),
               numericInput(inputId = "WeightLabels", label = "Axis Labels Text Size", value = 14),
               numericInput(inputId = "WeightPointSize", label = "Point Size", value = 1))),
        box(width=4, uiOutput("shapesweight"), conditionalPanel(condition = "input.showadvancesettingsWeights == true",
               numericInput(inputId = "WeightStar1", label = "Distance Between Error Bar and Star 1", value = 0.5 ),
               numericInput(inputId = "WeightStar2", label = "Distance Between Error Bar and Star 2", value = 1 ),
               numericInput(inputId = "WeightStar3", label = "Distance Between Error Bar and Star 3", value = 1.5 ),
               numericInput(inputId = "WeightStarSize", label = "Size of Significance Stars", value = 6),
               checkboxInput("weightBaseline", "Show a horizontal line at 0 for percent weight graphs?", value = T))),
        box(width=4,
               selectInput("WeightUnits", label = "What Units of Measure for Height and Width?", choices = c("in", "px"), selected = "in")),
        box(width=4, numericInput(inputId = "WeightWidth", label = "Width of downloaded graphic", value = 9),
               numericInput(inputId = "WeightHeight", label = "Height of downloaded graphic", value = 5),
            downloadButton("downloadgraphWeight", "Download Weight Graphic"), br(),
            downloadButton("downloadstatsWeight", "Download Statistical Testing by Day")),
        box(title = "Statistical Testing Results", width=12, tableOutput("weightstatstable"))
        ))),## End of weight_graph
tabItem(tabName = "tuitorials",
        fluidRow(
           box(title="Expliantion of Test Data", width=6,status="primary",
               "The test data provided with this analysis suite was originally designed for a lecture at UT Austin's Institute of Neuroscience.
               As our lab is interested in alcohol's effect in EAE, we decided to make sample data that originated from fictional alcohol-related experiments.
               Test data set 1 compares red and white wine's effect. Test data set 2 compares 5% Rum to 20% Rum. Test data set 3 compares hot and cold Sake.", br(),
               br()),
           box(title="Video Guide", width=6,status="primary",
               "A video guide walking through the application is coming soon!"),
           box(title="Have Other Questions?", width=6,status="primary",
               "If you have any questions, concerns, or potential features to add, you can fill out this",
               tags$a(href="https://utexas.qualtrics.com/jfe/form/SV_d5VBqQDfC89Mz1r", "survey"))
        )),## End of tuitorials
tabItem(tabName = "faqs",
        fluidRow(
        box(title = "Frequently Asked Questions", width =12, status = "primary",
        "Thank you for your patience as we continue to develop this application and support its users!",
        "If you have any questions, concerns, or potential features to add, you can fill out this",
        tags$a(href="https://utexas.qualtrics.com/jfe/form/SV_d5VBqQDfC89Mz1r", "survey"),
        " and we will get back to you!", br(), br(),
        strong("Where are all the analyses?"),br(), "They pop up once you provide data! This is to prevent people from moving too far
        out of order and streamline users in the proper direction! If you just want to play with some test data there is an option on the data upload portal
        to load in your choice of three different test/practice data sets.", br(), br(),
        strong("How should I choose if my data is parametric or non-parametric?"), br(), "Choosing between parametric and non-parametric
        has been a long standing problem with no simple solution in longitudinal data. We recommend referencing a general statistical
        resource for more information
        on choosing between tests. (e.g. Robert Sokal and F. James Rohlf's Biometry: The Principles and Practices of Statistics in Biological Research or
        Jerrold Zar's Biostatistical Analysis)",
        br(), br(),
        strong("When should I use p-value corrections and when shouldn't I?"), br(),
        "This is another question that has troubled EAE analysis for a while. Since each day's score is not independent of
        other days' scores, each day is not an independent variable. However, correcting for this is statisitically complicated. One option
        is to use p-value corrections to attempt to correct for the number of comparisons; however, this is often too conservative of an approach
        and results in no statistical significance in biologically significant differences.
        The standard in the field is to either A) run a statistical test on each day and treat them as if they were indepdent (usually not using p-value corrections) or
        B) use area under the curve to run only a single comparison on the selected days of the curve as a whole. While definitely not a
        perfect solution, we feel the best approach is to use a multifaceted suite of tools to find significance. Each test has assumptions,
        but by using a variety of tests, the effects of this assumptions can be minimized.",
        br(), br(),
        strong("Can I change the cut-off threshold for significance markings?"), br(),
        "Right now this isn't an option in the Analysis Suite. It currently uses the standardized system of *: p<=0.05, **: p<=0.01, and ***: p<=0.001.", br(),
        br(),
        strong("What type of colors can I use?"), br(),
        "This application can take any common color name native in R (see list from Jenny Bryan",
        tags$a(href="https://www.stat.ubc.ca/~jenny/STAT545A/r.col.white.bkgd.pdf", "here"),
        ") or any hex color format (e.g. #ffffff is white, #000000 is black, and #BF5700 is burnt orange, hook em')",
        br(), br(),
        strong("What does the colon mean in variable selection?"),
        br(),
        "The colon between varaiables means looking at the interaction between two variables (e.g. A treatment works and you might
        see overall efficacy, but it is driven by the males.).",
        br(), br(),
        strong("Where can I find an example paper for how these tools are used in publication?"), br(),
        "To see how these methods and data are used in publications we suggest looking at some of the following papers:",
        br(),
        "Caslin B, Maguire C, Karmakar K, Mohler K, Wylie D, Melamed E (2019) Alcohol shifts gut microbial networks and ameliorates a murine model of neuroinflammation in a sex-specific pattern. Proc Natl Acad Sci U S A. doi:10.1073/pnas.1912359116."
        ),

)),## End of faqs
tabItem(tabName = "citation", fluidRow(
        box(width =12, status = "primary", "Created and managed by Cole Maguire in the Melamed Lab at the Dell Medical School's Department of Neurology, the University of Texas at Austin.
             You can check out some of our other work on the", tags$a(href="http://sites.utexas.edu/melamed-lab/home/", "Melamed Lab website."),
            br(), br(),
            "If you want to contact us about any questions, concerns, or potential features to add, you can fill out this",
            tags$a(href="https://utexas.qualtrics.com/jfe/form/SV_d5VBqQDfC89Mz1r", "survey"),
            " and we will get back to you!"),
        box(title = "Packages Used", width = 6, status = "primary", HTML(paste("shiny", "shinydashboard", "ggplot2", "ggpubr",
                        "dplyr", "tidyr", "DT", "tibble", "lmerTest", "pheatmap", "gtools", "cowplot",
                        "MASS", "AER", sep="<br/>"))),## End of package list box
        box(title = "How to Cite This Application", status = "primary", width = 6, "Maguire C, Caslin B, Bazzi S, Wylie D, Melamed E,
             A new software and analysis suite for experimental autoimmune encephalomyelitis, MSVirtual 2020."),
        box(title = "GitHub repository", status = "primary", width =6, "You can find the code used to run this Shiny application on GitHub", tags$a(href="https://github.com/colemag/EAEScore", "here!")),
        box(title = "Version 1.0.1 Notes", width = 6, status = "primary", "This is the inital build of the EAE Analysis Suite. As updates are made, developer notes will be tracked here.")
        ))## End of citation
            )## End of Tab Items
        )## End of Body
    )## End of ui

