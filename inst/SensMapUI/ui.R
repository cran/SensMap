library(shiny)
library(DT)
library(shinydashboard);
library(googleVis);
require(pastecs);
library(plotly)
library(shinyBS)
library(shinyjs)
library(FactoMineR)
library(factoextra)
library(MASS)
library(fields)
library(plotly)
library(mgcv)
library(MCMCpack)
library(glmulti)
library(plyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(shiny)
library(googleVis)
library(multcomp)
library(sjPlot)
library(scales)
library(doBy)
library(gridExtra)
library(agricolae)
library(devtools)
library(magrittr)
library(ggbiplot)
library(ggdendro)
library(dendextend)
library(grid)
#devtools::install_github("vqv/ggbiplot")
#install.packages("ggdendro")
#install.packages("pastecs")
library(pastecs)

shinyUI(bootstrapPage(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "SensMapGUI: Easy web-tool for consumer and sensory data mapping",
                    titleWidth = 800),
    dashboardSidebar(width = 300,

                     sidebarMenu(
                       sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                         label = "Search..."),

                       menuItem("Upload Data", icon = icon("upload"), tabName = "sensorydata"),

                       menuItem("Sensory Data Analysis", tabName = "statdesc", icon = icon("file-text-o")),

                       menuItem("Consumer Data Analysis", tabName = "statdesc1", icon = icon("file-o")),

                       menuItem("Internal Preference Mapping",icon = icon("object-group"), tabName = "intermap"),

                       menuItem("External Preference Mapping",icon = icon("random"), tabName = "epm"),

                       menuItem("Consumer preference prediction",icon = icon("commenting"), tabName = "resmod"),

                       menuItem("Smoothing of External Preference Mapping",icon = icon("map-signs"), tabName = "epmsmooth"),

                       menuItem("Stability of External Preference Mapping", tabName = "compmap", icon = icon("cubes"))
                     )
    ),

    dashboardBody(
      shinyjs::useShinyjs(),
      tags$script(HTML("$('body').addClass('skin-blue sidebar-mini');")),
      tags$head(tags$style(HTML('
                                .main-header .logo {
                                font-family: "Georgia", Times, "Times New Roman", serif;
                                font-weight: bold;
                                font-size: 20px;
                                }
                                '))),

      tabItems(
        tabItem(tabName = "sensorydata",
                fluidPage(tabBox(width=12,
                                 tabPanel(h4(tags$b("Data")),
                                          fluidPage(
                                            tabBox(width=12,
                                                   id = "sensorydatatab",
                                                   tabPanel(h4("Sensory Data"),
                                                            fileInput('rawInputFile','Upload Data File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                            column(3,uiOutput("assessor")),
                                                            column(3,uiOutput("replication")),
                                                            column(3,uiOutput("product")),
                                                            checkboxInput('headerUI','Header',TRUE),
                                                            radioButtons('sepUI','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Semicolon'),
                                                            radioButtons('decUI','decimal',c(Comma=',',point='.'),'point'),
                                                            radioButtons('quoteUI','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote') ),

                                                   tabPanel(h4("Consumer Data"),
                                                            fileInput('consInputFile','Upload Data File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                            uiOutput("labelconsUI"),
                                                            checkboxInput('headUI','Header',TRUE),
                                                            radioButtons('sepaUI','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Semicolon'),
                                                            radioButtons('deccUI','decimal',c(Comma=',',point='.'),'point'),
                                                            radioButtons('quotUI','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote')),

                                                   tabPanel(h4("Physico-Chemical Data"),
                                                            fileInput('physicalInputFile','Upload Data File',accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                                            uiOutput("labelphyUI"),
                                                            checkboxInput('headUI_phy','Header',TRUE),
                                                            radioButtons('sepaUI_phy','Seperator',c(Comma=',',Semicolon=';',Tab='\t'),'Semicolon'),
                                                            radioButtons('decUI_phy','decimal',c(Comma=',',point='.'),'point'),
                                                            radioButtons('quotUI_phy','Quote',c(None='','Double Quote'='"','Single Quote'="'"),'Double Quote'))



                                            ))
                                 ),

                                 tabPanel(h4(tags$b("View Data")),
                                          fluidPage(
                                            tabBox(width=12,
                                                   id = "phydata",
                                                   tabPanel(h4("Sensory Data"),  div(style = 'overflow-x: scroll', dataTableOutput('pre.data'))),
                                                   tabPanel(h4("Consumer Data"),  div(style = 'overflow-x: scroll', dataTableOutput('cons.data'))),
                                                   tabPanel(h4("Physico-Chemical Data"),  div(style = 'overflow-x: scroll', dataTableOutput('phy.data')))))

                                 ),

                                 tabPanel(h4(tags$b("Summary Tables")),
                                          fluidPage(
                                            tabBox(width=12,
                                                   id = "statdesc",
                                                   tabPanel(h4("Average table by product"),div(style = 'overflow-x: scroll',dataTableOutput('av')),
                                                            downloadButton("downloadav", "Download Table",class = "butt")),
                                                   tabPanel(h4("Average table by product and assessor"),div(style = 'overflow-x: scroll',dataTableOutput('av.table2')),
                                                            downloadButton("downloadavt", "Download Table",class = "butt"))

                                            )
                                          ))

                ))
        ),


        tabItem(tabName = "statdesc",
                fluidPage(
                  tabBox(width=12,
                         tabPanel(h4(tags$b("Descriptive Analysis")),width=12,
                                  fluidRow(
                                    tabBox(width=12,
                                           id = "char",
                                           tabPanel("Boxplot",
                                                    box( title = tags$b("Inputs"), status = "primary",solidHeader = TRUE,collapsible = TRUE,
                                                         uiOutput("gdesc"),
                                                         uiOutput("gfactor"),
                                                         htmlOutput("jitter"),
                                                         tags$hr(),
                                                         width = 4,
                                                         # uiOutput("down"),
                                                         selectInput(
                                                           inputId = "downloadPlotType",
                                                           label   = "Select download file type:",
                                                           choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
                                                         # Allow the user to set the height and width of the plot download.
                                                         # h5(HTML("Set download image dimensions(units are inches for PDF, pixels for all other formats)")),
                                                         numericInput(
                                                           inputId = "downloadPlotHeight",label = "Height (inches)",value = 7,min = 1,max = 100),
                                                         numericInput(
                                                           inputId = "downloadPlotWidth",label = "Width (inches)",value = 10,min = 1,max = 100),
                                                         #Choose download filename.
                                                         textInput(
                                                           inputId = "downloadPlotFileName",
                                                           label = "Enter file name for download"),
                                                         downloadButton(
                                                           outputId = "downloadPlot",
                                                           label    = "Download Plot",class = "butt"),
                                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))

                                                    ),
                                                    box( title = tags$b("Boxplots"),solidHeader = TRUE,collapsible = TRUE,  status = "primary",width = 8 , plotOutput("plotA", height = 500))
                                           ),
                                           tabPanel("Boxplotly",
                                                    box( title = tags$b("Inputs"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=4,
                                                         uiOutput("variab"),
                                                         uiOutput("Vfactor")),
                                                    box( title = tags$b("Boxplotly"),width=8,solidHeader = TRUE,collapsible = TRUE,  status = "primary",  plotlyOutput("plotB", height = 500))

                                           ),
                                           tabPanel("Line Chart",
                                                    box(title=tags$b("Inputs"),status="primary",solidHeader=TRUE,collapsible=TRUE,
                                                        uiOutput("vlines"),
                                                        uiOutput("flines"),
                                                        tags$hr(),width=4),
                                                    box(title=tags$b("Lines"),width=8,solidHeader = TRUE,collapsible = TRUE,  status = "primary",  htmlOutput("plotD",height = 500)
                                                    )),
                                           tabPanel("Bubble Chart",
                                                    box(title=tags$b("Inputs"),status="info",solidHeader=TRUE,collapsible=TRUE,
                                                        uiOutput("vbubble"),
                                                        uiOutput("vbubble1"),
                                                        uiOutput("f"),
                                                        uiOutput("f2"),
                                                        uiOutput("f1"),
                                                        width=3),
                                                    box(title=tags$b("Bubbles"),width=8,solidHeader = TRUE,collapsible = TRUE,  status = "primary",  htmlOutput("plotE")
                                                    )),
                                           tabPanel("Motion Chart",
                                                    box(title=tags$b("Motion"),width=10,solidHeader = TRUE,collapsible = TRUE,  status = "primary",  htmlOutput("plotF",height = 500))


                                           )
                                    ))
                         ),
                         tabPanel(h4(tags$b("Anova of factors effect")),width=12,
                                  box( title = tags$b("Anova"),solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 6,height = 550,
                                       uiOutput("dependant"),
                                       uiOutput("independants"),
                                       tags$hr(),
                                       verbatimTextOutput("anov"),
                                       br(),
                                       actionButton("tabBut", tags$b("Download Table"), icon("cog"),
                                                    style="color: #fff; background-color: darkcyan; border-color: #2e6da4"),
                                       bsModal("modalExample", tags$b("ANOVA Table"), "tabBut", size = "large",
                                               dataTableOutput("anov.d"),downloadButton("downloadtable", "Download Table",class = "butt"),
                                               tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))
                                  ),
                                  box( title = tags$b("Tukey HSD"),solidHeader = TRUE,collapsible = TRUE,  status = "primary", width = 6,height = 550,
                                       plotOutput("tukeyplot"),
                                       actionButton("tabBut1", tags$b("View Results"), icon("cog"),
                                                    style="color: #fff; background-color: darkcyan; border-color: #2e6da4"),
                                       downloadButton(
                                         outputId = "downloadtuk",
                                         label    = "Download Plot",class = "butt"),
                                       tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                       bsModal("modalExample1", tags$b("Tukey result"), "tabBut1", size = "large",
                                               div(style = '
                                                   height: 400px;
                                                   overflow-y: scroll;', verbatimTextOutput("tukey"))) )
                                               ),

                         tabPanel(h4(tags$b("PCA on Sensory Data")),width=12,
                                  fluidPage(
                                    tabBox(width=12,
                                           id = "pcasenso",
                                           tabPanel(h4("Screeplot"),
                                                    box(title = tags$b("Screeplot"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,
                                                        plotOutput("scree", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot2",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
                                                    box(title = tags$b("Eigen Values"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,
                                                        tableOutput("stable"),downloadButton("downloadstab", "Download Table",class = "butt"))),

                                           tabPanel(h4("Correlation circle"),
                                                    box(title = tags$b("Correlation circle"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("cercle", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot1",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
                                                    )),
                                           tabPanel(h4("Graph of individuals"),
                                                    box(title = tags$b("Graph of individuals"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("ind", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot3",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))),
                                           tabPanel(h4("Biplot"),
                                                    box(title = tags$b("Biplot"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("bi", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot4",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))))

                                    ))
                         )

                                               ))
                ),

        tabItem(tabName = "statdesc1",
                fluidPage(
                  tabBox(width=12,

                         tabPanel(h4(tags$b("PCA on Consumer Data")),width=12,
                                  fluidPage(
                                    tabBox(width=12,
                                           id = "pcacons",
                                           tabPanel(h4("Screeplot"),
                                                    box(title = tags$b("Screeplot"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,
                                                        plotOutput("screec", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot6",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
                                                    box(title = tags$b("Eigen Values"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,
                                                        tableOutput("stablec"),downloadButton("downloadstab1", "Download Table",class = "butt"))),

                                           tabPanel(h4("Correlation circle"),
                                                    box(title = tags$b("Correlation circle"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("cerclec", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot5",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))),

                                           tabPanel(h4("Graph of individuals"),
                                                    box(title = tags$b("Graph of individuals"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("indc", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot7",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))),
                                           tabPanel(h4("Biplot"),
                                                    box(title = tags$b("Biplot"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=10,
                                                        plotOutput("bic", height = 380),
                                                        downloadButton(
                                                          outputId = "downloadPlot8",
                                                          label    = "Download Plot",class = "butt"),
                                                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))))

                                    ))
                         ),
                         tabPanel(h4(tags$b("Classification")),width=12,
                                  box(title = tags$b("Dendrogram"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,height = 550,
                                      plotOutput("dend"),
                                      actionButton("tabBut2", tags$b("View Tree"), icon("cog"),
                                                   style="color: #fff; background-color: darkcyan; border-color: #2e6da4"),
                                      bsModal("modal", tags$b("Tree"), "tabBut2", size = "large",
                                              plotOutput("dend1"),downloadButton("downloaddend", "Download plot",class = "butt"),
                                              tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))),
                                      actionButton("tabBut3", tags$b("Results"), icon("cog"),
                                                   style="color: #fff; background-color: darkcyan; border-color: #2e6da4"),
                                      bsModal("modal1", tags$b("Class"), "tabBut3", size = "large",
                                              div(style = '
                                                  height: 400px;
                                                  overflow-y: scroll;', verbatimTextOutput("dend12")))
                                              ),
                                  box(title = tags$b("Clusters"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=6,height = 550,
                                      plotOutput("clus"), downloadButton("downloadclus", "Download plot",class = "butt"),
                                      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")))
                                              )

                  ))
      ),

      tabItem(tabName = "intermap",
              box(title = tags$b("Internal preference mapping"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=12,
                  uiOutput("interval"),
                  actionButton("actioncarto", tags$b("Run Analysis"),
                               icon("gears"),
                               style="color: #fff; background-color: steelblue; border-color: #2e6da4"),
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   h4("working..."),
                                   h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                  )
                  ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                      plotOutput("cart"), downloadButton(
                                        outputId = "downloadPlotinternal",
                                        label    = "Download Plot",class = "butt"),
                                      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
                  )
              )
      ),


      tabItem(tabName = "epm",

              box(title = tags$b("Parameters"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=4,
                  height = 610,background = "navy",
                  uiOutput("Dimension_var"),uiOutput("Prediction_var"),uiOutput("mod1"),uiOutput("formula_lm1"),
                  uiOutput("par_var"),tags$hr(),uiOutput("interval2"),actionButton("actioncarto13", tags$b("Run Analysis"),
                                                                                   icon("gears"),
                                                                                   style="color: #fff; background-color: steelblue; border-color: #2e6da4")),
              box(title = tags$b("Mapping"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=8,height = 610,
                  fluidRow( tabBox(width=12,
                                   tabPanel(h4("External mapping"),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             h4("working..."),
                                                             h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                                            )
                                            ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                                plotOutput("epmplot"),downloadButton(
                                                                  outputId = "downloadPlotexternal",
                                                                  label    = "Download Plot",class = "butt")
                                            )
                                   ),
                                   tabPanel(h4("Prediction Surface"),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             h4("working..."),
                                                             h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                                            )
                                            ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                                plotOutput("epmplot2"),downloadButton(
                                                                  outputId = "downloadPlotpredsurf",
                                                                  label    = "Download Plot",class = "butt")
                                            )
                                   ),
                                   tabPanel(h4("External mapping 3D"),
                                            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                             h4("working..."),
                                                             h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                                            )
                                            ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                                                plotlyOutput("epmplot3")
                                            )
                                   )

                  )))
      ),

      tabItem(tabName = "resmod",
              box(title = tags$b("Parameters"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=4,
                  height = 600,background = "navy",
                  uiOutput("Dimension_var1"),br(),uiOutput("Prediction_var1"),br(),
                  uiOutput("aic"),br(),actionButton("actioncarto14", tags$b("Run Rnalysis"),icon("gears"),
                                                    style="color: #fff; background-color: steelblue; border-color: #2e6da4")),
              box(title = tags$b("Mapping"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=8,height = 600,
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   h4("working..."),
                                   h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                  )
                  ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                      plotOutput("model_selection"),
                                      downloadButton(
                                        outputId = "downloadPlot_aic",
                                        label    = "Download Plot",class = "butt"),
                                      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
                  )
              )),

      tabItem(tabName = "epmsmooth",
              box(title = tags$b("Parameters"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=4,
                  height = 610,background = "navy",
                  uiOutput("Dimension_var3"),uiOutput("Prediction_var3"),uiOutput("mod2"),uiOutput("formula_lm2"),
                  uiOutput("par_var3"),tags$hr(),uiOutput("interval3"),actionButton("actioncarto15", tags$b("Run Rnalysis"),icon("gears"),
                                                                                    style="color: #fff; background-color: steelblue; border-color: #2e6da4"
                  )),
              box(title = tags$b("Mapping"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=8,height = 610,
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   h4("working..."),
                                   h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                  )
                  ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                      plotOutput("smooth"),plotOutput("smooth_lo"),
                                      downloadButton(
                                        outputId = "downloadPlotsmooth",
                                        label    = "Download Plot",class = "butt")
                  )
              )
      ),

      tabItem(tabName = "compmap",
              box(title = tags$b("Parameters"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=4,
                  height = 600,background = "navy",
                  uiOutput("reduction"),uiOutput("Prediction_com"),uiOutput("formula_lm"),uiOutput("formula_gam"),
                  uiOutput("numm"),br(),actionButton("actioncarto16", tags$b("Run Rnalysis"),icon("gears"),
                                                     style="color: #fff; background-color: steelblue; border-color: #2e6da4")),
              box(title = tags$b("Mapping"), status = "primary",solidHeader = TRUE,collapsible = TRUE,width=8,height = 600,
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   h4("working..."),
                                   h2(HTML('<i class="fa fa-cog fa-spin fa-2x"></i>'))
                  )
                  ,  conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                      plotOutput("comparaison"),
                                      downloadButton(
                                        outputId = "downloadPlot_comp",
                                        label    = "Download Plot",class = "butt"),
                                      tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}"))
                  )
              )

      )

        ))

      ))
)
