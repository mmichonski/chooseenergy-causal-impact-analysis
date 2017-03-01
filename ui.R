#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
#          Causal Impact Analysis for Plan Changes       #
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#




library(markdown)
library(DT)
library(shiny)
library(shinydashboard)
library(lubridate)
library(RColorBrewer)
library(gplots)
library(data.table)
library(plotly)
library(ggplot2)
library(grid)
library(gtable)
library(shinyjs)




dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='http://image.go.chooseenergy.com/lib/fe9a1372756c047c76/m/1/trigger-renewal_reminder-stacked_logo-black-242x124-01.png',
                                           tags$img(src='http://image.go.chooseenergy.com/lib/fe9a1372756c047c76/m/1/trigger-renewal_reminder-stacked_logo-black-242x124-01.png',height='40',width='85'))

dashboardPage(skin = "black",
              dbHeader,
              dashboardSidebar(disable = TRUE),
              dashboardBody(
                  tags$head(
                      tags$style(HTML("
                                      @import url('https://fonts.googleapis.com/css?family=Assistant:700');
                                      ")
                      )
                      ),
                  h2("Causal Impact Analysis", align = "center", style = "font-family: 'Assistant', sans-serif; color:black"),
                  h2(" "),
                  column(width = 10, offset = 1,
                         h4("  This app calculates all of the plan changes on Choose Energy's site and compares them with changes in lead sales volume and conversion rate. It then picks out the ten largest impacts on lead volume and determines the most likely causes, whether it be a  plan price change, traffic change, or multiple possibilities.", align = "left"),
                         h4("Beneath the causal impact analysis are screenshots of another feature of the app. These interactive, filterable plots allow direct access to the data in order to zoom in on the specific performance of a given plan, utility zone, or price change. However, only screenshots are shown for data security reasons.", align = "left"),
                         fluidRow(
                         h2(" ")
                         )
                         ),
                  h2(" "),
                  h2(" "),
                  tabBox(
                      title = NULL,
                      id = "impact.tabset", width = 12,
                      
                      # Tab 1 
                      
                      tabPanel("Impact 1",
                               h2(" "),
                               fluidRow(
                                      infoBoxOutput("change.1"),
                                      infoBoxOutput("utility.1"),
                                      infoBoxOutput("product.1")
                                      ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                      h3(textOutput("plan_name1.tab")),
                                      h4(textOutput("plan_term1.tab")),
                                      h4(textOutput("plan_supplier1.tab")),
                                      h4(textOutput("plan_active1.tab")),
                                      h4(textOutput("plan_id1.tab")),
                                      h2(textOutput("price_change1")),
                                      h4(" ")
                               ),
                                    box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                        textOutput("likelihood.description.1")
                               )
                               ),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot1"),
                                          verbatimTextOutput("causal.impact.cr.summary1")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot1"),
                                          verbatimTextOutput("causal.impact.leads.summary1"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes1"),
                                          plotOutput("plan_breakdown1", height = "600px"),
                                          imageOutput("top.image")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 2 
                      
                      tabPanel("Impact 2",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.2"),
                                   infoBoxOutput("utility.2"),
                                   infoBoxOutput("product.2")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name2.tab")),
                                       h4(textOutput("plan_term2.tab")),
                                       h4(textOutput("plan_supplier2.tab")),
                                       h4(textOutput("plan_active2.tab")),
                                       h4(textOutput("plan_id2.tab")),
                                       h2(textOutput("price_change2")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.2")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot2"),
                                          verbatimTextOutput("causal.impact.cr.summary2")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot2"),
                                          verbatimTextOutput("causal.impact.leads.summary2"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes2"),
                                          plotOutput("plan_breakdown2", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 3 
                      
                      tabPanel("Impact 3",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.3"),
                                   infoBoxOutput("utility.3"),
                                   infoBoxOutput("product.3")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name3.tab")),
                                       h4(textOutput("plan_term3.tab")),
                                       h4(textOutput("plan_supplier3.tab")),
                                       h4(textOutput("plan_active3.tab")),
                                       h4(textOutput("plan_id3.tab")),
                                       h2(textOutput("price_change3")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.3")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot3"),
                                          verbatimTextOutput("causal.impact.cr.summary3")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot3"),
                                          verbatimTextOutput("causal.impact.leads.summary3"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes3"),
                                          plotOutput("plan_breakdown3", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 4 
                      
                      tabPanel("Impact 4",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.4"),
                                   infoBoxOutput("utility.4"),
                                   infoBoxOutput("product.4")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name4.tab")),
                                       h4(textOutput("plan_term4.tab")),
                                       h4(textOutput("plan_supplier4.tab")),
                                       h4(textOutput("plan_active4.tab")),
                                       h4(textOutput("plan_id4.tab")),
                                       h2(textOutput("price_change4")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.4")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot4"),
                                          verbatimTextOutput("causal.impact.cr.summary4")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot4"),
                                          verbatimTextOutput("causal.impact.leads.summary4"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes4"),
                                          plotOutput("plan_breakdown4", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      
                      # Tab 5 
                      
                      tabPanel("Impact 5",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.5"),
                                   infoBoxOutput("utility.5"),
                                   infoBoxOutput("product.5")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name5.tab")),
                                       h4(textOutput("plan_term5.tab")),
                                       h4(textOutput("plan_supplier5.tab")),
                                       h4(textOutput("plan_active5.tab")),
                                       h4(textOutput("plan_id5.tab")),
                                       h2(textOutput("price_change5")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.5")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot5"),
                                          verbatimTextOutput("causal.impact.cr.summary5")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot5"),
                                          verbatimTextOutput("causal.impact.leads.summary5"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes5"),
                                          plotOutput("plan_breakdown5", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 6 
                      
                      tabPanel("Impact 6",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.6"),
                                   infoBoxOutput("utility.6"),
                                   infoBoxOutput("product.6")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name6.tab")),
                                       h4(textOutput("plan_term6.tab")),
                                       h4(textOutput("plan_supplier6.tab")),
                                       h4(textOutput("plan_active6.tab")),
                                       h4(textOutput("plan_id6.tab")),
                                       h2(textOutput("price_change6")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.6")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot6"),
                                          verbatimTextOutput("causal.impact.cr.summary6")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot6"),
                                          verbatimTextOutput("causal.impact.leads.summary6"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes6"),
                                          plotOutput("plan_breakdown6", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 7 
                      
                      tabPanel("Impact 7",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.7"),
                                   infoBoxOutput("utility.7"),
                                   infoBoxOutput("product.7")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name7.tab")),
                                       h4(textOutput("plan_term7.tab")),
                                       h4(textOutput("plan_supplier7.tab")),
                                       h4(textOutput("plan_active7.tab")),
                                       h4(textOutput("plan_id7.tab")),
                                       h2(textOutput("price_change7")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.7")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot7"),
                                          verbatimTextOutput("causal.impact.cr.summary7")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot7"),
                                          verbatimTextOutput("causal.impact.leads.summary7"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes7"),
                                          plotOutput("plan_breakdown7", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 8 
                      
                      tabPanel("Impact 8",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.8"),
                                   infoBoxOutput("utility.8"),
                                   infoBoxOutput("product.8")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name8.tab")),
                                       h4(textOutput("plan_term8.tab")),
                                       h4(textOutput("plan_supplier8.tab")),
                                       h4(textOutput("plan_active8.tab")),
                                       h4(textOutput("plan_id8.tab")),
                                       h2(textOutput("price_change8")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.8")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot8"),
                                          verbatimTextOutput("causal.impact.cr.summary8")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot8"),
                                          verbatimTextOutput("causal.impact.leads.summary8"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes8"),
                                          plotOutput("plan_breakdown8", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 9 
                      
                      tabPanel("Impact 9",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.9"),
                                   infoBoxOutput("utility.9"),
                                   infoBoxOutput("product.9")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name9.tab")),
                                       h4(textOutput("plan_term9.tab")),
                                       h4(textOutput("plan_supplier9.tab")),
                                       h4(textOutput("plan_active9.tab")),
                                       h4(textOutput("plan_id9.tab")),
                                       h2(textOutput("price_change9")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.9")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot9"),
                                          verbatimTextOutput("causal.impact.cr.summary9")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot9"),
                                          verbatimTextOutput("causal.impact.leads.summary9"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes9"),
                                          plotOutput("plan_breakdown9", height = "600px")
                                          
                                   )
                               )
                      ),
                      
                      # Tab 10 
                      
                      tabPanel("Impact 10",
                               h2(" "),
                               fluidRow(
                                   infoBoxOutput("change.10"),
                                   infoBoxOutput("utility.10"),
                                   infoBoxOutput("product.10")
                               ),
                               h2(" "),
                               fluidRow(
                                   box(title = "Prediction", status = "primary", solidHeader = TRUE, width = 6,
                                       h3(textOutput("plan_name10.tab")),
                                       h4(textOutput("plan_term10.tab")),
                                       h4(textOutput("plan_supplier10.tab")),
                                       h4(textOutput("plan_active10.tab")),
                                       h4(textOutput("plan_id10.tab")),
                                       h2(textOutput("price_change10")),
                                       h4(" ")
                                   ),
                                   box(title = "Explanation", status = "primary", solidHeader = TRUE, width = 6,
                                       textOutput("likelihood.description.10")
                                   )
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Conversion Rate", align = "center"),
                                          plotOutput("causal.impact.cr.plot10"),
                                          verbatimTextOutput("causal.impact.cr.summary10")),
                                   column(width = 6,
                                          h4("Causal Impact Analysis of Lead Volume", align = "center"),
                                          plotOutput("causal.impact.leads.plot10"),
                                          verbatimTextOutput("causal.impact.leads.summary10"))
                               ),
                               h2(" "),
                               fluidRow(
                                   column(width = 8, offset = 2,
                                          plotlyOutput("complete.plan.changes10"),
                                          plotOutput("plan_breakdown10", height = "600px")
                                          
                                   )
                               )
                      )
                      
                  ),
                  
                  img(src='Top.png', align = "center", height = '300px', width = '1000px'),
                  img(src='Middle.png', align = "center", height = '400px', width = '1000px'),
                  img(src='Bottom.png', align = "center", height = '520px', width = '995px')

                  
                      
                  )
        )

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
