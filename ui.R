# Define UI for application that draws a histogram
shinyUI(
  
  dashboardPage(skin = "green",
    dashboardHeader(title = "Fortune 500 Companies 2018"),
    
    dashboardSidebar(
      sidebarMenu(
        ## GENERAL OVERVIEW
        menuItem("Overview of Fortune 500", tabName = "overview", icon = icon("fas fa-money-bill-alt")),
        
        # ## COMPANY PROFILE
        # menuItem("Company Profile", tabName = "profile", icon = icon("far fa-building")),
        
        ## COMPANY COMPARISON
        menuItem("Company Comparison", tabName = "comparison", icon = icon("fas fa-balance-scale")),
        
        ## SECTOR BREAKDOWN
        menuItem("Sector Breakdown", tabName = "sector", icon = icon("far fa-building")),
        
        ## F500 LOCATION BREAKDOWN
        menuItem("Company by Location", tabName = "location", icon = icon("fas fa-map-marker-alt")),
        
        ## GENDER BREAKDOWN
        menuItem("THE FUTURE IS FEMALE", tabName = "females", icon = icon("female"))
        
        
        # ## F500 DATA
        # menuItem("Fortune 500 Data", tabName = "data", icon = icon("database"))
        )
      ),
    
    dashboardBody(
      tabItems(
        ## Tab for overview
        tabItem(tabName = "overview",
                tabPanel("Charts"),
                column(5, fluidRow(box(htmlOutput("overview_rev"), br(),
                                       htmlOutput("overview_prof"), br(),
                                       htmlOutput("overview_employees"), br(),
                                       htmlOutput("overview_years"),
                                       width = 12, height = "900px"))),
                #column(6, fluidRow(box(htmlOutput("overview_rev_emp"), width = 12))),
                #column(6, fluidRow(box(htmlOutput("overview_prof_emp"), width = 12))),
                column(7, box(htmlOutput("overview_rev_vs_prof"), width = 12, height = "900px"))#,
                #column(6, fluidRow(box(htmlOutput("overview_e_rev_vs_prof"), width = 12)))
        ),
        
        
        
        ## Tab for company comparisons
        tabItem(tabName = "comparison",
                ## INPUT 1 COMPANY 1
                column(6, selectizeInput(inputId = "company1",
                                         label = "Company 1",
                                         choices = company_names,
                                         selected = "Estee Lauder"),
                       br(),
                       infoBoxOutput("company1_CEO", width = 6),
                       infoBoxOutput("company1_location", width = 6),
                       infoBoxOutput("company1_rank", width = 6),
                       infoBoxOutput("company1_rank_change", width = 6),
                       infoBoxOutput("company1_sector", width = 6),
                       infoBoxOutput("company1_industry", width = 6),
                       infoBoxOutput("company1_revenue", width = 6),
                       infoBoxOutput("company1_revenue_change", width = 6),
                       infoBoxOutput("company1_profit", width = 6),
                       infoBoxOutput("company1_profit_change", width = 6),
                       infoBoxOutput("company1_assets", width = 6),
                       infoBoxOutput("company1_mkt_value", width = 6),
                       infoBoxOutput("company1_employees", width = 6),
                       infoBoxOutput("company1_years", width = 6)
                       ),
                
                ## INPUT 2 COMPANY 2
                column(6,selectizeInput(inputId = "company2",
                                        label = "Company 2",
                                        choices = company_names,
                                        selected = "Ulta Beauty"),
                       br(),
                       infoBoxOutput("company2_CEO", width = 6),
                       infoBoxOutput("company2_location", width = 6),
                       infoBoxOutput("company2_rank", width = 6),
                       infoBoxOutput("company2_rank_change", width = 6),
                       infoBoxOutput("company2_sector", width = 6),
                       infoBoxOutput("company2_industry", width = 6),
                       infoBoxOutput("company2_revenue", width = 6),
                       infoBoxOutput("company2_revenue_change", width = 6),
                       infoBoxOutput("company2_profit", width = 6),
                       infoBoxOutput("company2_profit_change", width = 6),
                       infoBoxOutput("company2_assets", width = 6),
                       infoBoxOutput("company2_mkt_value", width = 6),
                       infoBoxOutput("company2_employees", width = 6),
                       infoBoxOutput("company2_years", width = 6))
                ),
        ## Tab for sector comparisons
        tabItem(tabName = "sector",
                ## INPUT SECTOR 1 AND 2
                column(6, selectizeInput(inputId = "sectors1",
                                         label = "Sector 1",
                                         choices = sector_choices,
                                         selected = "Retailing")
                       ),
                column(6, selectizeInput(inputId = "sectors2",
                                         label = "Sector 2",
                                         choices = sector_choices,
                                         selected = "Technology")
                       ),

                ## AVG REVENUE AND PROFIT
                column(6, fluidRow(box(valueBoxOutput("avgrev_sector1", width = 6),
                                        valueBoxOutput("avgprof_sector1", width = 6), width = 12))),
                
                column(6, fluidRow(box(valueBoxOutput("avgrev_sector2", width = 6),
                                        valueBoxOutput("avgprof_sector2", width = 6), width = 12))),
                ## OUTPUT PIE CHARTS
                column(6, fluidRow(box(htmlOutput("sectors1"), height = 450, width = 12))),
                column(6, fluidRow(box(htmlOutput("sectors2"), height = 450, width = 12))),
                ## OUTPUT TOTAL REV BAR GRAPHS
                column(6, fluidRow(box(htmlOutput("sectors1_rev_bar"), height = 450, width = 12))),
                column(6, fluidRow(box(htmlOutput("sectors2_rev_bar"), height = 450, width = 12))),
                ## OUTPUT TOTAL PROF BAR GRAPHS
                column(6, fluidRow(box(htmlOutput("sectors1_prof_bar"), height = 450, width = 12))),
                column(6, fluidRow(box(htmlOutput("sectors2_prof_bar"), height = 450, width = 12)))
                ),
        
        ## Tab for Fortune 500 locations
        tabItem(tabName = "location",
                ## MAP
                column(6, fluidRow(box(htmlOutput("location"),
                                       br(),
                                       checkboxInput("show", "Show comparison", value = FALSE),
                                       height = 700, width = 12))
                ),
                ## STATE 1
                  column(6, selectizeInput(inputId = "state1",
                                           label = "State 1",
                                           choices = state_choices,
                                           selected = "NY"),
                         fluidRow(box(valueBoxOutput("avgrev_state1", width = 6),
                                      valueBoxOutput("avgprof_state1", width = 6), width = 12)),
                         fluidRow(box(htmlOutput("revenue_hist_state1", width = 12), width = 12)),
                         fluidRow(box(htmlOutput("prof_hist_state1", width = 12), width = 12)),
                         fluidRow(box(dataTableOutput("state1_table"), width = 6))
                         ),
                  ## STATE 2
                  column(6, selectizeInput(inputId = "state2",
                                           label = "State 2",
                                           choices = state_choices,
                                           selected = "CA"),
                         fluidRow(box(valueBoxOutput("avgrev_state2", width = 6),
                                      valueBoxOutput("avgprof_state2", width = 6), width = 12)),
                         fluidRow(box(htmlOutput("revenue_hist_state2", width = 12), width = 12)),
                         fluidRow(box(htmlOutput("prof_hist_state2", width = 12), width = 12)),
                         fluidRow(box(dataTableOutput("state2_table"), width = 6))
                         )
                ),

        
        ## Tab for gender breakdown
        tabItem(tabName = "females",
                ## INPUT GENDER
                column(12, fluidRow(infoBoxOutput("femaleinfo", width = 12))),
                
                column(12, selectizeInput(inputId = "f_ceo",
                                          label = "Female CEOs",
                                          choices = female_choices)),
                column(6, box(imageOutput("female_image"), width = 12, height = "500px")),
                column(6, fluidRow(infoBoxOutput("female_f500_overview_name", width = NULL),
                                   infoBoxOutput("female_f500_overview_company", width = NULL),
                                   infoBoxOutput("female_f500_overview_rank", width = NULL)),
                       box(htmlOutput("female_biography"), width = 12))
        ),
        
        ## Tab for Fortune 500 data
        tabItem(tabName = "data",
                # datatable
                fluidRow(box(DT::dataTableOutput("table"),
                             width = 12)))
        
      )
    )
  )
)
