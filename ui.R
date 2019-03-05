shinyUI(
  
  dashboardPage(skin = "green",
    dashboardHeader(title = "Fortune 500 Companies 2018"),
    
    dashboardSidebar(
      sidebarMenu(
        
        ## GENERAL OVERVIEW
        menuItem("Overview of Fortune 500", tabName = "overview", icon = icon("fas fa-money-bill")),
        
        # ## COMPANY PROFILE
        # menuItem("Company Profile", tabName = "profile", icon = icon("far fa-building")),
        
        ## COMPANY COMPARISON
        menuItem("Company Comparison", tabName = "comparison", icon = icon("fas fa-balance-scale")),
        
        ## SECTOR BREAKDOWN
        menuItem("Sector Breakdown", tabName = "sector", icon = icon("far fa-building")),
        
        ## F500 LOCATION BREAKDOWN
        menuItem("Company by Location", tabName = "location", icon = icon("fas fa-map-marked-alt")),
        
        ## GENDER BREAKDOWN
        menuItem("THE FUTURE IS FEMALE", tabName = "females", icon = icon("female")),
        
        ## ABOUT ME
        menuItem("About", tabName = "contact", icon = icon("fas fa-info-circle"))
        
        )
      ),
    
    dashboardBody(
      tabItems(
        ## Tab for overview
        tabItem(tabName = "overview",
                tabBox(
                  id = "overview_tab", width = 12,

                  tabPanel("Overview",
                           column(6, fluidRow(box(htmlOutput("overview_rev"), br(),
                                               htmlOutput("overview_prof"), br(),
                                               width = 12, height = "750px"))),
                           column(6, box(htmlOutput("overview_rev_vs_prof"), width = 12, height = "750px"))
                           ),
                  tabPanel("Breakdown per Employee",
                           column(6, fluidRow(box(htmlOutput("overview_rev_emp"), br(),
                                                  htmlOutput("overview_prof_emp"), br(),
                                                  width = 12, height = "750px"))),
                           column(6, box(htmlOutput("overview_rev_vs_prof_emp"), width = 12, height = "750px"))
                ),
                height = "900px"
        )),
        
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
                
                tabBox(
                  id = "sector_tab1", width = 12,
                  tabPanel("Industry breakdown",
                           ## OUTPUT PIE CHARTS
                           column(6, br(), fluidRow(htmlOutput("sectors1"), height = 700)),
                           column(6, br(), fluidRow(htmlOutput("sectors2"), height = 700))),
                  tabPanel("Financial breakdown",
                           column(6, fluidRow(infoBoxOutput("avgrev_sector1", width = 6),
                                              infoBoxOutput("avgprof_sector1", width = 6))),
                           column(6, fluidRow(infoBoxOutput("avgrev_sector2", width = 6),
                                              infoBoxOutput("avgprof_sector2", width = 6))),
                           column(6,
                                  br(),
                                  fluidRow(box(htmlOutput("sectors1_rev_bar"),
                                               htmlOutput("sectors1_prof_bar"), height = 600, width = 12))),
                           column(6,
                                  br(),
                                  fluidRow(box(htmlOutput("sectors2_rev_bar"),
                                               htmlOutput("sectors2_prof_bar"), height = 600, width = 12)))
                           ),
                  height = "800px"
                  )
                
                ## OUTPUT TOTAL REV BAR GRAPHS
                
                ),
        
        ## Tab for Fortune 500 locations
        tabItem(tabName = "location",
                # INPUT STATE 1 AND 2
                column(6, selectizeInput(inputId = "state1",
                                         label = "State 1",
                                         choices = state_choices,
                                         selected = "NY")
                       ),
                column(6, selectizeInput(inputId = "state2",
                                         label = "State 2",
                                         choices = state_choices,
                                         selected = "CA")
                       ),
                
                tabBox(
                  id = "state_tab", width = 12,
                  tabPanel("Financial breakdown by state",
                           ## STATE 1
                           column(6, fluidRow(valueBoxOutput("avgrev_state1", width = 6),
                                                  valueBoxOutput("avgprof_state1", width = 6)),
                                  fluidRow(box(htmlOutput("revenue_hist_state1", width = 12), width = 12)),
                                  fluidRow(box(htmlOutput("prof_hist_state1", width = 12), width = 12))
                           ),
                           ## STATE 2
                           column(6, fluidRow(valueBoxOutput("avgrev_state2", width = 6),
                                                  valueBoxOutput("avgprof_state2", width = 6)),
                                  fluidRow(box(htmlOutput("revenue_hist_state2", width = 12), width = 12)),
                                  fluidRow(box(htmlOutput("prof_hist_state2", width = 12), width = 12))
                           )
                           ),
                  tabPanel("Ranking by state",
                           ## STATE 1
                           column(12,
                                  fluidRow(box(dataTableOutput("state1_table"), width = 6),
                                           box(dataTableOutput("state2_table"), width = 6))
                                  
                           )
                  ),
                  ## MAP
                  tabPanel("Map",
                           fluidRow(box(htmlOutput("location"),
                                        checkboxInput("show", "Show comparison", value = FALSE),
                                        height = 350, width = 12))),
                  height = "800px"
                  )
                ),

        ## Tab for gender breakdown
        tabItem(tabName = "females",
                ## INPUT GENDER
                column(12, fluidRow(infoBoxOutput("femaleinfo", width = 12))),
                
                column(12, selectizeInput(inputId = "f_ceo",
                                          label = "Female CEOs",
                                          choices = female_choices)),
                column(12,  box(fluidRow(infoBoxOutput("female_f500_overview_name", width = 4),
                                     infoBoxOutput("female_f500_overview_company", width = 4),
                                     infoBoxOutput("female_f500_overview_rank", width = 4)), width = 12)),
                column(6, box(imageOutput("female_image"), width = 12, height = "400px")),
                column(6,box(htmlOutput("female_biography"), width = 12))
        ),
        
        ## Tab for about me
        tabItem(tabName = "contact",
                column(12,
                       box(htmlOutput("about"), width = 12),
                       box(htmlOutput("contact"), width = 12)
                       )
        )
      )
    )
  )
)
