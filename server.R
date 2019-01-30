# Define server logic required to draw a histogram
shinyServer(function(input, output, session){

  # OVERVIEW
  output$overview_rev = renderGvis({
    ## Show overall distribution of revenues
    if (input$radio == 1) {
      gvisHistogram(f500[,"Revenues ($M)", drop = F],
                    options=list(title = "Distribution of Revenues ($M)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
      
    } else if (input$radio == 2){
      gvisLineChart(f500, xvar = "Rank", yvar = "Revenues ($M)",
                   options=list(title = "Distribution of Revenues per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
    ## Show revenue per employee
    else if (input$radio == 3){
        gvisHistogram(f500[,"Revenues per Employee ($M)", drop = F]*1000,
                      options=list(title = "Distribution of Revenues per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
  })
  
  output$overview_prof = renderGvis({
    ## Show overall distribution of revenues
    if (input$radio == 1) {
      gvisHistogram(f500[,"Profits ($M)", drop = F],
                    options=list(title = "Distribution of Profits ($M)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
      
    } else if (input$radio == 2){
      gvisLineChart(f500, xvar = "Rank", yvar = "Profits ($M)",
                    options=list(title = "Distribution of Profits per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
    ## Show revenue per employee
    else if (input$radio == 3){
      gvisHistogram(f500[,"Profits per Employee ($M)", drop = F]*1000,
                    options=list(title = "Distribution of Profits per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
  })
  
  # output$overview_rev = renderGvis ({
  #   gvisHistogram(f500[,"Revenues ($M)", drop = F],
  #                 options=list(title = "Distribution of Revenues ($M)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  # })
  # output$overview_prof = renderGvis ({
  #   gvisHistogram(f500[,"Profits ($M)", drop = F],
  #                 options=list(title = "Distribution of Profits ($M)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  # })
  # output$overview_rev_emp = renderGvis ({
  #   gvisHistogram(f500[,"Revenues per Employee ($M)", drop = F]*1000,
  #                 options=list(title = "Distribution of Revenues per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  # })
  # output$overview_prof_emp = renderGvis ({
  #   gvisHistogram(f500[,"Profits per Employee ($M)", drop = F]*1000,
  #                 options=list(title = "Distribution of Profits per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  # })
  output$overview_employees = renderGvis ({
    gvisHistogram(f500[,"Employees", drop = F],
                  options=list(title = "Distribution of Employees", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  output$overview_years = renderGvis ({
    gvisHistogram(f500[,"Years on Fortune 500 List", drop = F],
                  options=list(title = "Distribution of Time on Fortune 500 List", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  
  output$overview_rev_vs_prof = renderGvis({
    if (input$radio == 1 | input$radio == 2){
      gvisScatterChart(rev_prof_tt,options=list(width="auto", height="850px",
                                                title="Revenues vs. Profits",
                                                hAxis="{title:'Revenue ($M)'}",
                                                vAxis="{title:'Profit ($M)'}",
                                                explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    } else if (input$radio ==3){
      gvisScatterChart(e_rev_prof_tt,options=list(width="auto", height="850px",
                                                  #title="Revenues and Profits",
                                                  hAxis="{title:'Revenue per Employee ($1,000)'}",
                                                  vAxis="{title:'Profit per Employee ($1,000)'}",
                                                  explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
      }
  })

  
  
  output$overview_rev = renderGvis({
    ## Show overall distribution of revenues
    if (input$radio == 1) {
      gvisHistogram(f500[,"Revenues ($M)", drop = F],
                    options=list(title = "Distribution of Revenues ($M)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
      
    } else if (input$radio == 2){
      gvisLineChart(f500, xvar = "Rank", yvar = "Revenues ($M)",
                    options=list(title = "Distribution of Revenues per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
    ## Show revenue per employee
    else if (input$radio == 3){
      gvisHistogram(f500[,"Revenues per Employee ($M)", drop = F]*1000,
                    options=list(title = "Distribution of Revenues per Employee ($1,000)", width="auto", height="auto", legend="none", explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
    }
  })
  
  
  # COMPANY COMPARISON
  # COMPANY 1
  output$company1_CEO = renderInfoBox({
    current_CEO = f500 %>% filter(., Title == input$company1) %>% pull(., `CEO`)
    infoBox(title = "CEO", value = current_CEO, color = "black", icon = icon("fas fa-user-tie"))
  })
  output$company1_location = renderInfoBox({
    city_state = f500 %>% filter(., Title == input$company1) %>% select(., `City`, `State`)
    city = city_state %>% pull(., `City`)
    state = city_state %>% pull(., `State`)
    infoBox(title = "Company HQ", value = paste(city, state, sep = ", "), color = "black", icon = icon("fas fa-location-arrow"))
  })
  output$company1_sector = renderInfoBox({
    pf_sector = f500 %>% filter(., Title == input$company1) %>% pull(., `Sector`)
    infoBox(title = "Sector", value = pf_sector, color = "black", icon = icon("fas fa-chart-pie"))
  })
  output$company1_industry = renderInfoBox({
    pf_industry = f500 %>% filter(., Title == input$company1) %>% pull(., `Industry`)
    infoBox(title = "Industry", value = pf_industry, color = "black", icon = icon("fas fa-industry"))
  })
  output$company1_rank = renderInfoBox({
    ranking = f500 %>% filter(., Title == input$company1) %>% pull(., Rank)
    infoBox(title = "Current Rank", value = ranking, color = "black", icon = icon("fas fa-trophy"))
  })
  output$company1_rank_change = renderInfoBox({
    ranking_change = f500 %>% filter(., Title == input$company1) %>% pull(., `Change in Rank`)
    infoBox(title = "Change in Rank", value = ranking_change, color = "black", icon = icon("fas fa-random"))
  })
  output$company1_revenue = renderInfoBox({
    revenue = f500 %>% filter(., Title == input$company1) %>% pull(., `Revenues ($M)`)
    infoBox(title = "Revenue ($M)", value = paste0("$",prettyNum(revenue, big.mark = ",")), color = "black", icon = icon("fas fa-money-bill"))
  })
  output$company1_revenue_change = renderInfoBox({
    revenue_change = f500 %>% filter(., Title == input$company1) %>% pull(., `Revenue Change`)
    infoBox(title = "Change in Revenue", value = revenue_change, color = "black", icon = icon("fas fa-chart-line"))
  })
  output$company1_profit = renderInfoBox({
    profit = f500 %>% filter(., Title == input$company1) %>% pull(., `Profits ($M)`)
    infoBox(title = "Profit ($M)", value = paste0("$",prettyNum(profit, big.mark = ",")), color = "black", icon = icon("fas fa-hand-holding-usd"))
  })
  output$company1_profit_change = renderInfoBox({
    profit_change = f500 %>% filter(., Title == input$company1) %>% pull(., `Profit Change`)
    infoBox(title = "Change in Profit", value = profit_change, color = "black", icon = icon("fas fa-chart-line"))
  })
  output$company1_rev_emp = renderInfoBox({
    rev_emp = f500 %>% filter(., Title == input$company1) %>% pull(., `Revenues per Employee ($M)`)
    infoBox(title = "Revenue per Employee ($1000)", value = 1000*rev_emp)
  })
  output$company1_prof_emp = renderInfoBox({
    prof_emp = f500 %>% filter(., Title == input$company1) %>% pull(., `Profits per Employee ($M)`)
    infoBox(title = "Profit per Employee ($1000)", value = 1000*prof_emp)
  })  
  output$company1_assets = renderInfoBox({
    prof_assets = f500 %>% filter(., Title == input$company1) %>% pull(., `Assets ($M)`)
    infoBox(title = "Assets ($M)", value = paste0("$",prettyNum(prof_assets, big.mark = ",")), color = "black", icon = icon("fas fa-shopping-basket"))
  })
  output$company1_mkt_value = renderInfoBox({
    prof_mktvalue = f500 %>% filter(., Title == input$company1) %>% pull(., `Mkt Value as of 3/29/18 ($M)`)
    infoBox(title = "Market Value ($M)", value = paste0("$",prettyNum(prof_mktvalue, big.mark = ",")), color = "black", icon = icon("fas fa-business-time"))
  })
  output$company1_employees = renderInfoBox({
    emp = f500 %>% filter(., Title == input$company1) %>% pull(., `Employees`)
    infoBox(title = "Employees", value = prettyNum(emp, big.mark = ","), color = "black", icon = icon("fas fa-users"))
  })
  output$company1_years = renderInfoBox({
    years = f500 %>% filter(., Title == input$company1) %>% pull(., `Years on Fortune 500 List`)
    infoBox(title = "Years on F500 List", value = years, color = "black", icon = icon("far fa-calendar-alt"))
  })
  
  
  # COMPANY 2
  output$company2_CEO = renderInfoBox({
    current_CEO = f500 %>% filter(., Title == input$company2) %>% pull(., `CEO`)
    infoBox(title = "CEO", value = current_CEO, color = "black", icon = icon("fas fa-user-tie"))
  })
  output$company2_location = renderInfoBox({
    city_state = f500 %>% filter(., Title == input$company2) %>% select(., `City`, `State`)
    city = city_state %>% pull(., `City`)
    state = city_state %>% pull(., `State`)
    infoBox(title = "Company HQ", value = paste(city, state, sep = ", "), color = "black", icon = icon("fas fa-location-arrow"))
  })
  output$company2_sector = renderInfoBox({
    pf_sector = f500 %>% filter(., Title == input$company2) %>% pull(., `Sector`)
    infoBox(title = "Sector", value = pf_sector, color = "black", icon = icon("fas fa-chart-pie"))
  })
  output$company2_industry = renderInfoBox({
    pf_industry = f500 %>% filter(., Title == input$company2) %>% pull(., `Industry`)
    infoBox(title = "Industry", value = pf_industry, color = "black", icon = icon("fas fa-industry"))
  })
  output$company2_rank = renderInfoBox({
    ranking = f500 %>% filter(., Title == input$company2) %>% pull(., Rank)
    infoBox(title = "Current Rank", value = ranking, color = "black", icon = icon("fas fa-trophy"))
  })
  output$company2_rank_change = renderInfoBox({
    ranking_change = f500 %>% filter(., Title == input$company2) %>% pull(., `Change in Rank`)
    infoBox(title = "Change in Rank", value = ranking_change, color = "black", icon = icon("fas fa-random"))
  })
  output$company2_revenue = renderInfoBox({
    revenue = f500 %>% filter(., Title == input$company2) %>% pull(., `Revenues ($M)`)
    infoBox(title = "Revenue ($M)", value = paste0("$",prettyNum(revenue, big.mark = ",")), color = "black", icon = icon("fas fa-money-bill"))
  })
  output$company2_revenue_change = renderInfoBox({
    revenue_change = f500 %>% filter(., Title == input$company2) %>% pull(., `Revenue Change`)
    infoBox(title = "Change in Revenue", value = revenue_change, color = "black", icon = icon("fas fa-chart-line"))
  })
  output$company2_profit = renderInfoBox({
    profit = f500 %>% filter(., Title == input$company2) %>% pull(., `Profits ($M)`)
    infoBox(title = "Profit ($M)", value = paste0("$",prettyNum(profit, big.mark = ",")), color = "black", icon = icon("fas fa-hand-holding-usd"))
  })
  output$company2_profit_change = renderInfoBox({
    profit_change = f500 %>% filter(., Title == input$company2) %>% pull(., `Profit Change`)
    infoBox(title = "Change in Profit", value = profit_change, color = "black", icon = icon("fas fa-chart-line"))
  })
  output$company2_rev_emp = renderInfoBox({
    rev_emp = f500 %>% filter(., Title == input$company2) %>% pull(., `Revenues per Employee ($M)`)
    infoBox(title = "Revenue per Employee ($1000)", value = 1000*rev_emp, icon = icon("fas fa-trophy"))
  })
  output$company2_prof_emp = renderInfoBox({
    prof_emp = f500 %>% filter(., Title == input$company2) %>% pull(., `Profits per Employee ($M)`)
    infoBox(title = "Profit per Employee ($1000)", value = 1000*prof_emp, color = "black", icon = icon("fas fa-chart-line"))
  })  
  output$company2_assets = renderInfoBox({
    prof_assets = f500 %>% filter(., Title == input$company2) %>% pull(., `Assets ($M)`)
    infoBox(title = "Assets ($M)", value = paste0("$",prettyNum(prof_assets, big.mark = ",")), color = "black", icon = icon("fas fa-shopping-basket"))
  })
  output$company2_mkt_value = renderInfoBox({
    prof_mktvalue = f500 %>% filter(., Title == input$company2) %>% pull(., `Mkt Value as of 3/29/18 ($M)`)
    infoBox(title = "Market Value ($M)", value = paste0("$",prettyNum(prof_mktvalue, big.mark = ",")), color = "black", icon = icon("fas fa-business-time"))
  })
  output$company2_employees = renderInfoBox({
    emp = f500 %>% filter(., Title == input$company2) %>% pull(., `Employees`)
    infoBox(title = "Employees", value = prettyNum(emp, big.mark = ","), color = "black", icon = icon("fas fa-users"))
  })
  output$company2_years = renderInfoBox({
    years = f500 %>% filter(., Title == input$company2) %>% pull(., `Years on Fortune 500 List`)
    infoBox(title = "Years on F500 List", value = years, color = "black", icon = icon("far fa-calendar-alt"))
  })
  
  
  
  
  
  
  # BREAKDOWN BY SECTOR
  output$sectors1 = renderGvis ({
    gvisPieChart(subset(by_sector_industry, Sector == input$sectors1)[,c(2,3)],
                 options = list(title = paste("Industries in", input$sectors1, "Sector"), width = "auto", height = "400px"))
  })
  output$sectors2 = renderGvis ({
    gvisPieChart(subset(by_sector_industry, Sector == input$sectors2)[,c(2,3)],
                 options = list(title = paste("Industries in", input$sectors2, "Sector"), width = "auto", height = "400px"))
  })
  
  ## Should try and stack the revenue (take out profits to get costs) and profits
  output$sectors1_rev_bar = renderGvis ({
    gvisBarChart(subset(f500 %>% group_by(., Sector, Industry) %>% summarise(., ttl_rev = sum(`Revenues ($M)`)), Sector == input$sectors1)[,c(2,3)] %>% arrange(., desc(ttl_rev)), xvar = "Industry", yvar = "ttl_rev",
                 options = list(title = paste("Total revenues by industry in", input$sectors1, "Sector"), width = "auto", height = "400px"))
  })
  output$sectors2_rev_bar = renderGvis ({
    gvisBarChart(subset(f500 %>% group_by(., Sector, Industry) %>% summarise(., ttl_rev = sum(`Revenues ($M)`)), Sector == input$sectors2)[,c(2,3)] %>% arrange(., desc(ttl_rev)), xvar = "Industry", yvar = "ttl_rev",
                 options = list(title = paste("Total revenues by industry in", input$sectors2, "Sector"), width = "auto", height = "400px"))
  })
  
  output$sectors1_prof_bar = renderGvis ({
    gvisBarChart(subset(f500 %>% group_by(., Sector, Industry) %>% summarise(., ttl_prof = sum(`Profits ($M)`)), Sector == input$sectors1)[,c(2,3)] %>% arrange(., desc(ttl_prof)), xvar = "Industry", yvar = "ttl_prof",
                 options = list(title = paste("Total profits by industry in", input$sectors1, "Sector"), width = "auto", height = "400px"))
  })
  output$sectors2_prof_bar = renderGvis ({
    gvisBarChart(subset(f500 %>% group_by(., Sector, Industry) %>% summarise(., ttl_prof = sum(`Profits ($M)`)), Sector == input$sectors2)[,c(2,3)] %>% arrange(., desc(ttl_prof)), xvar = "Industry", yvar = "ttl_prof",
                 options = list(title = paste("Total profits by industry in", input$sectors2, "Sector"), width = "auto", height = "400px"))
  })

  
  
  
  # REVENUE STATISTICS BY SECTOR USING INFOBOX
  output$avgrev_sector1 <- renderValueBox({
    avgrev_value = f500 %>% filter(., Sector == input$sectors1) %>% summarise(., mean = mean(`Revenues ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgrev_value), big.mark = ","))),
             subtitle = "Average Revenue ($M)", color = "olive")
  })
  output$avgprof_sector1 <- renderValueBox({
    avgprof_value = f500 %>% filter(., Sector == input$sectors1) %>% summarise(., mean = mean(`Profits ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgprof_value), big.mark = ","))),
             subtitle = "Average Profit ($M)", color = "olive")
  })
  output$avgrev_sector2 <- renderValueBox({
    avgrev_value = f500 %>% filter(., Sector == input$sectors2) %>% summarise(., mean = mean(`Revenues ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgrev_value), big.mark = ","))),
             subtitle = "Average Revenue ($M)", color = "olive")
  })
  output$avgprof_sector2 <- renderValueBox({
    avgprof_value = f500 %>% filter(., Sector == input$sectors2) %>% summarise(., mean = mean(`Profits ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgprof_value), big.mark = ","))),
             subtitle = "Average Profit ($M)", color = "olive")
  })
  
  
  # BREAKDOWN BY LOCATION
  # SHOWING MAP WITH GOOGLEVIS
  output$location = renderGvis({
    if (input$show) {
      gvisGeoChart(by_state %>% filter(., State == input$state1 | State == input$state2),
                   locationvar = "State",
                   colorvar = "Companies",
                   options=list(region="US",
                                displayMode="regions",
                                resolution="provinces",
                                width="auto", height="600px",
                                legend = "none"))
    } else {
      gvisGeoChart(by_state,
                   locationvar = "State",
                   colorvar = "Companies",
                   options=list(region="US",
                                displayMode="regions",
                                resolution="provinces",
                                width="auto", height="600px",
                                legend = "none"))
    }
  })
  output$avgrev_state1 <- renderValueBox({
    avgrev_value = f500 %>% filter(., State == input$state1) %>% summarise(., mean = mean(`Revenues ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgrev_value), big.mark = ","))),
             subtitle = "Avg. Revenue ($M)", color = "olive")
  })
  output$avgprof_state1 <- renderValueBox({
    avgprof_value = f500 %>% filter(., State == input$state1) %>% summarise(., mean = mean(`Profits ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgprof_value), big.mark = ","))),
             subtitle = "Avg. Profit ($M)", color = "olive")
  })
  output$avgrev_state2 <- renderValueBox({
    avgrev_value = f500 %>% filter(., State == input$state2) %>% summarise(., mean = mean(`Revenues ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgrev_value), big.mark = ","))),
             subtitle = "Avg. Revenue ($M)", color = "olive")
  })
  output$avgprof_state2 <- renderValueBox({
    avgprof_value = f500 %>% filter(., State == input$state2) %>% summarise(., mean = mean(`Profits ($M)`, na.rm=TRUE))
    valueBox(value = tags$p(paste0("$", prettyNum(round(avgprof_value), big.mark = ","))),
             subtitle = "Avg. Profit ($M)", color = "olive")
  })
  output$revenue_hist_state1 = renderGvis({
    state1_rev = f500 %>% filter(., State == input$state1) %>% select(., `Revenues ($M)`)
    gvisHistogram(state1_rev,
                  options=list(title = "Distribution of Revenue by State ($M)",
                               width="auto", height="auto", legend="none",
                               explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  output$prof_hist_state1 = renderGvis({
    state1_prof = f500 %>% filter(., State == input$state1) %>% select(., `Profits ($M)`)
    gvisHistogram(state1_prof,
                  options=list(title = "Distribution of Profit by State ($M)",
                               width="auto", height="auto", legend="none",
                               explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  output$revenue_hist_state2 = renderGvis({
    state2_rev = f500 %>% filter(., State == input$state2) %>% select(., `Revenues ($M)`)
    gvisHistogram(state2_rev,
                  options=list(title = "Distribution of Revenue by State ($M)",
                               width="auto", height="auto", legend="none",
                               explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  output$prof_hist_state2 = renderGvis({
    state2_prof = f500 %>% filter(., State == input$state2) %>% select(., `Profits ($M)`)
    gvisHistogram(state2_prof,
                  options=list(title = "Distribution of Profit by State ($M)",
                               width="auto", height="auto", legend="none",
                               explorer="{actions:['dragToZoom', 'rightClickToReset'], maxZoomIn:0.01}"))
  })
  output$state1_table = renderDataTable({
    state1_ranks = f500 %>% filter(., State == input$state1) %>% select(., Rank, Title) %>% arrange(., Rank)
    datatable(state1_ranks, rownames = F, caption = paste("Company ranks in", input$state1), options=list(lengthMenu = c(5, 10, 15, 20)))
  })
  output$state2_table = renderDataTable({
    state2_ranks = f500 %>% filter(., State == input$state2) %>% select(., Rank, Title) %>% arrange(., Rank)
    datatable(state2_ranks, rownames = F, caption = paste("Company ranks in", input$state2), options=list(lengthMenu = c(5, 10, 15, 20)))
  })
  
  
  # FEMALE HIGHLIGHTS
  output$femaleinfo = renderValueBox({
    infoBox(title = "Not-so-fun fact:", subtitle = "Only 5% of Fortune 500 CEOs in 2018 are female. You can check them out below!", icon = icon("venus"), color = "fuchsia")
  })
  output$female_image = renderImage({
    filename = normalizePath(file.path("./images",
                                       paste0(f500_females %>% filter(., CEO == input$f_ceo) %>% pull(imagenames), ".jpg")))
    list(src = filename, width = "100%")
  }, deleteFile = FALSE)
  output$female_f500_overview_name = renderInfoBox({
    infoBox(title = "Company CEO", value = input$f_ceo, icon = icon("female"), color = "fuchsia")
  })
  output$female_f500_overview_company = renderInfoBox({
    f_overview = f500_females %>% filter(., `CEO` == input$f_ceo) %>% select(., Title, Rank)
    infoBox(title = "Company name", value = pull(f_overview, Title), color = "fuchsia")
  })
  output$female_f500_overview_rank = renderInfoBox({
    f_overview = f500_females %>% filter(., `CEO` == input$f_ceo) %>% select(., Title, Rank)
    infoBox(title = "Current rank", value = pull(f_overview, Rank), icon = icon("fas fa-trophy"), color = "fuchsia")
  })
  output$female_biography <- renderUI({
    str1 <- f500_females %>% filter(., `CEO` == input$f_ceo) %>% select(., fem_bios) %>% pull()
    link = f500_females %>% filter(., `CEO` == input$f_ceo) %>% select(., fem_bio_links) %>% pull()
    str2 <- paste("To learn more about", input$f_ceo, "you can click", a("here", class = "web", href = link))
    HTML(paste(str1, str2, sep = '<br/><br/>'))
    
  })
  
  
  
  
  
  output$table = DT::renderDataTable({
    datatable(f500, rownames = F)
  })

  
  
})
