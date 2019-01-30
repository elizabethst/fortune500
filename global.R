## GITIGNORE FILE
# .DSTORE
# anything containing password
# keep credentials in local files

library(shiny)
library(shinythemes)
library(shinydashboard)
library(googleVis)
library(DT)


library(tidyverse) # ggplot2 tibble tidyr readr
library(data.table) # for fread
library(ggthemes)
library(googleVis)
# library(leaflet)
library(scales) # for comma fx

#setwd("~/Documents/NYC Data Science Academy/RShiny_Project_Fortune/fortune500/")
f500 = fread("./data/f500.csv")
company_names = f500$Title
genders = unique(f500$`CEO Gender`)
sector_choices = sort(unique(f500$Sector))
state_choices = sort(unique(f500$State))
f500_females = fread("./data/f500_females.csv")
female_choices = f500_females$CEO

# by sector and industry
by_sector_industry = f500 %>% group_by(., Sector, Industry) %>% summarise(., Number = n())

f500 %>% group_by(., Sector, Industry) %>% summarise(., ttl_rev = sum(`Revenues ($M)`))


# UNDER THE OVERVIEW SECTION.
# Plotting revenues and profits, colored by different sectors
rev_prof = data.frame(f500$`Revenues ($M)`, f500$`Profits ($M)`, f500$`Sector`, f500$`Title`)
colnames(rev_prof) = c("Revenues ($M)", "Profits ($M)", "Sector", "Title")
rev_prof = rev_prof %>% spread(Sector, `Profits ($M)`)
tooltip_matrix = matrix(rep(rev_prof$`Title`, 21), ncol = 21)
colnames(tooltip_matrix) = paste0(colnames(rev_prof)[3:ncol(rev_prof)], ".html.tooltip")


rev_prof_tt = data.frame(matrix(ncol = (1+21+21), nrow = 500))
rev_prof_tt[,1] = rev_prof[,1]
rev_prof_tt[,seq(2, 2*(ncol(rev_prof)-2), by = 2)] = rev_prof[,c(3:ncol(rev_prof))]
rev_prof_tt[,seq(3, 2*(ncol(rev_prof)-1), by = 2)] = tooltip_matrix
colnames(rev_prof_tt)[1] = c("Revenues ($M)")
colnames(rev_prof_tt)[seq(2, 2*(ncol(rev_prof)-2), by = 2)] = colnames(rev_prof)[c(3:ncol(rev_prof))]
colnames(rev_prof_tt)[seq(3, 2*(ncol(rev_prof)-1), by = 2)] = colnames(tooltip_matrix)



# UNDER THE OVERVIEW SECTION.
# Plotting revenues and profits, colored by different sectors
e_rev_prof = data.frame(f500$`Revenues per Employee ($M)`, f500$`Profits per Employee ($M)`, f500$`Sector`, f500$`Title`)
colnames(e_rev_prof) = c("Revenues per Employee ($M)", "Profits per Employee ($M)", "Sector", "Title")
e_rev_prof = e_rev_prof %>% spread(Sector, `Profits per Employee ($M)`)
e_tooltip_matrix = matrix(rep(e_rev_prof$`Title`, 21), ncol = 21)
colnames(e_tooltip_matrix) = paste0(colnames(e_rev_prof)[3:ncol(e_rev_prof)], ".html.tooltip")


e_rev_prof_tt = data.frame(matrix(ncol = (1+21+21), nrow = 500))
e_rev_prof_tt[,1] = e_rev_prof[,1]
e_rev_prof_tt[,seq(2, 2*(ncol(e_rev_prof)-2), by = 2)] = e_rev_prof[,c(3:ncol(e_rev_prof))]
e_rev_prof_tt[,seq(3, 2*(ncol(e_rev_prof)-1), by = 2)] = tooltip_matrix
colnames(e_rev_prof_tt)[1] = c("Revenues per Employee ($M)")
colnames(e_rev_prof_tt)[seq(2, 2*(ncol(e_rev_prof)-2), by = 2)] = colnames(e_rev_prof)[c(3:ncol(e_rev_prof))]
colnames(e_rev_prof_tt)[seq(3, 2*(ncol(e_rev_prof)-1), by = 2)] = colnames(tooltip_matrix)









# location
by_state = f500 %>% group_by(., State) %>% summarise(., "Companies" = n())
# by_state %>% arrange(., desc(Companies)) %>% top_n(10)

