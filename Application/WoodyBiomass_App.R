# Shiny App Development, Carbon Containment Lab
library(shiny)
library(shinyBS)
library(shinyalert)
library(usmap)
library(readxl)

# Read in libraries for map production
library(stringr)
library(grid)
library(gridExtra)

# Read in libraries for data wrangling
library(tidyverse)
library(sf)
library(plotly)
library(scales)
library(kableExtra)
library(dplyr)

# Fluid page describes the appearance of the interface (UI)
ui <- fluidPage(
  # Load message
  useShinyalert(),
  # Give the panel a title
  titlePanel("Availability of Woody Biomass by Filter on Overstocked Land"),
  # Describe layout of the plots/tables/figures
  sidebarLayout(position = "right", 
                mainPanel(h1("Plot Display"), 
                          plotOutput("USMap"), 
                          bsTooltip("USMap", "State plot: Dark gray represents missing from FIA database, white represents exclusion by selected criteria. Histogram: Individual columns represent sum over total state.",
                                    "left", options = list(container = "body")),
                          plotOutput("RoadPlot"),
                          plotOutput("BurnPlot"),
                          plotOutput("StackedBar"),
                          tableOutput("SummaryTable"),
                          bsTooltip("SummaryTable", "Individual rows represent valid counties that meet all criteria.",
                                    "left", options = list(container = "body"))),
                sidebarPanel(h1("Selections for Scenario"),
                             fluidRow(
                               # Describe user interaction buttons
                               checkboxGroupInput("radio_region", h3("Region for Consideration"),
                                                  choices = c("Arizona" = "AZ",
                                                              "California"= "CA",
                                                              "Colorado"= "CO",
                                                              "Idaho" = "ID",
                                                              "Montana" = "MT",
                                                              "Nevada" = "NV",
                                                              "New Mexico" = "NM",
                                                              "Oregon" = "OR",
                                                              "Utah" = "UT",
                                                              "Washington" = "WA",
                                                              "Wyoming" = "WY"),
                                                  selected = c("WA", "OR", "CA")),
                               #radioButtons("radio_mort", h3("Only counties where mortality exceeds growth"),
                               #            choices = list("All Counties" = 1,
                               #                             "Mortality Exceeds Growth Only" = 2),
                               radioButtons("radio_acre", h3("Total Available Amount or Per Acre"),
                                            choices = list("Total Amount" = 1,
                                                           "Amount per Acre" = 2),
                                            selected = 1),
                               bsTooltip("radio_region", "Calculations for totals shown according to selected regions. Barplots appear only for multi-state region selections",
                                         "right", options = list(container = "body")),
                               
                               # Button for determining which pools are selected from 
                               radioButtons("radio_pool", h3("Potential Pool Restrictions"),
                                            choices = list("Whatever is necessary" = 1,
                                                           "Only <5 inch diameter wood" = 2,
                                                           "Only <9 inch diameter wood" = 3,
                                                           "Only <12 inch diameter wood" = 4,
                                                           "<12 inch diameter and tops" = 5),
                                            selected = 5),
                               
                               # Button for determining method of carbon removal
                               radioButtons("radio_method", h3("Method of Biomass Restoration"),
                                            choices = list("Treatment to 60% Full Stocking" = 1,
                                                           "Treatment to 80% Full Stocking" = 2,
                                                           "Treatment to 100% Full Stocking" = 3),
                                            selected = 2),
                               
                               # Button for determining whether to remove reserved status wood
                               radioButtons("radio_reserved", h3("Remove Reserved Wood from Consideration?"),
                                            choices = list("Keep all wood" = 1,
                                                           "Remove reserved wood" = 2),
                                            selected = 1),
                               bsTooltip("radio_method", "Restoration method refers to the volume at which trees will be cleared, determined by FIA's classification of optimal stocking.",
                                         "right", options = list(container = "body")),
                               
                               # Button for determining manner of fire risk evaluation
                               radioButtons("radio_fire", h3("Fire Risk Field"),
                                            choices = list(#"Burn Probability Percentile (state)" = 1,
                                                           "Burn Probability (nation)" = 2,
                                                           #"Wildfire Hazard Potential (state)" = 3,
                                                           "Wildfire Hazard (nation)" = 4),
                                            selected = 4),
                               
                               # Pop-up text explanation
                               bsTooltip("radio_fire", "Distinctions in percentiles are made relative to national averages and state averages",
                                         "right", options = list(container = "body")),
                               sliderInput("slider_fire", h3("National Percentile of Wildfire Hazard Potential"),
                                           min = 0, max = 100, value = 90),
                               # Pop-up text explanation
                               bsTooltip("slider_fire", "Slider provides value as either percentile or fraction, depending on selected criteria.",
                                         "right", options = list(container = "body")),
                               radioButtons("radio_dead", h3("Deadwood Additionally Included in Removal"),
                                            choices = list("Include salvageable dead wood" = 1,
                                                           "Ignore salvageable dead wood"= 2),
                                            selected = 1),
                               bsTooltip("radio_dead", "Slider provides percentage of existing deadwood to also be removed as part of any effort (default is 0).",
                                         "right", options = list(container = "body")),
                               radioButtons("radio_road", h3("Distance from Road"),
                                            choices = list("Total" = 1,
                                                           "100 ft or less" = 2,
                                                           "300 ft or less" = 3,
                                                           "500 ft or less" = 4,
                                                           "1000 ft or less" = 5,
                                                           "1/2 mile or less" = 6,
                                                           "1 mile or less" = 7,
                                                           "3 miles or less" = 8,
                                                           "5 miles or less" = 9),
                                            selected = 1),
                               bsTooltip("radio_road", "Calculations for volume within specific distance of accessible roads.",
                                         "right", options = list(container = "body")),
                               radioButtons("radio_units", h3("Unit of Carbon Measurement"),
                                            choices = list("Bone Dry Tons (BDT)" = 1, 
                                                           "CO2 Equivalent (CO2e)" = 2),
                                            selected = 1),
                               # Pop-up text explanation
                               bsTooltip("radio_units", "Translation assumed to be 1 BDT CO2 = 1.835 MTCO2e",
                                         "right", options = list(container = "body"))#,
                               #downloadButton("report", "Generate report")
                             )
                )
  )
)

# Now develop the server - this is the code that decides what happens once the 
# user input is gathered from the above layout
server <- function(input, output) {
  shinyalert(
    title = "Welcome",
    text = "The Carbon Containment Lab's Woody Biomass Calculator",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  # Output showing county by county Total Loss
  output$USMap <- renderPlot({
    # Initialize empty data
    rm(list = ls())
    
    # For testing purposes
    # input <- list("radio_region" = c("WA", "CA", "OR", "ID", "NV", "AZ", "NM"),
    #              "radio_acre" = 1, "radio_pool" = 5, "radio_method" = 2, 
    #              "radio_fire" = 4, "slider_fire" = 1, "radio_dead" = 1, 
    #              "radio_road" = 1, "radio_units" = 1, "radio_reserved" = 2)
    
    x <- NA
    
    ## USER NOTE: All vestigial code is commented out, in case we want to 
    ## implement it again in a later version of this build
    
    # Read in and clean all necessary documents, removing the white space and 
    # giving the columns appropriate names
    mort <- read_excel("WAORCA_Mortality_Overstock_Road.xlsx")
    names(mort) <- mort[1, ]
    names(mort)[1] <- "county"
    mort <- mort[3:nrow(mort), -12]
    mort[mort == "-"] <- "0"
    mort[, 2:11] <- sapply(mort[, 2:11], as.numeric)
    
    growth <- read_excel("WAORCA_Growth_Overstock_Road.xlsx")
    names(growth) <- growth[1, ]
    names(growth)[1] <- "county"
    growth <- growth[3:nrow(growth), -12]
    growth[growth == "-"] <- "0"
    growth[, 2:11] <- sapply(growth[, 2:11], as.numeric)
    
    if (input$radio_pool == 1) {
      biomass <- read_excel("FULL_Biomass_Overstock_Road.xlsx")
    } else if (input$radio_pool == 2) {
      biomass <- read_excel("FULL_Biomass_Less5.xlsx")
    } else if (input$radio_pool == 3) {
      biomass <- read_excel("FULL_Biomass_Less9.xlsx")
    } else if (input$radio_pool == 4) {
      biomass <- read_excel("FULL_Biomass_Less12.xlsx")
    } else if (input$radio_pool == 5) {
      biomass <- read_excel("FULL_Biomass_Tops.xlsx")
    }
    
    
    colnames(biomass)[1] <- "county"
    biomass <- biomass[2:nrow(biomass),]
    biomass[biomass == "-"] <- "0"
    biomass[is.na(biomass)] <- 0
    biomass[, 2:11] <- sapply(biomass[, 2:11], as.numeric)
    
    # Create removals for slope and NPS ########################################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% biomass$county, ]
    
    slp$ex_ratio <-  (slp$`61-80 percent` +  slp$`41-60 percent` + 
                       slp$`81-100 percent` + slp$`100+ percent`) / slp$Total
    
    nps <- read_excel("FULL_Biomass_Ownership.xlsx")
    nps <- nps[2:nrow(nps), ]
    nps[nps == "-"] <- "0"
    nps[, 2:ncol(nps)] <- sapply(nps[, 2:ncol(nps)], as.numeric)
    names(nps)[1] <- "county" 
    nps <- nps[nps$county %in% biomass$county, ]
    nps$protected <- nps$`National Park Service` / nps$Total
    
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - nps$protected)
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - slp$ex_ratio)
    ############################################################################
    
    reg <- read_excel("FULL_Biomass_Regular_Road.xlsx")
    colnames(reg)[1] <- "county"
    reg <- reg[2:nrow(reg),]
    reg[reg == "-"] <- "0"
    reg[, 2:11] <- sapply(reg[, 2:11], as.numeric)
    
    bio_tot <- read_excel("FULL_Biomass_Total_Road.xlsx")
    colnames(bio_tot)[1] <- "county"
    bio_tot <- bio_tot[2:nrow(bio_tot),]
    bio_tot[bio_tot == "-"] <- "0"
    bio_tot[, 2:11] <- sapply(bio_tot[, 2:11], as.numeric)
    
    dead <- read_excel("FULL_Biomass_Dead_Decay.xlsx")
    dead <- dead[2:nrow(dead), ]
    names(dead) <- c("county", "dead_wood", "class5", "class4", "class3", 
                     "class2", "class1")
    dead[dead == "-"] <- "0"
    dead[, 2:7] <- sapply(dead[, 2:7], as.numeric)
    dead$dead_wood <- dead$class2 + dead$class1
    dead <- dead[, 1:2]
    
    acre <- read_excel("FULL_Overstock_Acre.xlsx")
    colnames(acre) <- acre[1, ]
    colnames(acre)[1] <- "county"
    acre <- acre[3:nrow(acre),]
    acre[acre == "-"] <- "0"
    acre[, 2:11] <- sapply(acre[, 2:11], as.numeric)
    names(acre)[2] <- "total_acres"
    
    fire <- read_excel("Wildfire_Data.xlsx")
    fire <- fire[fire$STATE %in% c("Arizona", "California", "Colorado", "Idaho", 
                                   "Montana", "Nevada", "New Mexico", "Oregon",
                                   "Utah", "Washington", "Wyoming"), ]
    names(fire)[names(fire) == "GEOID"] <- "fips"
    
    # Figure out where mortality > growth
    bad_mortality <- mort$county[mort$Total - growth$Total > 0]
    
    biomass$flag <- ifelse(biomass$county %in% bad_mortality, 1, 0)
    biomass$fips <- substr(biomass$county, 1, 5)
    biomass$fips <- str_remove(biomass$fips, "^0+")
    # plot_usmap(include = c("CA", "WA", "OR"), regions = "county",
    #            data = biomass[, c("fips", "flag")],
    #            values = "flag", color = "black") +
    #   labs(title = "Counties Where Mortality Exceeds Growth",
    #        caption = "Source: FIA") +
    #   scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                               "Millions MTCO2e"), low = "white",
    #                        mid = "white", high = "red", midpoint = 0, space = "Lab",
    #                        na.value = "grey75", guide = "colourbar",
    #                        aesthetics = "fill", label = scales::comma) +
    #   theme(legend.position = "") +
    #   theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
    # FILTER STEPS BASED ON INPUT ##############################################
    
    # Filter for method of carbon removal
    if (input$radio_method == 1) {
      biomass[, 2:11] <- biomass[, 2:11] + .4 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 2) {
      biomass[, 2:11] <- biomass[, 2:11] + .2 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 3) {
      biomass <- biomass
    }
    
    # Filter for mortality selection
    # if(input$radio_mort == 2) {
    #   biomass <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Filter for region
    grep_string <- paste0(input$radio_region, collapse = " | ")
    x <- biomass[grep(grep_string, biomass$county), ]
    acre <- acre[grep(grep_string, biomass$county), ]
    
    # if (input$radio_region == 1) {
    #   x <- biomass
    # } else if (input$radio_region == 2) {
    #   x <- biomass[grep(" WA | OR ", biomass$county), ]
    # } else if (input$radio_region == 3) {
    #   x <- biomass[grep(" WA ", biomass$county), ]
    # } else if (input$radio_region == 4) {
    #   x <- biomass[grep(" OR ", biomass$county), ]
    # } else if (input$radio_region == 5) {
    #   x <- biomass[grep(" CA ", biomass$county), ]
    # } else if (input$radio_region == 6) {
    #   x <- biomass[grep(" ID ", biomass$county), ]
    # } else if (input$radio_region == 7) {
    #   x <- biomass[grep(" NV ", biomass$county), ]
    # } else if (input$radio_region == 8) {
    #   x <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Second, filter for distance from road
    if (input$radio_road == 1) {
      x <- x[, 1:2]
      acre <- acre[, 1:2]
    } else if (input$radio_road == 2) {
      x$Total <- x$`100 ft or less`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 3) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 4) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 5) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 6) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 7) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile` + x$`1/2 to 1 mile`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile` + acre$`1/2 to 1 mile`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 8) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`+ x$`1/2 to 1 mile` + x$`1 to 3 miles`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`+ acre$`1/2 to 1 mile` + acre$`1 to 3 miles`
      acre <- acre[, 1:2]
    } else if (input$radio_road == 9) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`+ x$`1/2 to 1 mile` + x$`1 to 3 miles` + x$`3 to 5 miles`
      x <- x[, 1:2]
      
      acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`+ acre$`1/2 to 1 mile` + acre$`1 to 3 miles` + acre$`3 to 5 miles`
      acre <- acre[, 1:2]
    }
    
    # # Third, filter for Mortality Exceeding Growth
    # if (input$radio_mort == 1) {
    #   x <- x
    # } else if (input$radio_mort == 2) {
    #   x <- x[x$county %in% bad_mortality, ]
    #   acre <- acre[acre$county %in% bad_mortality]
    # }
    
    # Next, filter for fire percentile type
    if (input$radio_fire == 1) {
      fire <- fire[, c("fips", "Mean BP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 2) {
      fire <- fire[, c("fips", "Mean BP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 3) {
      fire <- fire[, c("fips", "Mean WHP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 4) {
      fire <- fire[, c("fips", "Mean WHP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } 
    
    # Fourth, filter for fire percentile
    x$fips <- substr(x$county, 1, 5)
    x$fips <- str_remove(x$fips, "^0+")
    x <- merge(x, fire, by = "fips")
    x$Total[x$measurement <= input$slider_fire / 100] <- 0
    x$Total <- x$Total / 1000000
    
    # Filter for removal of reserved wood
    removal <- read_excel("FULL_Biomass_ReservedStatus.xlsx")
    removal <- removal[2:nrow(removal), ]
    names(removal) <- c("county", "total", "not_reserved", "reserved")
    removal[removal == "-"] <- "0"
    removal[, 2:ncol(removal)] <- sapply(removal[, 2:ncol(removal)], as.numeric)
    removal$rem_ratio <- removal$not_reserved / removal$total
    removal <- removal[removal$county %in% x$county, ]
    if (input$radio_reserved == 2) {
      x$Total[x$county %in% removal$county] <- x$Total[x$county %in% removal$county] * 
        removal$rem_ratio
    }
    
    # Next, add in dead wood if appropriate
    dead <- dead[dead$county %in% x$county, ]
    dead$dead_wood <- dead$dead_wood / 1000000
    if (input$radio_dead == 1) {
      x$Total[x$county %in% dead$county] <- x$Total[x$county %in% dead$county] + 
        dead$dead_wood 
    } else if (input$radio_dead == 2) {
      x$Total <- x$Total
    }
    
    # Sixth, filter for unit
    if (input$radio_units == 1) {
      x <- x
    } else if (input$radio_units == 2) {
      x$Total <- x$Total * 1.835
    }
    
    # Fifth, change for per acre or total
    if (input$radio_acre == 1) {
      x <- x
    } else if (input$radio_acre == 2) {
      x <- merge(x, acre, by = "county")
      x$total_acres[x$total_acres == 0] <- mean(x$total_acres)
      x$Total <- x$Total / x$total_acres * 1000000
    }
    
    x_first_plot <- x
    
    # Produce resulting plot
    p1 <- plot_usmap(include = input$radio_region, regions = "county",
                     data = x[, c("fips", "Total")],
                     values = "Total", color = "black") +
      labs(title = ifelse(input$radio_units == 1,  "Overstocked Biomass",
                          "Overstocked MTCO2"),
           subtitle = ifelse(input$radio_units == 1,  ifelse(input$radio_acre == 1, "Millions Short Dry Tons of Biomass", "Short Dry Tons of Biomass"),
                             ifelse(input$radio_acre == 1, "MMTCO2e", "MTCO2e")),
           caption = "Source: FIA") +
      scale_fill_gradient2(ifelse(input$radio_units == 1,  ifelse(input$radio_acre == 1, "Million Dry Tons", "Dry Tons"),
                                  ifelse(input$radio_acre == 1, "MMTCO2e", "MTCO2e")), low = "white",
                           mid = "white", high = "#065735", midpoint = 2, space = "Lab",
                           na.value = "grey75", guide = "colourbar",
                           aesthetics = "fill", label = scales::comma) +
      theme(legend.position = "right") +
      theme(panel.background = element_rect(color = "black", fill = "lightblue")) 
    
    
    p1b <- plot_usmap(include = input$radio_region, regions = "county",
                      data = x[, c("fips", "measurement")],
                      values = "measurement", color = "black") +
      labs(title = "Fire Risk of Included Counties",
           subtitle = "Relative Fire Risk for Qualified Counties",
           caption = "Source: FIA") +
      scale_fill_gradient2(ifelse(input$radio_units == 1,  "Percentile",
                                  "Percentile"), low = "white", high = "dark red", space = "Lab",
                           na.value = "grey75", guide = "colourbar",
                           aesthetics = "fill", label = scales::comma) +
      theme(legend.position = "right") +
      theme(panel.background = element_rect(color = "black", fill = "lightblue")) 
    
    out <- aggregate(x$Total, by=list(Group=x$state), FUN=sum)
    x$state <- factor(x$state, levels = out$Group[order(out$x)])
    
    if (input$radio_acre == 2) {
      
      x_summary <- x %>% 
        group_by(state) %>% 
        summarise(mean_val = mean(Total[Total != 0]),
                  sd_val = sd(Total), 
                  n_val = n(),
                  SE_val = sd(Total)/ sqrt(n()))
      
      p2 <- ggplot(data = x_summary, aes(x = state, y = mean_val, fill = state)) + geom_bar(stat = "identity") +
        theme(legend.position="none") +
        labs(title = ifelse(input$radio_acre == 1,  "Sum of Total Biomass Availability",
                            "Mean of Total Biomass Availability"),
             subtitle = ifelse(input$radio_acre == 1, paste("Total Biomass Availability:", round(sum(x$Total)), 1), ""),
             x = "State",
             y = ifelse(input$radio_units == 1,  ifelse(input$radio_acre == 1, "Millions Dry Tons", "Dry Tons"),
                        ifelse(input$radio_acre == 1, "MMTCO2e", "MTCO2e"))) +
        geom_text(aes(label = round(stat(y), 1), group = state), stat = 'summary', fun = sum, vjust = 4) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
        geom_errorbar(aes(ymin = mean_val-.1*mean_val, ymax = mean_val+.1*mean_val), width = .5, stat = "sum", size = .5)
      grid.arrange(p1, p2, ncol = 2)
      
    } else if (input$radio_acre == 1) {
      x_summary <- x %>% 
        group_by(state) %>% 
        summarise(sum_val = sum(Total),
                  sd_val = sd(Total), 
                  n_val = n(),
                  SE_val = sd(Total)/ sqrt(n()))
      
      p2 <- ggplot(data = x_summary, aes(x = state, y = sum_val, fill = state)) + geom_bar(stat = "identity") +
        theme(legend.position="none") +
        labs(title = ifelse(input$radio_acre == 1,  "Sum of Total Biomass Availability",
                            "Mean of Total Biomass Availability"),
             subtitle = ifelse(input$radio_acre == 1, paste("Total Biomass Availability:", round(sum(x$Total)), 1), ""),
             x = "State",
             y = ifelse(input$radio_units == 1,  ifelse(input$radio_acre == 1, "Millions Dry Tons", "Dry Tons"),
                        ifelse(input$radio_acre == 1, "MMTCO2e", "MTCO2e"))) +
        geom_text(aes(label = round(stat(y), 1), group = state), stat = 'summary', fun = sum, vjust = 4) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
        geom_errorbar(aes(ymin = sum_val-sd_val, ymax = sum_val+sd_val), width = .5, stat = "sum", size = .5)
      grid.arrange(p1, p2, ncol = 2)
    }
    # if (input$radio_region %in% c(1,8)) {
    #   p1 <- plot_usmap(include = c("WA", "OR", "CA", "ID", "NV"), regions = "county",
    #                    data = x[, c("fips", "Total")],
    #                    values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1,  "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 2, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue")) 
    #   
    #   # p2_data <- data.frame("state" = unique(x$state), "sum" = rep(NA, length(unique(x$state))))
    #   # for (i in 1:nrow(p2_data)) p2_data$sum[i] <- sum(x$Total[x$state == p2_data$state[i]])
    #   out <- aggregate(x$Total, by=list(Group=x$state), FUN=sum)
    #   x$state <- factor(x$state, levels = out$Group[order(out$x)])
    #   
    #   p2 <- ggplot(data = x, aes(x = state, y = Total, fill = state)) + geom_bar(stat = "sum") + 
    #     theme(legend.position="none") + 
    #     labs(title = "Sum of Total Carbon Availability",  
    #          subtitle = paste("Total Carbon Availability:", round(sum(x$Total)), 1),
    #          x = "State", 
    #          y = ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                     "Millions MTCO2e")) + 
    #     geom_text(aes(label = round(stat(y), 1), group = state), stat = 'summary', fun = sum, vjust = -1) + 
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #           panel.background = element_blank(), axis.line = element_line(colour = "black"))
    #   grid.arrange(p1, p2, ncol=2)
    #   
    # } else if (input$radio_region == 2) {
    #   p1 <- plot_usmap(include = c("WA", "OR"), regions = "county",
    #                    data = x[, c("fips", "Total")],
    #                    values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1,  "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 2, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    #   
    #   # p2_data <- data.frame("state" = unique(x$state), "sum" = rep(NA, length(unique(x$state))))
    #   # for (i in 1:nrow(p2_data)) p2_data$sum[i] <- sum(x$Total[x$state == p2_data$state[i]])
    #   out <- aggregate(x$Total, by=list(Group=x$state), FUN=sum)
    #   x$state <- factor(x$state, levels = out$Group[order(out$x)])
    #   
    #   p2 <- ggplot(data = x, aes(x = state, y = Total, fill = state)) + geom_bar(stat = "sum") + 
    #     theme(legend.position="none") + 
    #     labs(title = "Sum of Total Carbon Availability", 
    #          subtitle = paste("Total Carbon Availability:", round(sum(x$Total)), 1),
    #          x = "State", 
    #          y = ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                     "Millions MTCO2e")) + 
    #     geom_text(aes(label = round(stat(y), 1), group = state), stat = 'summary', fun = sum, vjust = -1) + 
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #           panel.background = element_blank(), axis.line = element_line(colour = "black"))
    #   grid.arrange(p1, p2, ncol=2)
    # } else if (input$radio_region == 3) {
    #   plot_usmap(include = c("WA"), regions = "county",
    #              data = x[, c("fips", "Total")],
    #              values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1, "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    # } else if (input$radio_region == 4) {
    #   plot_usmap(include = c("OR"), regions = "county",
    #              data = x[, c("fips", "Total")],
    #              values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1, "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    # } else if (input$radio_region == 5) {
    #   plot_usmap(include = c("CA"), regions = "county",
    #              data = x[, c("fips", "Total")],
    #              values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1, "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    # } else if (input$radio_region == 6) {
    #   plot_usmap(include = c("ID"), regions = "county",
    #              data = x[, c("fips", "Total")],
    #              values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1, "Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    # }else if (input$radio_region == 7) {
    #   plot_usmap(include = c("NV"), regions = "county",
    #              data = x[, c("fips", "Total")],
    #              values = "Total", color = "black") +
    #     labs(title = "Overstocked Biomass for Thinning",
    #          subtitle = ifelse(input$radio_units == 1,"Measured in Millions Short Dry Tons of Carbon",
    #                            "Measured in Millions MTCO2e"),
    #          caption = "Source: FIA") +
    #     scale_fill_gradient2(ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Carbon",
    #                                 "=Millions MTCO2e"), low = "#300005",
    #                          mid = "white", high = "#065735", midpoint = 0, space = "Lab",
    #                          na.value = "grey75", guide = "colourbar",
    #                          aesthetics = "fill", label = scales::comma) +
    #     theme(legend.position = "right") +
    #     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    # }
  })
  
  # Output showing county by county Total Loss
  output$RoadPlot <- renderPlot({
    # Initialize empty data
    x <- NA
    
    ## USER NOTE: All vestigial code is commented out, in case we want to 
    ## implement it again in a later version of this build
    
    # Read in and clean all necessary documents, removing the white space and 
    # giving the columns appropriate names
    mort <- read_excel("WAORCA_Mortality_Overstock_Road.xlsx")
    names(mort) <- mort[1, ]
    names(mort)[1] <- "county"
    mort <- mort[3:nrow(mort), -12]
    mort[mort == "-"] <- "0"
    mort[, 2:11] <- sapply(mort[, 2:11], as.numeric)
    
    growth <- read_excel("WAORCA_Growth_Overstock_Road.xlsx")
    names(growth) <- growth[1, ]
    names(growth)[1] <- "county"
    growth <- growth[3:nrow(growth), -12]
    growth[growth == "-"] <- "0"
    growth[, 2:11] <- sapply(growth[, 2:11], as.numeric)
    
    if (input$radio_pool == 1) {
      biomass <- read_excel("FULL_Biomass_Overstock_Road.xlsx")
    } else if (input$radio_pool == 2) {
      biomass <- read_excel("FULL_Biomass_Less5.xlsx")
    } else if (input$radio_pool == 3) {
      biomass <- read_excel("FULL_Biomass_Less9.xlsx")
    } else if (input$radio_pool == 4) {
      biomass <- read_excel("FULL_Biomass_Less12.xlsx")
    } else if (input$radio_pool == 5) {
      biomass <- read_excel("FULL_Biomass_Tops.xlsx")
    }
    
    colnames(biomass)[1] <- "county"
    biomass <- biomass[2:nrow(biomass),]
    biomass[biomass == "-"] <- "0"
    biomass[is.na(biomass)] <- 0
    biomass[, 2:11] <- sapply(biomass[, 2:11], as.numeric)
    
    # Create removals for slope and NPS ########################################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% biomass$county, ]
    
    slp$ex_ratio <-  (slp$`61-80 percent` + slp$`41-60 percent` + 
                        slp$`81-100 percent` + slp$`100+ percent`) / slp$Total
    
    nps <- read_excel("FULL_Biomass_Ownership.xlsx")
    nps <- nps[2:nrow(nps), ]
    nps[nps == "-"] <- "0"
    nps[, 2:ncol(nps)] <- sapply(nps[, 2:ncol(nps)], as.numeric)
    names(nps)[1] <- "county" 
    nps <- nps[nps$county %in% biomass$county, ]
    nps$protected <- nps$`National Park Service` / nps$Total
    
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - nps$protected)
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - slp$ex_ratio)
    ############################################################################
    
    reg <- read_excel("FULL_Biomass_Regular_Road.xlsx")
    colnames(reg)[1] <- "county"
    reg <- reg[2:nrow(reg),]
    reg[reg == "-"] <- "0"
    reg[, 2:11] <- sapply(reg[, 2:11], as.numeric)
    
    acre <- read_excel("FULL_Overstock_Acre.xlsx")
    colnames(acre) <- acre[1, ]
    colnames(acre)[1] <- "county"
    acre <- acre[3:nrow(acre),]
    acre[acre == "-"] <- "0"
    acre[, 2:11] <- sapply(acre[, 2:11], as.numeric)
    names(acre)[2] <- "total_acres"
    
    bio_tot <- read_excel("FULL_Biomass_Total_Road.xlsx")
    colnames(bio_tot)[1] <- "county"
    bio_tot <- bio_tot[2:nrow(bio_tot),]
    bio_tot[bio_tot == "-"] <- "0"
    bio_tot[, 2:11] <- sapply(bio_tot[, 2:11], as.numeric)
    
    dead <- read_excel("FULL_Biomass_Dead_Decay.xlsx")
    dead <- dead[2:nrow(dead), ]
    names(dead) <- c("county", "dead_wood", "class5", "class4", "class3", 
                     "class2", "class1")
    dead[dead == "-"] <- "0"
    dead[, 2:7] <- sapply(dead[, 2:7], as.numeric)
    dead$dead_wood <-  dead$class2 + dead$class1
    dead <- dead[, 1:2]
    
    fire <- read_excel("Wildfire_Data.xlsx")
    fire <- fire[fire$STATE %in% c("Arizona", "California", "Colorado", "Idaho",
                                   "Montana", "Nevada", "New Mexico", "Oregon",
                                   "Utah", "Washington", "Wyoming"), ]
    names(fire)[names(fire) == "GEOID"] <- "fips"
    
    # Figure out where mortality > growth
    bad_mortality <- mort$county[mort$Total - growth$Total > 0]
    
    biomass$flag <- ifelse(biomass$county %in% bad_mortality, 1, 0)
    biomass$fips <- substr(biomass$county, 1, 5)
    biomass$fips <- str_remove(biomass$fips, "^0+")
    
    # FILTER STEPS BASED ON INPUT ##############################################
    
    # Filter for method of carbon removal
    if (input$radio_method == 1) {
      biomass[, 2:11] <- biomass[, 2:11] + .4 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 2) {
      biomass[, 2:11] <- biomass[, 2:11] + .2 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 3) {
      biomass <- biomass
    }
    
    # Filter for mortality selection
    # if(input$radio_mort == 2) {
    #   biomass <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Filter for region
    grep_string <- paste0(input$radio_region, collapse = " | ")
    x <- biomass[grep(grep_string, biomass$county), ]
    
    # Next, filter for fire percentile type
    if (input$radio_fire == 1) {
      fire <- fire[, c("fips", "Mean BP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 2) {
      fire <- fire[, c("fips", "Mean BP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 3) {
      fire <- fire[, c("fips", "Mean WHP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 4) {
      fire <- fire[, c("fips", "Mean WHP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } 
    
    # Fourth, filter for fire percentile
    x$fips <- substr(x$county, 1, 5)
    x$fips <- str_remove(x$fips, "^0+")
    x <- merge(x, fire, by = "fips")
    x <- x[x$measurement >= input$slider_fire / 100, ]
    x[, 3:12] <- x[, 3:12] / 1000000
    
    # Filter for removal of reserved wood
    removal <- read_excel("FULL_Biomass_ReservedStatus.xlsx")
    removal <- removal[2:nrow(removal), ]
    names(removal) <- c("county", "total", "not_reserved", "reserved")
    removal[removal == "-"] <- "0"
    removal[, 2:ncol(removal)] <- sapply(removal[, 2:ncol(removal)], as.numeric)
    removal$rem_ratio <- removal$not_reserved / removal$total
    removal <- removal[removal$county %in% x$county, ]
    if (input$radio_reserved == 2) {
      x$Total[x$county %in% removal$county] <- x$Total[x$county %in% removal$county] * 
        removal$rem_ratio
    }
    
    # Next, add in dead wood if appropriate
    dead <- dead[dead$county %in% x$county, ]
    dead$dead_wood <- dead$dead_wood / 1000000
    if (input$radio_dead == 1) {
      x$Total[x$county %in% dead$county] <- x$Total[x$county %in% dead$county] + 
        dead$dead_wood  
    } else if (input$radio_dead == 2) {
      x$Total <- x$Total
    }
    
    # Sixth, filter for unit
    if (input$radio_units == 1) {
      x <- x
    } else if (input$radio_units == 2) {
      x[, 3:12] <- x[, 3:12] * 1.835
    }
    
    gg_frame <- data.frame(matrix(nrow = length(levels(as.factor(x$state))), ncol = 10))
    colnames(gg_frame) <- c("state", colnames(x)[4:12])
    gg_frame$state <- levels(as.factor(x$state))
    
    for (state in levels(as.factor(x$state))) {
      temp <- x[x$state == state, ]
      temp_sums <- colSums(temp[, 4:12])
      gg_frame[gg_frame$state == state, 2:10] <- cumsum(temp_sums)
    }
    
    gg_frame_use <- data.frame("state" = rep(gg_frame$state, 9),
                               "vals" = unlist(gg_frame[, 2:10]),
                               "labels" = rep(colnames(gg_frame)[2:10], each = nrow(gg_frame)))
    gg_frame_use$labels <- factor(gg_frame_use$labels, 
                                  levels = c("100 ft or less", "101-300 ft", "301-500 ft",
                                             "501-1000 ft", "1001 ft to 1/2 mile", "1/2 to 1 mile",
                                             "1 to 3 miles", "3 to 5 miles", "Greater than 5 miles"))
    
    out <- aggregate(gg_frame_use$vals, by=list(Group=gg_frame_use$state), FUN=sum)
    gg_frame_use$state <- factor(gg_frame_use$state, levels = out$Group[order(out$x)])
    
    # Select for error bars
    road_names <- unique(gg_frame_use$labels)
    gg_frame_use$error_bars <- NA
    if (input$radio_road == 1) {
      gg_frame_use$error_bars[gg_frame_use$labels == "Greater than 5 miles"] <- 
        sd(gg_frame_use$vals[gg_frame_use$labels == "Greater than 5 miles"]) / 4
    } else {
      gg_frame_use$error_bars[gg_frame_use$labels == road_names[input$radio_road]] <-
        sd(gg_frame_use$vals[gg_frame_use$labels == road_names[input$radio_road]]) / 4
    }
    
    levels(gg_frame_use$labels) <- c("<100 ft", "<300 ft", "<500 ft", "<1000 ft", 
                                     "<1/2 mile", "<1 mile", "<3 miles", "<5 miles", 
                                     ">5 miles")
    
    ggplot(gg_frame_use, aes(x = labels, y = vals, group = state, color = state)) + 
      geom_line(size = 2) + 
      labs(title = "Cumulative Amount of Wood by State", subtitle = "Sorted by Distance from Road",
           x = "Distance from Road", y = ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                                "Millions MTCO2e"),
           color = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text = element_text(size=15), axis.title = element_text(size = 15),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
      geom_errorbar(aes(ymin=vals-error_bars, ymax=vals + error_bars), width=.2, size = 1.5)
    
  })
  
  # Output showing county by county Total Loss
  output$BurnPlot <- renderPlot({
    # Initialize empty data
    x <- NA
    
    ## USER NOTE: All vestigial code is commented out, in case we want to 
    ## implement it again in a later version of this build
    
    # Read in and clean all necessary documents, removing the white space and 
    # giving the columns appropriate names
    mort <- read_excel("WAORCA_Mortality_Overstock_Road.xlsx")
    names(mort) <- mort[1, ]
    names(mort)[1] <- "county"
    mort <- mort[3:nrow(mort), -12]
    mort[mort == "-"] <- "0"
    mort[, 2:11] <- sapply(mort[, 2:11], as.numeric)
    
    growth <- read_excel("WAORCA_Growth_Overstock_Road.xlsx")
    names(growth) <- growth[1, ]
    names(growth)[1] <- "county"
    growth <- growth[3:nrow(growth), -12]
    growth[growth == "-"] <- "0"
    growth[, 2:11] <- sapply(growth[, 2:11], as.numeric)
    
    if (input$radio_pool == 1) {
      biomass <- read_excel("FULL_Biomass_Overstock_Road.xlsx")
    } else if (input$radio_pool == 2) {
      biomass <- read_excel("FULL_Biomass_Less5.xlsx")
    } else if (input$radio_pool == 3) {
      biomass <- read_excel("FULL_Biomass_Less9.xlsx")
    } else if (input$radio_pool == 4) {
      biomass <- read_excel("FULL_Biomass_Less12.xlsx")
    } else if (input$radio_pool == 5) {
      biomass <- read_excel("FULL_Biomass_Tops.xlsx")
    }
    
    colnames(biomass)[1] <- "county"
    biomass <- biomass[2:nrow(biomass),]
    biomass[biomass == "-"] <- "0"
    biomass[is.na(biomass)] <- 0
    biomass[, 2:11] <- sapply(biomass[, 2:11], as.numeric)
    
    # Create removals for slope and NPS ########################################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% biomass$county, ]
    
    slp$ex_ratio <-  (slp$`61-80 percent` + slp$`41-60 percent` + 
                        slp$`81-100 percent` + slp$`100+ percent`) / slp$Total
    
    nps <- read_excel("FULL_Biomass_Ownership.xlsx")
    nps <- nps[2:nrow(nps), ]
    nps[nps == "-"] <- "0"
    nps[, 2:ncol(nps)] <- sapply(nps[, 2:ncol(nps)], as.numeric)
    names(nps)[1] <- "county" 
    nps <- nps[nps$county %in% biomass$county, ]
    nps$protected <- nps$`National Park Service` / nps$Total
    
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - nps$protected)
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - slp$ex_ratio)
    ############################################################################
    
    reg <- read_excel("FULL_Biomass_Regular_Road.xlsx")
    colnames(reg)[1] <- "county"
    reg <- reg[2:nrow(reg),]
    reg[reg == "-"] <- "0"
    reg[, 2:11] <- sapply(reg[, 2:11], as.numeric)
    
    acre <- read_excel("FULL_Overstock_Acre.xlsx")
    colnames(acre) <- acre[1, ]
    colnames(acre)[1] <- "county"
    acre <- acre[3:nrow(acre),]
    acre[acre == "-"] <- "0"
    acre[, 2:11] <- sapply(acre[, 2:11], as.numeric)
    names(acre)[2] <- "total_acres"
    
    bio_tot <- read_excel("FULL_Biomass_Total_Road.xlsx")
    colnames(bio_tot)[1] <- "county"
    bio_tot <- bio_tot[2:nrow(bio_tot),]
    bio_tot[bio_tot == "-"] <- "0"
    bio_tot[, 2:11] <- sapply(bio_tot[, 2:11], as.numeric)
    
    dead <- read_excel("FULL_Biomass_Dead_Decay.xlsx")
    dead <- dead[2:nrow(dead), ]
    names(dead) <- c("county", "dead_wood", "class5", "class4", "class3", 
                     "class2", "class1")
    dead[dead == "-"] <- "0"
    dead[, 2:7] <- sapply(dead[, 2:7], as.numeric)
    dead$dead_wood <-  dead$class2 + dead$class1
    dead <- dead[, 1:2]
    
    fire <- read_excel("Wildfire_Data.xlsx")
    fire <- fire[fire$STATE %in% c("Arizona", "California", "Colorado", "Idaho",
                                   "Montana", "Nevada", "New Mexico", "Oregon",
                                   "Utah", "Washington", "Wyoming"), ]
    names(fire)[names(fire) == "GEOID"] <- "fips"
    
    # Figure out where mortality > growth
    bad_mortality <- mort$county[mort$Total - growth$Total > 0]
    
    biomass$flag <- ifelse(biomass$county %in% bad_mortality, 1, 0)
    biomass$fips <- substr(biomass$county, 1, 5)
    biomass$fips <- str_remove(biomass$fips, "^0+")
    
    # FILTER STEPS BASED ON INPUT ##############################################
    
    # Filter for method of carbon removal
    if (input$radio_method == 1) {
      biomass[, 2:11] <- biomass[, 2:11] + .4 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 2) {
      biomass[, 2:11] <- biomass[, 2:11] + .2 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 3) {
      biomass <- biomass
    }
    
    # Filter for mortality selection
    # if(input$radio_mort == 2) {
    #   biomass <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Filter for region
    grep_string <- paste0(input$radio_region, collapse = " | ")
    x <- biomass[grep(grep_string, biomass$county), ]
    
    # Next, filter for fire percentile type
    if (input$radio_fire == 1) {
      fire <- fire[, c("fips", "Mean BP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 2) {
      fire <- fire[, c("fips", "Mean BP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 3) {
      fire <- fire[, c("fips", "Mean WHP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 4) {
      fire <- fire[, c("fips", "Mean WHP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } 
    
    # Fourth, filter for fire percentile
    x$fips <- substr(x$county, 1, 5)
    x$fips <- str_remove(x$fips, "^0+")
    x <- merge(x, fire, by = "fips")
    x[, 3:12] <- x[, 3:12] / 1000000
    
    # Filter for removal of reserved wood
    removal <- read_excel("FULL_Biomass_ReservedStatus.xlsx")
    removal <- removal[2:nrow(removal), ]
    names(removal) <- c("county", "total", "not_reserved", "reserved")
    removal[removal == "-"] <- "0"
    removal[, 2:ncol(removal)] <- sapply(removal[, 2:ncol(removal)], as.numeric)
    removal$rem_ratio <- removal$not_reserved / removal$total
    removal <- removal[removal$county %in% x$county, ]
    if (input$radio_reserved == 2) {
      x$Total[x$county %in% removal$county] <- x$Total[x$county %in% removal$county] * 
        removal$rem_ratio
    }
    
    # Next, add in dead wood if appropriate
    dead <- dead[dead$county %in% x$county, ]
    dead$dead_wood <- dead$dead_wood / 1000000
    if (input$radio_dead == 1) {
      x$Total[x$county %in% dead$county] <- x$Total[x$county %in% dead$county] + 
        dead$dead_wood  
    } else if (input$radio_dead == 2) {
      x$Total <- x$Total
    }
    
    # Sixth, filter for unit
    if (input$radio_units == 1) {
      x <- x
    } else if (input$radio_units == 2) {
      x[, 3:12] <- x[, 3:12] * 1.835
    }
    
    gg_frame <- data.frame("Distance" = rep(c(seq(0, 1.0, by = 0.1)), length(input$radio_region)),
                           "State" = rep(input$radio_region, each = 11),
                           "Value" = rep(0, 11*length(input$radio_region)))
    x$Total <- x$Total / 1000000
    
    for (i in 1:(11*length(input$radio_region))) {
      temp <- x[grep(paste0(" ", gg_frame$State[i], " "), x$county), ]
      temp <- temp[temp$measurement <= (1-gg_frame$Distance[i]), ]
      gg_frame$Value[i] <- sum(temp$Total)
    }
    
    fire_choices <- c("Burn Probability Percentile (state)",
                      "Burn Probability (nation)",
                      "Wildfire Hazard Potential (state)",
                      "Wildfire Hazard (nation)")
    
    
    out <- aggregate(gg_frame$Value, by=list(Group=gg_frame$State), FUN=sum)
    gg_frame$State <- factor(gg_frame$State, levels = out$Group[order(out$x)])
    
    gg_third_plot <- gg_frame
    
    # Select for error bars
    gg_third_plot$errors <- NA
    
    gg_third_plot$errors[as.character(gg_third_plot$Distance) == as.character(round(input$slider_fire / 100, 1))] <- 
      sd(gg_third_plot$Value[as.character(gg_third_plot$Distance) == as.character(round(input$slider_fire / 100, 1))])
    
    ggplot(gg_third_plot, aes(x = Distance, y = Value, group = State, color = State)) + 
      geom_line(size = 2) + 
      labs(title = "Cumulative Amount of Wood", subtitle = "Sorted by Selected Fire Criteria",
           x = "Fire Percentile", y = ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                             "Millions MTCO2e"),
           color = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
      geom_errorbar(aes(ymin=Value-errors, ymax=Value + errors), width=.02, size = 1.5)
  })
  
  # Output showing county by county Total Loss
  output$StackedBar <- renderPlot({
    # Initialize empty data
    x <- NA
    
    ## USER NOTE: All vestigial code is commented out, in case we want to 
    ## implement it again in a later version of this build
    
    # Read in and clean all necessary documents, removing the white space and 
    # giving the columns appropriate names
    mort <- read_excel("WAORCA_Mortality_Overstock_Road.xlsx")
    names(mort) <- mort[1, ]
    names(mort)[1] <- "county"
    mort <- mort[3:nrow(mort), -12]
    mort[mort == "-"] <- "0"
    mort[, 2:11] <- sapply(mort[, 2:11], as.numeric)
    
    growth <- read_excel("WAORCA_Growth_Overstock_Road.xlsx")
    names(growth) <- growth[1, ]
    names(growth)[1] <- "county"
    growth <- growth[3:nrow(growth), -12]
    growth[growth == "-"] <- "0"
    growth[, 2:11] <- sapply(growth[, 2:11], as.numeric)
    
    if (input$radio_pool == 1) {
      biomass <- read_excel("FULL_Biomass_Overstock_Road.xlsx")
    } else if (input$radio_pool == 2) {
      biomass <- read_excel("FULL_Biomass_Less5.xlsx")
    } else if (input$radio_pool == 3) {
      biomass <- read_excel("FULL_Biomass_Less9.xlsx")
    } else if (input$radio_pool == 4) {
      biomass <- read_excel("FULL_Biomass_Less12.xlsx")
    } else if (input$radio_pool == 5) {
      biomass <- read_excel("FULL_Biomass_Tops.xlsx")
    }
    
    colnames(biomass)[1] <- "county"
    biomass <- biomass[2:nrow(biomass),]
    biomass[biomass == "-"] <- "0"
    biomass[is.na(biomass)] <- 0
    biomass[, 2:11] <- sapply(biomass[, 2:11], as.numeric)
    
    # Create removals for slope and NPS ########################################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% biomass$county, ]
    
    slp$ex_ratio <-  (slp$`61-80 percent` +  slp$`41-60 percent` + 
                        slp$`81-100 percent` + slp$`100+ percent`) / slp$Total
    
    nps <- read_excel("FULL_Biomass_Ownership.xlsx")
    nps[nps == "-"] <- "0"
    nps[, 2:ncol(nps)] <- sapply(nps[, 2:ncol(nps)], as.numeric)
    names(nps)[1] <- "county" 
    nps <- nps[nps$county %in% biomass$county, ]
    nps$protected <- nps$`National Park Service` / nps$Total
    
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - nps$protected)
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - slp$ex_ratio)
    ############################################################################
    
    reg <- read_excel("FULL_Biomass_Regular_Road.xlsx")
    colnames(reg)[1] <- "county"
    reg <- reg[2:nrow(reg),]
    reg[reg == "-"] <- "0"
    reg[, 2:11] <- sapply(reg[, 2:11], as.numeric)
    
    bio_tot <- read_excel("FULL_Biomass_Total_Road.xlsx")
    colnames(bio_tot)[1] <- "county"
    bio_tot <- bio_tot[2:nrow(bio_tot),]
    bio_tot[bio_tot == "-"] <- "0"
    bio_tot[, 2:11] <- sapply(bio_tot[, 2:11], as.numeric)
    
    dead <- read_excel("FULL_Biomass_Dead_Decay.xlsx")
    dead <- dead[2:nrow(dead), ]
    names(dead) <- c("county", "dead_wood", "class5", "class4", "class3", 
                     "class2", "class1")
    dead[dead == "-"] <- "0"
    dead[, 2:7] <- sapply(dead[, 2:7], as.numeric)
    dead$dead_wood <-   dead$class2 + dead$class1
    dead <- dead[, 1:2]
    
    # acre <- read_excel("WAORCAIDNV_Overstock_Acre.xlsx")
    # colnames(acre) <- acre[1, ]
    # colnames(acre)[1] <- "county"
    # acre <- acre[3:nrow(acre),]
    # acre[acre == "-"] <- "0"
    # acre[, 2:11] <- sapply(acre[, 2:11], as.numeric)
    # names(acre)[2] <- "total_acres"
    
    fire <- read_excel("Wildfire_Data.xlsx")
    fire <- fire[fire$STATE %in% c("Arizona", "California", "Idaho", "Montana", "Nevada", "New Mexico",
                                   "Oregon", "Utah", "Washington", "Wyoming"), ]
    names(fire)[names(fire) == "GEOID"] <- "fips"
    
    if (input$radio_pool == 1) {
      size <- read_excel("FULL_Biomass_Overstock_Size.xlsx")
      colnames(size) <- size[1, ]
      colnames(size)[1] <- "county"
      size <- size[3:nrow(size),]
      size[size == "-"] <- "0"
      size[, 2:23] <- sapply(size[, 2:23], as.numeric)
      size$small <- size$`1.0-2.9` + size$`3.0-4.9` + size$`5.0-6.9` + size$`7.0-8.9` + size$`9.0-10.9` + size$`11.0-12.9`
      size$medium <- size$`13.0-14.9` + size$`15.0-16.9` + size$`17.0-18.9` + size$`19.0-20.9`
      size$large <- size$`21.0-22.9` + size$`23.0-24.9` + size$`25.0-26.9` + size$`27.0-28.9` + size$`29.0-30.9` + 
        size$`31.0-32.9` + size$`33.0-34.9` + size$`35.0-36.9` + size$`37.0-38.9` + size$`39.0-40.9` + size$`41.0+`
      size <- size[, c("county", "small", "medium", "large")]
    } else if (input$radio_pool == 2) {
      size <- read_excel("FULL_Biomass_Overstock_Size.xlsx")
      colnames(size) <- size[1, ]
      colnames(size)[1] <- "county"
      size <- size[3:nrow(size),]
      size[size == "-"] <- "0"
      size[, 2:23] <- sapply(size[, 2:23], as.numeric)
      size$small <- size$`1.0-2.9` + size$`3.0-4.9` + size$`5.0-6.9` + size$`7.0-8.9` + size$`9.0-10.9` + size$`11.0-12.9`
      size$medium <- size$`13.0-14.9` + size$`15.0-16.9` + size$`17.0-18.9` + size$`19.0-20.9`
      size$large <- size$`21.0-22.9` + size$`23.0-24.9` + size$`25.0-26.9` + size$`27.0-28.9` + size$`29.0-30.9` + 
        size$`31.0-32.9` + size$`33.0-34.9` + size$`35.0-36.9` + size$`37.0-38.9` + size$`39.0-40.9` + size$`41.0+`
      size <- size[, c("county", "small")]
    } else if (input$radio_pool == 3) {
      size <- read_excel("FULL_Biomass_Overstock_Size.xlsx")
      colnames(size) <- size[1, ]
      colnames(size)[1] <- "county"
      size <- size[3:nrow(size),]
      size[size == "-"] <- "0"
      size[, 2:23] <- sapply(size[, 2:23], as.numeric)
      size$small <- size$`1.0-2.9` + size$`3.0-4.9` + size$`5.0-6.9` + size$`7.0-8.9` + size$`9.0-10.9` + size$`11.0-12.9`
      size$medium <- size$`13.0-14.9` + size$`15.0-16.9` + size$`17.0-18.9` + size$`19.0-20.9`
      size$large <- size$`21.0-22.9` + size$`23.0-24.9` + size$`25.0-26.9` + size$`27.0-28.9` + size$`29.0-30.9` + 
        size$`31.0-32.9` + size$`33.0-34.9` + size$`35.0-36.9` + size$`37.0-38.9` + size$`39.0-40.9` + size$`41.0+`
      size <- size[, c("county", "small")]
    } else if (input$radio_pool == 4) {
      size <- read_excel("FULL_Biomass_Overstock_Size.xlsx")
      colnames(size) <- size[1, ]
      colnames(size)[1] <- "county"
      size <- size[3:nrow(size),]
      size[size == "-"] <- "0"
      size[, 2:23] <- sapply(size[, 2:23], as.numeric)
      size$small <- size$`1.0-2.9` + size$`3.0-4.9` + size$`5.0-6.9` + size$`7.0-8.9` + size$`9.0-10.9` + size$`11.0-12.9`
      size$medium <- size$`13.0-14.9` + size$`15.0-16.9` + size$`17.0-18.9` + size$`19.0-20.9`
      size$large <- size$`21.0-22.9` + size$`23.0-24.9` + size$`25.0-26.9` + size$`27.0-28.9` + size$`29.0-30.9` + 
        size$`31.0-32.9` + size$`33.0-34.9` + size$`35.0-36.9` + size$`37.0-38.9` + size$`39.0-40.9` + size$`41.0+`
      size <- size[, c("county", "small")]
    } else if (input$radio_pool == 5) {
      # CURRENT MARKER, READ IN SMALL AND BRANCHES AVAILABLE
      tops <- read_excel("FULL_Biomass_Tops.xlsx", sheet = 2)
      twelves <- read_excel("FULL_Biomass_Tops.xlsx", sheet = 3)
      
      tops[is.na(tops)] <- 0
      twelves[is.na(twelves)] <- 0
      
      size <- data.frame("county" = tops$`County code and name`,
                         "tops" = tops$Total, "twelves" = twelves$Total)
    }
    
    # Figure out where mortality > growth
    bad_mortality <- mort$county[mort$Total - growth$Total > 0]
    
    biomass$flag <- ifelse(biomass$county %in% bad_mortality, 1, 0)
    biomass$fips <- substr(biomass$county, 1, 5)
    biomass$fips <- str_remove(biomass$fips, "^0+")
    
    # FILTER STEPS BASED ON INPUT ##############################################
    
    # Filter for method of carbon removal
    if (input$radio_method == 1) {
      biomass[, 2:11] <- biomass[, 2:11] + .4 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 2) {
      biomass[, 2:11] <- biomass[, 2:11] + .2 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 3) {
      biomass <- biomass
    }
    
    # Filter for mortality selection
    # if(input$radio_mort == 2) {
    #   biomass <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Filter for region
    grep_string <- paste0(input$radio_region, collapse = " | ")
    x <- biomass[grep(grep_string, biomass$county), ]
    
    # if (input$radio_region == 1) {
    #   x <- biomass
    # } else if (input$radio_region == 2) {
    #   x <- biomass[grep(" WA | OR ", biomass$county), ]
    # } else if (input$radio_region == 3) {
    #   x <- biomass[grep(" WA ", biomass$county), ]
    # } else if (input$radio_region == 4) {
    #   x <- biomass[grep(" OR ", biomass$county), ]
    # } else if (input$radio_region == 5) {
    #   x <- biomass[grep(" CA ", biomass$county), ]
    # } else if (input$radio_region == 6) {
    #   x <- biomass[grep(" ID ", biomass$county), ]
    # } else if (input$radio_region == 7) {
    #   x <- biomass[grep(" NV ", biomass$county), ]
    # } else if (input$radio_region == 8) {
    #   x <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Second, filter for distance from road
    if (input$radio_road == 1) {
      x <- x[, 1:2]
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 2) {
      x$Total <- x$`100 ft or less`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 3) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 4) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 5) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 6) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 7) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile` + x$`1/2 to 1 mile`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile` + acre$`1/2 to 1 mile`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 8) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`+ x$`1/2 to 1 mile` + x$`1 to 3 miles`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`+ acre$`1/2 to 1 mile` + acre$`1 to 3 miles`
      # acre <- acre[, 1:2]
    } else if (input$radio_road == 9) {
      x$Total <- x$`100 ft or less` + x$`101-300 ft` + x$`301-500 ft` + x$`501-1000 ft` + x$`1001 ft to 1/2 mile`+ x$`1/2 to 1 mile` + x$`1 to 3 miles` + x$`3 to 5 miles`
      x <- x[, 1:2]
      
      # acre$Total <- acre$`100 ft or less` + acre$`101-300 ft` + acre$`301-500 ft` + acre$`501-1000 ft` + acre$`1001 ft to 1/2 mile`+ acre$`1/2 to 1 mile` + acre$`1 to 3 miles` + acre$`3 to 5 miles`
      # acre <- acre[, 1:2]
    }
    
    # # Third, filter for Mortality Exceeding Growth
    # if (input$radio_mort == 1) {
    #   x <- x
    # } else if (input$radio_mort == 2) {
    #   x <- x[x$county %in% bad_mortality, ]
    #   acre <- acre[acre$county %in% bad_mortality]
    # }
    
    # Next, filter for fire percentile type
    if (input$radio_fire == 1) {
      fire <- fire[, c("fips", "Mean BP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 2) {
      fire <- fire[, c("fips", "Mean BP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 3) {
      fire <- fire[, c("fips", "Mean WHP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 4) {
      fire <- fire[, c("fips", "Mean WHP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } 
    
    # Fourth, filter for fire percentile
    x$fips <- substr(x$county, 1, 5)
    x$fips <- str_remove(x$fips, "^0+")
    x <- merge(x, fire, by = "fips")
    x$Total[x$measurement <= input$slider_fire / 100] <- 0
    x$Total <- x$Total / 1000000
    
    # # Fifth, change for per acre or total
    # if (input$radio_acre == 1) {
    #   x <- x
    #   x$Total <- x$Total / 1000000
    # } else if (input$radio_acre == 2) {
    #   x <- merge(x, acre, by = "county")
    #   x$Total <- x$Total / x$total_acres
    # }
    
    # Filter for removal of reserved wood
    removal <- read_excel("FULL_Biomass_ReservedStatus.xlsx")
    removal <- removal[2:nrow(removal), ]
    names(removal) <- c("county", "total", "not_reserved", "reserved")
    removal[removal == "-"] <- "0"
    removal[, 2:ncol(removal)] <- sapply(removal[, 2:ncol(removal)], as.numeric)
    removal$rem_ratio <- removal$not_reserved / removal$total
    removal <- removal[removal$county %in% x$county, ]
    if (input$radio_reserved == 2) {
      x$Total[x$county %in% removal$county] <- x$Total[x$county %in% removal$county] * 
        removal$rem_ratio
    }
    
    # Next, add in dead wood if appropriate
    dead <- dead[dead$county %in% x$county, ]
    dead$dead_wood <- dead$dead_wood / 1000000
    if (input$radio_dead == 1) {
      x$Total[x$county %in% dead$county] <- x$Total[x$county %in% dead$county] + 
        dead$dead_wood  
    } else if (input$radio_dead == 2) {
      x$Total <- x$Total
    }
    
    # Sixth, filter for unit
    if (input$radio_units == 1) {
      x <- x
    } else if (input$radio_units == 2) {
      x$Total <- x$Total * 1.835
    }
    
    # First part of stacked barplot - show total potential in each state
    # biomass <- biomass[grep(grep_string, biomass$county), ]
    # biomass$Total <- biomass$Total / 1000000
    
    if (input$radio_pool == 1) {
      first_stack <- data.frame("State" = input$radio_region,
                                "Total" = 0)
      for (state_abb in input$radio_region) {
        temp <- x[grep(paste0(" ", state_abb, " "), x$county), ]
        first_stack$Total[first_stack$State == state_abb] <- sum(temp$Total)
      }
      
      # Second of stacked barplot - show potential in selected counties
      # biomass <- biomass[biomass$county %in% x$county, ]
      # biomass$Total[x$Total == 0] <- 0
      
      second_stack <- data.frame("State" = input$radio_region,
                                 "Total"= 0)
      for (state_abb in input$radio_region) {
        temp <- x[grep(paste0(" ", state_abb, " "), x$county), ]
        second_stack$Total[second_stack$State == state_abb] <- sum(temp$Total)
      }
      
      # Now for the new logic - start drawing from each category of size
      size <- size[size$county %in% x$county, ]
      size[, 2:4] <- size[, 2:4] / 1000000
      
      x <- x[order(x$county), ] 
      size <- size[order(size$county), ]
      
      stack_data <- data.frame("county" = x$county, 
                               "small" = 0, "medium" = 0, "large" = 0)
      
      
      for (i in 1:nrow(x)) {
        temp <- x$Total[i]
        
        if (temp == 0) {
          stack_data$small[i] <- 0
          stack_data$medium[i] <- 0
          stack_data$large[i] <- 0
          next
        } else if (temp < size$small[i]) {
          stack_data$small[i] <- temp
          stack_data$medium[i] <- 0
          stack_data$large[i] <- 0
          next
        } else if (temp > size$small[i]) {
          stack_data$small[i] <- size$small[i]
          stack_data$medium[i] <- 0
          stack_data$large[i] <- 0
        }
        
        temp <- temp - size$small[i]
        
        if (temp < size$medium[i]) {
          stack_data$medium[i] <- temp
          next
        } else if (temp > size$medium[i]) {
          stack_data$medium[i] <- size$medium[i]
        }
        
        temp <- temp - size$medium[i]
        stack_data$large[i] <- temp
      }
      
      third_stack <- data.frame("State" = input$radio_region,
                                "small" = 0, "medium" = 0, "large" = 0,
                                "dead" = 0)
      for (i in 1:length(input$radio_region)) {
        state_abb <- input$radio_region[i]
        temp <- stack_data[grep(paste0(" ", state_abb, " "), stack_data$county), ]
        temp_dead <- dead[grep(paste0(" ", state_abb, " "), dead$county), ]
        third_stack$small[third_stack$State == state_abb] <- sum(temp$small)
        third_stack$medium[third_stack$State == state_abb] <- sum(temp$medium)
        third_stack$large[third_stack$State == state_abb] <- sum(temp$large)
        third_stack$dead[third_stack$State == state_abb] <- sum(temp_dead$dead_wood)
      }
      
      pie_stack <- data.frame("Position" = rep(third_stack$State, 4),
                              "Labels" = rep(c("Small Wood", "Medium Wood", "Large Wood", "Dead Wood"), each = nrow(third_stack)),
                              "Values" = c(third_stack$small, third_stack$medium, third_stack$large, third_stack$dead))
    } else if (input$radio_pool %in% c(2, 3, 4)) {
      first_stack <- data.frame("State" = input$radio_region,
                                "Total" = 0)
      
      for (state_abb in input$radio_region) {
        temp <- x[grep(paste0(" ", state_abb, " "), x$county), ]
        first_stack$Total[first_stack$State == state_abb] <- sum(temp$Total)
      }
      
      # Second of stacked barplot - show potential in selected counties
      # biomass <- biomass[biomass$county %in% x$county, ]
      # biomass$Total[x$Total == 0] <- 0
      
      second_stack <- data.frame("State" = input$radio_region,
                                 "Total"= 0)
      for (state_abb in input$radio_region) {
        temp <- x[grep(paste0(" ", state_abb, " "), x$county), ]
        second_stack$Total[second_stack$State == state_abb] <- sum(temp$Total)
      }
      
      # Now for the new logic - start drawing from each category of size
      size <- size[size$county %in% x$county, ]
      size[, 2] <- size[, 2] / 1000000
      
      x <- x[order(x$county), ] 
      size <- size[order(size$county), ]
      
      third_stack <- data.frame("State" = input$radio_region,
                                "small" = 0)
      for (i in 1:length(input$radio_region)) {
        state_abb <- input$radio_region[i]
        temp <- size[grep(paste0(" ", state_abb, " "), size$county), ]
        dead_temp <- dead[grep(paste0(" ", state_abb, " "), dead$county), ]
        third_stack$small[third_stack$State == state_abb] <- sum(temp$small) * 10^6
        third_stack$dead[third_stack$State == state_abb] <- sum(dead_temp$dead_wood)
      }
      
      pie_stack <- data.frame("Position" = rep(third_stack$State, 2),
                              "Labels" = rep(c("Small Wood", "Dead Wood"), each = nrow(third_stack)),
                              "Values" = c(third_stack$small, third_stack$dead))
    } else if (input$radio_pool == 5) {
      # Now for the new logic - start drawing from each category of size
      size <- size[size$county %in% x$county, ]
      size[, 2:ncol(size)] <- sapply(size[, 2:ncol(size)], as.numeric)
      size[, 2:ncol(size)] <- size[, 2:ncol(size)] / 1000000
      
      third_stack <- data.frame("State" = input$radio_region,
                                "twelves" = 0, "Tops" = 0)
      for (i in 1:length(input$radio_region)) {
        state_abb <- input$radio_region[i]
        temp <- size[grep(paste0(" ", state_abb, " "), size$county), ]
        dead_temp <- dead[grep(paste0(" ", state_abb, " "), dead$county), ]
        temp$tops <- as.numeric(temp$tops)
        third_stack$twelves[third_stack$State == state_abb] <- sum(temp$twelves)
        third_stack$Tops[third_stack$State == state_abb] <- sum(temp$tops)
        third_stack$dead[third_stack$State == state_abb] <- sum(dead_temp$dead_wood)
      }
      
      
      
      pie_stack <- data.frame("Position" = rep(third_stack$State, 3),
                              "Labels" = rep(c("Wood <12 Inches", "Tops", "Dead Wood"), each = nrow(third_stack)),
                              "Values" = c(third_stack$twelves, third_stack$Tops, third_stack$dead))
    }
    
    if (input$radio_dead == 2) {
      pie_stack <- pie_stack[pie_stack$Labels != "Dead Wood", ]
    }
    
    p1 <- ggplot(data = pie_stack, aes(x = Position, y = Values, fill = Labels)) + 
      geom_col() + scale_x_discrete(limits = c(" ", input$radio_region)) + 
      coord_polar("y", start = 0) + 
      labs(title = "Total Contributions by Wood Type", y =ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                                                 "Millions MTCO2e"),
           x = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x=element_blank(), legend.text=element_text(size=8),
            legend.key.size = unit(.2, 'cm'))
    
    own <- read_excel("FULL_Biomass_Ownership.xlsx")
    own[own == "-"] <- "0"
    own[, 2:ncol(own)] <- sapply(own[, 2:ncol(own)], as.numeric)
    names(own)[1] <- "county" 
    own <- own[own$county %in% x$county, ]
    
    colnames(own)[3:ncol(own)]
    
    fourth_stack <- data.frame(matrix(NA, nrow = length(input$radio_region),
                                      ncol = 13))
    colnames(fourth_stack) <- c("state", colnames(own)[3:ncol(own)])
    fourth_stack$state <- input$radio_region
    
    for (i in 1:nrow(fourth_stack)) {
      temp <- own[grep(paste0(" ", fourth_stack$state[i], " "), own$county), ]
      fourth_stack[i, 2:12] <- colSums(temp[, 3:13])
    }
    pie_stack_2 <- data.frame("Position" = rep(third_stack$State, 11,),
                              "Labels" = rep(colnames(fourth_stack[2:12]), each = length(input$radio_region)),
                              "Values" = unlist(fourth_stack[, 2:12]))
    
    # Standardize by equal thinning assumption
    for (i in 1:length(input$radio_region)) {
      og_total <- sum(pie_stack$Values[pie_stack$Position == input$radio_region[i]])
      var_total <- sum(pie_stack_2$Values[pie_stack_2$Position == input$radio_region[i]]) / 1000000
      pie_stack_2$Values[pie_stack_2$Position == input$radio_region[i]] <- 
        pie_stack_2$Values[pie_stack_2$Position == input$radio_region[i]] / (var_total * 1000000) * og_total
    }
    
    p2 <- ggplot(data = pie_stack_2, aes(x = Position, y = Values, fill = Labels)) + 
      geom_col() + scale_x_discrete(limits = c(" ", input$radio_region)) + 
      coord_polar("y", start = 0) + 
      labs(title = "Ownership Groups", y =ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                                 "Millions MTCO2e"),
           subtitle = paste0("United States Forest Service: ", 
                             round(sum(pie_stack_2$Values[pie_stack_2$Labels %in% c("National Park Service", 
                                                                                    "National Forest System",
                                                                                    "National Forest Grassland",
                                                                                    "Other Forest Service")]) / 
                                     sum(pie_stack_2$Values) *100, 2), "%"),
           x = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x=element_blank(), legend.text=element_text(size=8),
            legend.key.size = unit(.2, 'cm'))
    
    #########################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% x$county, ]
    
    colnames(slp)[3:ncol(slp)]
    
    fifth_stack <- data.frame(matrix(NA, nrow = length(input$radio_region),
                                     ncol = 7))
    colnames(fifth_stack) <- c("state", colnames(slp)[3:ncol(slp)])
    fifth_stack$state <- input$radio_region
    
    for (i in 1:nrow(fifth_stack)) {
      temp <- slp[grep(paste0(" ", fifth_stack$state[i], " "), slp$county), ]
      fifth_stack[i, 2:7] <- colSums(temp[, 3:8])
    }
    pie_stack_3 <- data.frame("Position" = rep(third_stack$State, 6,),
                              "Labels" = rep(colnames(fifth_stack[2:7]), each = length(input$radio_region)),
                              "Values" = unlist(fifth_stack[, 2:7]))
    
    # Standardize by equal thinning assumption
    for (i in 1:length(input$radio_region)) {
      og_total <- sum(pie_stack$Values[pie_stack$Position == input$radio_region[i]])
      var_total <- sum(pie_stack_3$Values[pie_stack_3$Position == input$radio_region[i]]) / 1000000
      pie_stack_3$Values[pie_stack_3$Position == input$radio_region[i]] <- 
        pie_stack_3$Values[pie_stack_3$Position == input$radio_region[i]] / (var_total * 1000000) * og_total
    }
    
    pie_stack_3$Labels <- factor(pie_stack_3$Labels, levels = unique(pie_stack_3$Labels))
    
    p3 <- ggplot(data = pie_stack_3, aes(x = Position, y = Values, fill = Labels)) + 
      geom_col() + scale_x_discrete(limits = c(" ", input$radio_region)) + 
      coord_polar("y", start = 0) + 
      labs(title = "Slope of Usable Land", y =ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                                     "Millions MTCO2e"),
           x = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x=element_blank(), legend.text=element_text(size=8),
            legend.key.size = unit(.2, 'cm'))
    ########################
    
    #########################
    elv <- read_excel("FULL_Biomass_Elevation.xlsx")
    elv <- elv[2:nrow(elv), ]
    elv[elv == "-"] <- "0"
    elv[, 2:ncol(elv)] <- sapply(elv[, 2:ncol(elv)], as.numeric)
    names(elv)[1] <- "county" 
    elv <- elv[elv$county %in% x$county, ]
    
    colnames(elv)[3:ncol(elv)]
    
    sixth_stack <- data.frame(matrix(NA, nrow = length(input$radio_region),
                                     ncol = 12))
    colnames(sixth_stack) <- c("state", colnames(elv)[3:ncol(elv)])
    sixth_stack$state <- input$radio_region
    
    for (i in 1:nrow(sixth_stack)) {
      temp <- elv[grep(paste0(" ", sixth_stack$state[i], " "), elv$county), ]
      sixth_stack[i, 2:12] <- colSums(temp[, 3:13])
    }
    pie_stack_4 <- data.frame("Position" = rep(third_stack$State, 11,),
                              "Labels" = rep(colnames(sixth_stack[2:12]), each = length(input$radio_region)),
                              "Values" = unlist(sixth_stack[, 2:12]))
    
    # Standardize by equal thinning assumption
    for (i in 1:length(input$radio_region)) {
      og_total <- sum(pie_stack$Values[pie_stack$Position == input$radio_region[i]])
      var_total <- sum(pie_stack_4$Values[pie_stack_3$Position == input$radio_region[i]]) / 1000000
      pie_stack_4$Values[pie_stack_4$Position == input$radio_region[i]] <- 
        pie_stack_4$Values[pie_stack_4$Position == input$radio_region[i]] / (var_total * 1000000) * og_total
    }
    
    pie_stack_4$Labels <- factor(pie_stack_4$Labels, levels = unique(pie_stack_4$Labels))
    
    p4 <- ggplot(data = pie_stack_4, aes(x = Position, y = Values, fill = Labels)) + 
      geom_col() + scale_x_discrete(limits = c(" ", input$radio_region)) + 
      coord_polar("y", start = 0) + 
      labs(title = "Elevation of Usable Land", y =ifelse(input$radio_units == 1,  "Millions Short Dry Tons of Biomass",
                                                         "Millions MTCO2e"),
           x = "State") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            axis.text.x=element_blank(), legend.text=element_text(size=8),
            legend.key.size = unit(.2, 'cm'))
    ########################
    
    grid.arrange(p1, p2, p3, p4, ncol=2)
  })
  
  # Table showing results of calculation
  output$SummaryTable <- function() {
    # Initialize empty data
    x <- NA
    
    ## USER NOTE: All vestigial code is commented out, in case we want to 
    ## implement it again in a later version of this build
    
    # Read in and clean all necessary documents, removing the white space and 
    # giving the columns appropriate names
    mort <- read_excel("WAORCA_Mortality_Overstock_Road.xlsx")
    names(mort) <- mort[1, ]
    names(mort)[1] <- "county"
    mort <- mort[3:nrow(mort), -12]
    mort[mort == "-"] <- "0"
    mort[, 2:11] <- sapply(mort[, 2:11], as.numeric)
    
    growth <- read_excel("WAORCA_Growth_Overstock_Road.xlsx")
    names(growth) <- growth[1, ]
    names(growth)[1] <- "county"
    growth <- growth[3:nrow(growth), -12]
    growth[growth == "-"] <- "0"
    growth[, 2:11] <- sapply(growth[, 2:11], as.numeric)
    
    if (input$radio_pool == 1) {
      biomass <- read_excel("FULL_Biomass_Overstock_Road.xlsx")
    } else if (input$radio_pool == 2) {
      biomass <- read_excel("FULL_Biomass_Less5.xlsx")
    } else if (input$radio_pool == 3) {
      biomass <- read_excel("FULL_Biomass_Less9.xlsx")
    } else if (input$radio_pool == 4) {
      biomass <- read_excel("FULL_Biomass_Less12.xlsx")
    } else if (input$radio_pool == 5) {
      biomass <- read_excel("FULL_Biomass_Tops.xlsx")
    }
    
    colnames(biomass)[1] <- "county"
    biomass <- biomass[2:nrow(biomass),]
    biomass[biomass == "-"] <- "0"
    biomass[is.na(biomass)] <- 0
    biomass[, 2:11] <- sapply(biomass[, 2:11], as.numeric)
    
    # Create removals for slope and NPS ########################################
    slp <- read_excel("FULL_Biomass_Slope.xlsx")
    slp <- slp[2:nrow(slp), ]
    slp[slp == "-"] <- "0"
    slp[, 2:ncol(slp)] <- sapply(slp[, 2:ncol(slp)], as.numeric)
    names(slp)[1] <- "county" 
    slp <- slp[slp$county %in% biomass$county, ]
    
    slp$ex_ratio <-  (slp$`61-80 percent` + slp$`41-60 percent` + 
                        slp$`81-100 percent` + slp$`100+ percent`) / slp$Total
    
    nps <- read_excel("FULL_Biomass_Ownership.xlsx")
    nps <- nps[2:nrow(nps), ]
    nps[nps == "-"] <- "0"
    nps[, 2:ncol(nps)] <- sapply(nps[, 2:ncol(nps)], as.numeric)
    names(nps)[1] <- "county" 
    nps <- nps[nps$county %in% biomass$county, ]
    nps$protected <- nps$`National Park Service` / nps$Total
    
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - nps$protected)
    biomass[, 2:ncol(biomass)] <- biomass[, 2:ncol(biomass)] * (1 - slp$ex_ratio)
    ############################################################################
    
    reg <- read_excel("FULL_Biomass_Regular_Road.xlsx")
    colnames(reg)[1] <- "county"
    reg <- reg[2:nrow(reg),]
    reg[reg == "-"] <- "0"
    reg[, 2:11] <- sapply(reg[, 2:11], as.numeric)
    
    acre <- read_excel("FULL_Overstock_Acre.xlsx")
    colnames(acre) <- acre[1, ]
    colnames(acre)[1] <- "county"
    acre <- acre[3:nrow(acre),]
    acre[acre == "-"] <- "0"
    acre[, 2:11] <- sapply(acre[, 2:11], as.numeric)
    names(acre)[2] <- "total_acres"
    
    bio_tot <- read_excel("FULL_Biomass_Total_Road.xlsx")
    colnames(bio_tot)[1] <- "county"
    bio_tot <- bio_tot[2:nrow(bio_tot),]
    bio_tot[bio_tot == "-"] <- "0"
    bio_tot[, 2:11] <- sapply(bio_tot[, 2:11], as.numeric)
    
    dead <- read_excel("FULL_Biomass_Dead_Decay.xlsx")
    dead <- dead[2:nrow(dead), ]
    names(dead) <- c("county", "dead_wood", "class5", "class4", "class3", 
                     "class2", "class1")
    dead[dead == "-"] <- "0"
    dead[, 2:7] <- sapply(dead[, 2:7], as.numeric)
    dead$dead_wood <-  dead$class2 + dead$class1
    dead <- dead[, 1:2]
    
    fire <- read_excel("Wildfire_Data.xlsx")
    fire <- fire[fire$STATE %in% c("Arizona", "California", "Colorado", "Idaho",
                                   "Montana", "Nevada", "New Mexico", "Oregon",
                                   "Utah", "Washington", "Wyoming"), ]
    names(fire)[names(fire) == "GEOID"] <- "fips"
    
    # Figure out where mortality > growth
    bad_mortality <- mort$county[mort$Total - growth$Total > 0]
    
    biomass$flag <- ifelse(biomass$county %in% bad_mortality, 1, 0)
    biomass$fips <- substr(biomass$county, 1, 5)
    biomass$fips <- str_remove(biomass$fips, "^0+")
    
    # FILTER STEPS BASED ON INPUT ##############################################
    
    # Filter for method of carbon removal
    if (input$radio_method == 1) {
      biomass[, 2:11] <- biomass[, 2:11] + .4 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 2) {
      biomass[, 2:11] <- biomass[, 2:11] + .2 * reg[reg$county %in% biomass$county, 2:11]
    } else if (input$radio_method == 3) {
      biomass <- biomass
    }
    
    # Filter for mortality selection
    # if(input$radio_mort == 2) {
    #   biomass <- biomass[biomass$county %in% bad_mortality, ]
    # }
    
    # Filter for region
    grep_string <- paste0(input$radio_region, collapse = " | ")
    x <- biomass[grep(grep_string, biomass$county), ]
    
    # Next, filter for fire percentile type
    if (input$radio_fire == 1) {
      fire <- fire[, c("fips", "Mean BP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 2) {
      fire <- fire[, c("fips", "Mean BP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 3) {
      fire <- fire[, c("fips", "Mean WHP percentile within state", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } else if (input$radio_fire == 4) {
      fire <- fire[, c("fips", "Mean WHP percentile within nation", "STATE")]
      names(fire) <- c("fips", "measurement", "state")
    } 
    
    # Fourth, filter for fire percentile
    x$fips <- substr(x$county, 1, 5)
    x$fips <- str_remove(x$fips, "^0+")
    x <- merge(x, fire, by = "fips")
    x <- x[x$measurement >= input$slider_fire / 100, ]
    x[, 3:12] <- x[, 3:12] / 1000000
    
    # Filter for removal of reserved wood
    removal <- read_excel("FULL_Biomass_ReservedStatus.xlsx")
    removal <- removal[2:nrow(removal), ]
    names(removal) <- c("county", "total", "not_reserved", "reserved")
    removal[removal == "-"] <- "0"
    removal[, 2:ncol(removal)] <- sapply(removal[, 2:ncol(removal)], as.numeric)
    removal$rem_ratio <- removal$not_reserved / removal$total
    removal <- removal[removal$county %in% x$county, ]
    if (input$radio_reserved == 2) {
      x$Total[x$county %in% removal$county] <- x$Total[x$county %in% removal$county] * 
        removal$rem_ratio
    }
    
    # Next, add in dead wood if appropriate
    dead <- dead[dead$county %in% x$county, ]
    dead$dead_wood <- dead$dead_wood / 1000000
    if (input$radio_dead == 1) {
      x$Total[x$county %in% dead$county] <- x$Total[x$county %in% dead$county] + 
        dead$dead_wood  
    } else if (input$radio_dead == 2) {
      x$Total <- x$Total
    }
    
    # Sixth, filter for unit
    if (input$radio_units == 1) {
      x <- x
    } else if (input$radio_units == 2) {
      x[, 3:12] <- x[, 3:12] * 1.835
    }
    
    # Fifth, change for per acre or total
    if (input$radio_acre == 1) {
      x <- x
    } else if (input$radio_acre == 2) {
      x <- merge(x, acre, by = "county")
      x$total_acres[x$total_acres == 0] <- mean(x$total_acres)
      x$Total <- x$Total / x$total_acres * 1000000
    }
    
    x$measurement <- paste0(round(x$measurement, 3) * 100, "%")
    
    x <- x[, c("county", "Total", "measurement")]
    
    x <- x[x$Total != 0, ]
    
    if (length(grep(" MT ", x$county)) != 0) {
      x <- rbind(x[1:grep(" MT ", x$county)[length(grep(" MT ", x$county))], ],
                 c("Montana Total", (sum(x$Total[grep(" MT ", x$county)[1]:grep(" MT ", x$county)[length(grep(" MT ", x$county))]])), " "),
                 x[(grep(" MT ", x$county)[length(grep(" MT ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" ID ", x$county)) != 0) {
      x <- rbind(x[1:grep(" ID ", x$county)[length(grep(" ID ", x$county))], ],
                 c("Idaho Total", (sum(x$Total[grep(" ID ", x$county)[1]:grep(" ID ", x$county)[length(grep(" ID ", x$county))]])), " "),
                 x[(grep(" ID ", x$county)[length(grep(" ID ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" NV ", x$county)) != 0) {
      x <- rbind(x[1:grep(" NV ", x$county)[length(grep(" NV ", x$county))], ],
                 c("Nevada Total", (sum(x$Total[grep(" NV ", x$county)[1]:grep(" NV ", x$county)[length(grep(" NV ", x$county))]])), " "),
                 x[(grep(" NV ", x$county)[length(grep(" NV ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" OR ", x$county)) != 0) {
      x <- rbind(x[1:grep(" OR ", x$county)[length(grep(" OR ", x$county))], ],
                 c("Oregon Total", (sum(x$Total[grep(" OR ", x$county)[1]:grep(" OR ", x$county)[length(grep(" OR ", x$county))]])), " "),
                 x[(grep(" OR ", x$county)[length(grep(" OR ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" WA ", x$county)) != 0) {
      x <- rbind(x[1:grep(" WA ", x$county)[length(grep(" WA ", x$county))], ],
                 c("Washington Total", (sum(x$Total[grep(" WA ", x$county)[1]:grep(" WA ", x$county)[length(grep(" WA ", x$county))]])), " "),
                 x[(grep(" WA ", x$county)[length(grep(" WA ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" CA ", x$county)) != 0) {
      x <- rbind(x[1:grep(" CA ", x$county)[length(grep(" CA ", x$county))], ],
                 c("California Total", (sum(x$Total[grep(" CA ", x$county)[1]:grep(" CA ", x$county)[length(grep(" CA ", x$county))]])), " "),
                 x[(grep(" CA ", x$county)[length(grep(" CA ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" CO ", x$county)) != 0) {
      x <- rbind(x[1:grep(" CO ", x$county)[length(grep(" CO ", x$county))], ],
                 c("Colorado Total", (sum(x$Total[grep(" CO ", x$county)[1]:grep(" CO ", x$county)[length(grep(" CO ", x$county))]])), " "),
                 x[(grep(" CO ", x$county)[length(grep(" CO ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" AZ ", x$county)) != 0) {
      x <- rbind(x[1:grep(" AZ ", x$county)[length(grep(" AZ ", x$county))], ],
                 c("Arizona Total", (sum(x$Total[grep(" AZ ", x$county)[1]:grep(" AZ ", x$county)[length(grep(" AZ ", x$county))]])), " "),
                 x[(grep(" AZ ", x$county)[length(grep(" AZ ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" NM ", x$county)) != 0) {
      x <- rbind(x[1:grep(" NM ", x$county)[length(grep(" NM ", x$county))], ],
                 c("New Mexico Total", (sum(x$Total[grep(" NM ", x$county)[1]:grep(" NM ", x$county)[length(grep(" NM ", x$county))]])), " "),
                 x[(grep(" NM ", x$county)[length(grep(" NM ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" UT ", x$county)) != 0) {
      x <- rbind(x[1:grep(" UT ", x$county)[length(grep(" UT ", x$county))], ],
                 c("Utah Total", (sum(x$Total[grep(" UT ", x$county)[1]:grep(" UT ", x$county)[length(grep(" UT ", x$county))]])), " "),
                 x[(grep(" UT ", x$county)[length(grep(" UT ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    if (length(grep(" WY ", x$county)) != 0) {
      x <- rbind(x[1:grep(" WY ", x$county)[length(grep(" WY ", x$county))], ],
                 c("Wyoming Total", (sum(x$Total[grep(" WY ", x$county)[1]:grep(" WY ", x$county)[length(grep(" WY ", x$county))]])), " "),
                 x[(grep(" WY ", x$county)[length(grep(" WY ", x$county))]+1):nrow(x), ])
      x$Total <- as.numeric(x$Total)
    }
    
    
    ######################################################
    ## FIX FOR NEW STATES ######################################################
    ######################################################
    
    if (length(input$radio_region) > 0) x <- x[-c(nrow(x):(nrow(x)-1)), ]
    
    x <- rbind(x, c("Overall Total", sum(x$Total)/2, " "))
    x$Total <- round(as.numeric(x$Total), 1)
    
    x <- x[!is.na(x$county), ]
    
    colnames(x)[1] <- "Region"
    colnames(x)[2] <- "Total Biomass"
    fire_choices <- c("Fraction of Total HUs Directly Exposed",
                      "Burn Probability Percentile (state)",
                      "Burn Probability (nation)",
                      "Risk to Potential Structures (state)",
                      "Risk to Potential (nation)",
                      "Wildfire Hazard Potential (state)",
                      "Wildfire Hazard (nation)",
                      "Expected Damages Percentile (state)",
                      "Expected Damages (nation)")
    colnames(x)[3] <- "Fire Percentile"
    
    rownames(x) <- c()
    
    table <- x %>% knitr::kable("html") %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") 
    
    if (length(grep(" ID ", x$Region)) != 0) table <- table %>% pack_rows("Idaho", grep(" ID ", x$Region)[1], grep(" ID ", x$Region)[length(grep(" ID ", x$Region))])
    if (length(grep(" NV ", x$Region)) != 0) table <- table %>% pack_rows("Nevada", grep(" NV ", x$Region)[1], grep(" NV ", x$Region)[length(grep(" NV ", x$Region))])
    if (length(grep(" CA ", x$Region)) != 0) table <- table %>% pack_rows("California", grep(" CA ", x$Region)[1], grep(" CA ", x$Region)[length(grep(" CA ", x$Region))])
    if (length(grep(" OR ", x$Region)) != 0) table <- table %>% pack_rows("Oregon", grep(" OR ", x$Region)[1], grep(" OR ", x$Region)[length(grep(" OR ", x$Region))])
    if (length(grep(" WA ", x$Region)) != 0) table <- table %>% pack_rows("Washington", grep(" WA ", x$Region)[1], grep(" WA ", x$Region)[length(grep(" WA ", x$Region))])
    if (length(grep(" AZ ", x$Region)) != 0) table <- table %>% pack_rows("Arizona", grep(" AZ ", x$Region)[1], grep(" AZ ", x$Region)[length(grep(" AZ ", x$Region))])
    if (length(grep(" CO ", x$Region)) != 0) table <- table %>% pack_rows("Colorado", grep(" CO ", x$Region)[1], grep(" CO ", x$Region)[length(grep(" CO ", x$Region))])
    if (length(grep(" MT ", x$Region)) != 0) table <- table %>% pack_rows("Montana", grep(" MT ", x$Region)[1], grep(" MT ", x$Region)[length(grep(" MT ", x$Region))])
    if (length(grep(" NM ", x$Region)) != 0) table <- table %>% pack_rows("New Mexico", grep(" NM ", x$Region)[1], grep(" NM ", x$Region)[length(grep(" NM ", x$Region))])
    if (length(grep(" UT ", x$Region)) != 0) table <- table %>% pack_rows("Utah", grep(" UT ", x$Region)[1], grep(" UT ", x$Region)[length(grep(" UT ", x$Region))])
    if (length(grep(" WY ", x$Region)) != 0) table <- table %>% pack_rows("Wyoming", grep(" WY ", x$Region)[1], grep(" WY ", x$Region)[length(grep(" WY ", x$Region))])
    
    table <- table %>% pack_rows("Overall", nrow(x), nrow(x))
    
    table
    
    # Produce summary table
    #   if (input$radio_region == 1) {
    #     x <- x[, c("county", "Total", "measurement")]
    #     
    #     x <- rbind(x[1:grep(" ID ", x$county)[length(grep(" ID ", x$county))], ],
    #                c("Idaho Total", (sum(x$Total[grep(" ID ", x$county)[1]:grep(" ID ", x$county)[length(grep(" ID ", x$county))]])), " "),
    #                x[(grep(" ID ", x$county)[length(grep(" ID ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x[1:grep(" NV ", x$county)[length(grep(" NV ", x$county))], ],
    #                c("Nevada Total", (sum(x$Total[grep(" NV ", x$county)[1]:grep(" NV ", x$county)[length(grep(" NV ", x$county))]])), " "),
    #                x[(grep(" NV ", x$county)[length(grep(" NV ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x[1:grep(" OR ", x$county)[length(grep(" OR ", x$county))], ],
    #                c("Oregon Total", (sum(x$Total[grep(" OR ", x$county)[1]:grep(" OR ", x$county)[length(grep(" OR ", x$county))]])), " "),
    #                x[(grep(" OR ", x$county)[length(grep(" OR ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     x <- rbind(x[1:grep(" WA ", x$county)[length(grep(" WA ", x$county))], ],
    #                c("Washington Total", (sum(x$Total[grep(" WA ", x$county)[1]:grep(" WA ", x$county)[length(grep(" WA ", x$county))]])), " "),
    #                x[(grep(" WA ", x$county)[length(grep(" WA ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x[1:(grep(" CA ", x$county)[length(grep(" CA ", x$county))]), ],
    #                c("California Total", (sum(x$Total[grep(" CA ", x$county)[1]:grep(" CA ", x$county)[length(grep(" CA ", x$county))]])), " "))
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x, c("Overall Total", sum(x$Total)/2, " "))
    #     x$Total <- round(as.numeric(x$Total), 1)
    #     
    #     x <- x[!is.na(x$county), ]
    #     
    #     colnames(x)[1] <- "Region"
    #     colnames(x)[2] <- "Total Carbon"
    #     fire_choices <- c("Fraction of Total HUs Directly Exposed",
    #                       "Burn Probability Percentile (state)",
    #                       "Burn Probability (nation)",
    #                       "Risk to Potential Structures (state)",
    #                       "Risk to Potential (nation)",
    #                       "Wildfire Hazard Potential (state)",
    #                       "Wildfire Hazard (nation)",
    #                       "Expected Damages Percentile (state)",
    #                       "Expected Damages (nation)")
    #     colnames(x)[3] <- "Fire Percentile"
    #     
    #     rownames(x) <- c()
    #     
    #     x %>% knitr::kable("html") %>%
    #       kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
    #       pack_rows("Idaho", grep(" ID ", x$Region)[1], grep(" ID ", x$Region)[length(grep(" ID ", x$Region))]) %>% 
    #       pack_rows("Nevada", grep(" NV ", x$Region)[1], grep(" NV ", x$Region)[length(grep(" NV ", x$Region))]) %>% 
    #       pack_rows("California", grep(" CA ", x$Region)[1], grep(" CA ", x$Region)[length(grep(" CA ", x$Region))]) %>% 
    #       pack_rows("Oregon", grep(" OR ", x$Region)[1], grep(" OR ", x$Region)[length(grep(" OR ", x$Region))]) %>%
    #       pack_rows("Washington", grep(" WA ", x$Region)[1], grep(" WA ", x$Region)[length(grep(" WA ", x$Region))])
    #     
    #   } else if (input$radio_region %in% c(3,4,5,6,7)) {
    #     x <- x[, c("county", "Total", "measurement")]
    #     
    #     state_names <- c("Total (5 states)", "PNW", "Washington",
    #                      "Oregon", "California", "Idaho", "Nevada")
    #     x <- rbind(x, c("Total", sum(x$Total), " "))
    #     x$Total <- round(as.numeric(x$Total), 1)
    #     
    #     colnames(x)[1] <- "Region"
    #     colnames(x)[2] <- "Total Carbon"
    #     colnames(x)[3] <- "Fire Percentile"
    #     
    #     rownames(x) <- c()
    #     x %>% knitr::kable("html") %>%
    #       kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")
    #     
    #   } else if (input$radio_region == 2) {
    #     x <- x[, c("county", "Total", "measurement")]
    #     
    #     x <- rbind(x[1:grep(" OR ", x$county)[length(grep(" OR ", x$county))], ],
    #                c("Oregon Total", (sum(x$Total[grep(" OR ", x$county)[1]:grep(" OR ", x$county)[length(grep(" OR ", x$county))]])), " "),
    #                x[(grep(" OR ", x$county)[length(grep(" OR ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     x <- rbind(x[1:grep(" WA ", x$county)[length(grep(" WA ", x$county))], ],
    #                c("Washington Total", (sum(x$Total[grep(" WA ", x$county)[1]:grep(" WA ", x$county)[length(grep(" WA ", x$county))]])), " "))
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x, c("Overall Total", sum(x$Total)/2, " "))
    #     x$Total <- round(as.numeric(x$Total), 1)
    #     
    #     colnames(x)[1] <- "Region"
    #     colnames(x)[2] <- "Total Carbon"
    #     fire_choices <- c("Fraction of Total HUs Directly Exposed",
    #                       "Burn Probability Percentile (state)",
    #                       "Burn Probability (nation)",
    #                       "Risk to Potential Structures (state)",
    #                       "Risk to Potential (nation)",
    #                       "Wildfire Hazard Potential (state)",
    #                       "Wildfire Hazard (nation)",
    #                       "Expected Damages Percentile (state)",
    #                       "Expected Damages (nation)")
    #     colnames(x)[3] <- "Fire Percentile"
    #     
    #     rownames(x) <- c()
    #     
    #     x %>% knitr::kable("html") %>%
    #       kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
    #       pack_rows("Oregon", grep(" OR ", x$Region)[1], grep(" OR ", x$Region)[length(grep(" OR ", x$Region))]) %>%
    #       pack_rows("Washington", grep(" WA ", x$Region)[1], grep(" WA ", x$Region)[length(grep(" WA ", x$Region))]) 
    #   } else if (input$radio_region == 8) {
    #     x <- x[, c("county", "Total", "measurement")]
    #     
    #     x <- rbind(x[1:grep(" OR ", x$county)[length(grep(" OR ", x$county))], ],
    #                c("Oregon Total", (sum(x$Total[grep(" OR ", x$county)[1]:grep(" OR ", x$county)[length(grep(" OR ", x$county))]])), " "),
    #                x[(grep(" OR ", x$county)[length(grep(" OR ", x$county))]+1):nrow(x), ])
    #     x$Total <- as.numeric(x$Total)
    #     x <- rbind(x[1:grep(" WA ", x$county)[length(grep(" WA ", x$county))], ],
    #                c("Washington Total", (sum(x$Total[grep(" WA ", x$county)[1]:grep(" WA ", x$county)[length(grep(" WA ", x$county))]])), " "))
    #     x$Total <- as.numeric(x$Total)
    #     
    #     x <- rbind(x, c("Overall Total", sum(x$Total)/2, " "))
    #     x$Total <- round(as.numeric(x$Total), 1)
    #     
    #     colnames(x)[1] <- "Region"
    #     colnames(x)[2] <- "Total Carbon"
    #     fire_choices <- c("Fraction of Total HUs Directly Exposed",
    #                       "Burn Probability Percentile (state)",
    #                       "Burn Probability (nation)",
    #                       "Risk to Potential Structures (state)",
    #                       "Risk to Potential (nation)",
    #                       "Wildfire Hazard Potential (state)",
    #                       "Wildfire Hazard (nation)",
    #                       "Expected Damages Percentile (state)",
    #                       "Expected Damages (nation)")
    #     colnames(x)[3] <- "Fire Percentile"
    #     
    #     rownames(x) <- c()
    #     
    #     x %>% knitr::kable("html") %>%
    #       kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
    #       pack_rows("Oregon", grep(" OR ", x$Region)[1], grep(" OR ", x$Region)[length(grep(" OR ", x$Region))]) %>%
    #       pack_rows("Washington", grep(" WA ", x$Region)[1], grep(" WA ", x$Region)[length(grep(" WA ", x$Region))])
    #   }
    #   
  }
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "CCLab.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(radio_region = input$radio_region,
                     radio_method = input$radio_method,
                     radio_fire = input$radio_fire,
                     slider_fire = input$slider_fire,
                     slider_dead = input$slider_dead,
                     radio_road = input$radio_road,
                     radio_units = input$radio_units,
                     radio_dead = input$radio_dead,
                     radio_pool = input$radio_pool,
                     table = table,
                     x_first_plot = x_first_plot,
                     gg_frame_use = gg_frame_use,
                     gg_third_plot = gg_third_plot,
                     pie_stack = pie_stack)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
