library(shiny)
library(rsconnect)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(Rcpp)
library(shinyjs)
library(plotly)

setwd('C:\\Users\\ahearn\\Desktop\\gt\\ppol563\\migration')

ui <- fluidPage(mainPanel(
    navbarPage(
        "Kickstarter",
        navbarMenu(
            "Maps",
            tabPanel("US Map", plotlyOutput(
                "plotMap", height = 900, width = 1200
            )),
            tabPanel("County Map",
                     plotlyOutput("Smap"),
                     plotlyOutput("Cmap"))
        ),
        tabPanel(
            "Interaction",
            plotlyOutput("plotInt", height = 900, width = 1200)
        ),
        navbarMenu(
            "Barplots",
            tabPanel("Citys", plotlyOutput(
                "plotBar1", height = 900, width = 1200
            )),
            tabPanel(
                "Catigories",
                plotlyOutput("plotBar2", height = 900, width = 1200)
            )
        )
    )
))


server <- function(input, output, session) {
    #add reactive data information. Dataset = built in diamonds data
    H <- read_csv("MasterKickstarter.csv")
    M <- read_csv("Mapping.csv")
    C <- read_csv("County.csv")
    
    H <- as.data.frame(H)
    M <- as.data.frame(M)
    C <- as.data.frame(C)
    
    ### Plotting top twenty citys for a kick ###
    # calculate frequencies
    tab <- table(H$City)
    # sort
    tab_s <- sort(tab)
    # extract 10 most frequent nationalities
    top10 <- tail(names(tab_s), 25)
    # subset of data frame
    d_s <- subset(H, City %in% top10)
    # order factor levels
    d_s$City <- factor(d_s$City, levels = rev(top10))
    
    #function for capitalization
    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)),
              substring(s, 2),
              sep = "",
              collapse = " ")
    }
    
    
    
    M$code <- state.abb[match(M$State, state.name)]
    
    #making temp sets that fit for interactive maps
    H = H[!duplicated(H[, "City"], fromLast = T), ]
    H$State <- sapply(as.character(H$State), simpleCap)
    H$code <- state.abb[match(H$State, state.name)]
    
    H$City <- factor(H$City)
    
    #making quartiles for plotting size
    H$q <-
        with(H, cut(All_Time_Backers_city, quantile(All_Time_Backers_city)))
    levels(H$q) <-
        paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
    H$q <- as.ordered(H$q)
    
    
    CT <- function(r,data,pop){
        
        cali <- map_data("county") %>%
            filter(region == r)
        
        cali_pop <- left_join(cali, pop, by = c("subregion","region"))
        
        cali_pop$pop_cat <- with(cali_pop,
                                 (paste0(cali_pop$subregion, "<br />",
                                         round(cali_pop$MedianBackers), "Median Backers ||",round(cali_pop$MedianUSD),"MedianUSD","<br />",
                                         round(cali_pop$MeanBackers), "Mean Backers ||",round(cali_pop$MeanUSD),"MeanUSD","<br />",
                                         (cali_pop$TotalBackers), "Total Backers ||",(cali_pop$TotalUSD), "TotalUSD")))
        cali_pop[is.na(cali_pop)] <- 0
        cali_pop$pop_cat <- as.factor(cali_pop$pop_cat)
        
        
        
        p <- cali_pop %>%
            group_by(group) %>%
            plot_ly(x = ~long, y = ~lat, color = ~as.factor(TotalUSD), colors = c('#ffeda0','#f03b20'), text = ~pop_cat) %>%
            add_polygons(line = list(width = 0.4),showlegend = FALSE) %>%
            add_polygons(
                fillcolor = 'transparent',
                line = list(color = 'black', width = 0.5),
                showlegend = FALSE
            ) %>%
            layout(
                title = "Backers by County",
                titlefont = list(size = 10),
                xaxis = list(title = "", showgrid = FALSE,
                             zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(title = "", showgrid = FALSE,
                             zeroline = FALSE, showticklabels = FALSE)
            )
        p
        
    }
    
    output$Smap <- renderPlotly({
        M$hover <- with(M, paste("State:",State))
        # give state boundaries a white border
        l <- list(color = toRGB("white"), width = 2)
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            showland = TRUE,
            landcolor = toRGB("grey83"),
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white"),
            showlakes = TRUE,
            lakecolor = toRGB("white"),
            showsubunits = TRUE,
            showcountries = TRUE,
            resolution = 50,
            projection = list(type = 'albers usa',
                              rotation = list(lon = -100)),
            lonaxis = list(
                showgrid = TRUE,
                gridwidth = 0.5,
                range = c(-140,-55),
                dtick = 5
            ),
            lataxis = list(
                showgrid = TRUE,
                gridwidth = 0.5,
                range = c(15, 70),
                dtick = 5
            )
        )
        
        # Plotting a US interactive map
        p <- plot_ly(M,source = "CCM",
                     z = ~ M$`Mean Bakers`,
                     text = ~ M$hover,
                     type = 'choropleth',
                     locations = ~ M$code,
                     locationmode = "USA-states"
        ) %>%
            colorbar(title = "Money") %>%
            layout(
                title = 'Kickstarter USA',
                geo = g
            )
        p
    })
    
    output$Cmap <- renderPlotly({
        s <- event_data("plotly_click", source = "CCM")
        
        if(s$pointNumber == 4) {
            d = "colorado"
        }
        if(s$pointNumber == 3) {
            d = "california"
        }
        if(s$pointNumber == 43) {
            d = "utah"
        }
        if(s$pointNumber == 26) {
            d = "nevada"
        }
        if(s$pointNumber == 43) {
            d = "utah"
        }
        if(s$pointNumber == 1) {
            d = "arizona"
        }
        if(s$pointNumber == 36) {
            d = "oregon"
        }
        if(s$pointNumber == 46) {
            d = "washington"
        }
        if(s$pointNumber == 10) {
            d = "idaho"
        }
        if(s$pointNumber == 24) {
            d = "montana"
        }
        if(s$pointNumber == 49) {
            d = "wyoming"
        }
        if(s$pointNumber == 29) {
            d = "new mexico"
        }
        if(s$pointNumber == 42) {
            d = "texas"
        }
        if(s$pointNumber == 35) {
            d = "oklahoma"
        }
        if(s$pointNumber == 14) {
            d = "kansas"
        }
        if(s$pointNumber == 25) {
            d = "nebraska"
        }
        if(s$pointNumber == 40) {
            d = "south dakota"
        }
        if(s$pointNumber == 33) {
            d = "north dakota"
        }
        if(s$pointNumber == 21) {
            d = "minnestota"
        }
        if(s$pointNumber == 13) {
            d = "iowa"
        }
        if(s$pointNumber == 23) {
            d = "missouri"
        }
        if(s$pointNumber == 2) {
            d = "arkansas"
        }
        if(s$pointNumber == 16) {
            d = "louisiana"
        }
        if(s$pointNumber == 48) {
            d = "wisconsin"
        }
        if(s$pointNumber == 11) {
            d = "illinois"
        }
        if(s$pointNumber == 12) {
            d = "indiana"
        }
        if(s$pointNumber == 20) {
            d = "michigan"
        }
        if(s$pointNumber == 34) {
            d = "ohio"
        }
        if(s$pointNumber == 37) {
            d = "pennsylvania"
        }
        if(s$pointNumber == 30) {
            d = "new york"
        }
        if(s$pointNumber == 44) {
            d = "vermont"
        }
        if(s$pointNumber == 27) {
            d = "new hampshire"
        }
        if(s$pointNumber == 17) {
            d = "maine"
        }
        if(s$pointNumber == 19) {
            d = "massachusetts"
        }
        if(s$pointNumber == 38) {
            d = "road island"
        }
        if(s$pointNumber == 5) {
            d = "connecticut"
        }
        if(s$pointNumber == 28) {
            d = "new jersey"
        }
        if(s$pointNumber == 6) {
            d = "delaware"
        }
        if(s$pointNumber == 18) {
            d = "maryland"
        }
        if(s$pointNumber == 47) {
            d = "west virginia"
        }
        if(s$pointNumber == 45) {
            d = "virginia"
        }
        if(s$pointNumber == 15) {
            d = "kentucky"
        }
        if(s$pointNumber == 41) {
            d = "tennessee"
        }
        if(s$pointNumber == 32) {
            d = "north carolina"
        }
        if(s$pointNumber == 39) {
            d = "south carolina"
        }
        if(s$pointNumber == 9) {
            d = "georgia"
        }
        if(s$pointNumber == 8) {
            d = "florida"
        }
        if(s$pointNumber == 0) {
            d = "alabama"
        }
        if(s$pointNumber == 22) {
            d = "mississippi"
        }
        else {
            plotly_empty()
        }
        
        CT(d,H,C)
        
    })
    
    output$plotBar1 <- renderPlotly({
        p1 <- d_s %>% count(City, status) %>%
            plot_ly(x = ~ City,
                    y = ~ n,
                    color = ~ status)
        p1
        
    })
    
    output$plotBar2 <- renderPlotly({
        p2 <- H %>% count(Categories, status) %>%
            plot_ly(x = ~ Categories,
                    y = ~ n,
                    color = ~ status)
        
        p2
    })
    
    output$plotMap <- renderPlotly({
        #preparing the hover text
        M$hover <- with(
            M,
            paste(
                State,
                '<br>',
                "Pledges_total",
                M$`Total Pledged`,
                "Backers_total",
                M$`Total Backers`,
                "<br>",
                "Mean_pledges",
                M$`Mean Campaign USD`,
                "Mean_backers",
                M$`Mean Bakers`,
                "<br>",
                "Median Goal %",
                M$`Median Percent of Goal`,
                "Number of projects",
                M$`Projects Per`
            )
        )
        
        g <- list(
            scope = 'north america',
            showland = TRUE,
            landcolor = toRGB("grey83"),
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white"),
            showlakes = TRUE,
            lakecolor = toRGB("white"),
            showsubunits = TRUE,
            showcountries = TRUE,
            resolution = 50,
            projection = list(type = 'conic conformal',
                              rotation = list(lon = -100)),
            lonaxis = list(
                showgrid = TRUE,
                gridwidth = 0.5,
                range = c(-140,-55),
                dtick = 5
            ),
            lataxis = list(
                showgrid = TRUE,
                gridwidth = 0.5,
                range = c(15, 70),
                dtick = 5
            )
        )
        
        #plotting an interactive map for states and cities
        p <- plot_geo(H, sizes = c(5, 250)) %>%
            add_markers(
                x = ~ H$Longitude,
                y = ~ H$Latitude,
                size = ~ H$All_Time_Backers_city,
                color = ~ q,
                text = ~ paste(H$City, "<br />",
                               H$All_Time_Backers_city, "Backers")
            ) %>%
            add_trace(M,
                      z = ~ M$`Mean Campaign USD`,
                      text = ~ M$hover,
                      locations = ~ M$code
                      ,
                      locationmode = "USA-states"
            ) %>%
            layout(title = 'Backers City All Time', geo = g)
        p
        
    })
    
    output$plotInt <- renderPlotly({
        p <- H %>%
            plot_ly() %>%
            add_trace(
                type = 'parcoords',
                line = list(
                    color = ~ backers_count,
                    colorscale = 'Jet',
                    showscale = TRUE,
                    reversescale = TRUE,
                    cmin = 2,
                    cmax = 1500
                ),
                dimensions = list(
                    list(
                        range = c(0, 92),
                        constrantrange = c(0, 30),
                        label = 'Time',
                        values = ~ Length_of_kick
                    ),
                    list(
                        range = c(0, 2000),
                        constraintrange = c(0, 1000),
                        label = 'Pledge USD',
                        values = ~ Pledge_per_person
                    ),
                    list(
                        range = c(0, 8000000),
                        constrantrange = c(0, 3000000),
                        label = 'Population',
                        values = ~ MasterKickstarter$City_Pop
                    ),
                    list(
                        range = c(0, 1600),
                        constraintrange = c(0, 500),
                        label = 'Days Making',
                        values = ~ Days_spent_making_campign
                    ),
                    list(
                        tickvals = c(1, 2, 3, 4, 5),
                        ticktext = c('cancled', 'failed', 'live', 'successful', 'suspended'),
                        label = 'Status',
                        values = ~ as.integer(as.factor(status))
                    ),
                    list(
                        range = c(0, 1000000),
                        constraintrange = c(0, 300000),
                        label = 'Goal',
                        values = ~ goal
                    ),
                    list(
                        tickvals = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                        ticktext = c(
                            'art',
                            'comics',
                            'crafts',
                            'dance',
                            'design',
                            'fasion',
                            'film',
                            'food',
                            'games',
                            'journalism',
                            'music',
                            'photogaphy',
                            'publishing',
                            'technology',
                            'theator'
                        ),
                        label = 'Catigories',
                        values = ~ as.integer(as.factor(Categories))
                    ),
                    list(
                        range = c( ~ min(Prct_goal), 1200),
                        constraintrange = c(0, 500),
                        label = 'Prct goal',
                        values = ~ Prct_goal
                    )
                )
            )
        p
    })
    
}

shinyApp(ui, server)