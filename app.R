library(shiny)
library(rsconnect)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(Rcpp)
library(shinyjs)
library(plotly)
library(shinythemes)
library(tidyverse)


setwd('C:\\Users\\ahearn\\Desktop\\gt\\ppol563\\migration')

ui <- fluidPage(theme = shinytheme('yeti'),
                titlePanel("College Migration"),
    tabsetPanel(
        tabPanel("State Level", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("pickBarrons1", h3("Barron's"), 
                            choices = list(
                             "Most Competitive",
                             "Highly Competitive",
                             "Very Competitive",
                             "Competitive",
                             "Less Competitive",
                             "Noncompetitive",
                             "Special",
                             "Not Ranked: 4yr",
                             "Not Ranked: 2yr"), 
                             
                             selected = c("Most Competitive",
                                          "Highly Competitive",
                                          "Very Competitive",
                                          "Competitive",
                                          "Less Competitive",
                                          "Noncompetitive",
                                          "Special",
                                          "Not Ranked: 4yr",
                                          "Not Ranked: 2yr")),
                         
                         checkboxGroupInput("pickSector1", h3("Sector"), 
                            choices = list(
                             "Public, 4-year or above",
                             "Private not-for-profit, 4-year or above",
                             "Private for-profit, 4-year or above",
                             "Public, 2-year",
                             "Private not-for-profit, 2-year",
                             "Private for-profit, 2-year",
                             "Private not-for-profit, less-than 2-year",
                             "Private for-profit, less-than 2-year"), 
                             
                             selected = c("Public, 4-year or above",
                                          "Private not-for-profit, 4-year or above",
                                          "Private for-profit, 4-year or above",
                                          "Public, 2-year",
                                          "Private not-for-profit, 2-year",
                                          "Private for-profit, 2-year",
                                          "Private not-for-profit, less-than 2-year",
                                          "Private for-profit, less-than 2-year")
                            )
                         ),
                 
                     mainPanel(plotlyOutput("Smap", height = 800, width = 1000)))),
        
        tabPanel("Institution Level", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         checkboxGroupInput("pickBarrons2", h3("Barron's"), 
                                            choices = list(
                                                "Most Competitive",
                                                "Highly Competitive",
                                                "Very Competitive",
                                                "Competitive",
                                                "Less Competitive",
                                                "Noncompetitive",
                                                "Special",
                                                "Not Ranked: 4yr",
                                                "Not Ranked: 2yr"), 
                                            
                                            selected = c("Most Competitive",
                                                         "Highly Competitive",
                                                         "Very Competitive",
                                                         "Competitive",
                                                         "Less Competitive",
                                                         "Noncompetitive",
                                                         "Special",
                                                         "Not Ranked: 4yr",
                                                         "Not Ranked: 2yr")),
                     ),
                             mainPanel(plotlyOutput("Imap", height = 800, width = 1000),
                                       plotlyOutput("Imap2", height = 800, width = 1000)),

                    )),
        tabPanel("Institution Level2", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("selectState", h3("State"), options = list(maxOptions = 10000),
                                        choices = NULL),
                         selectizeInput("select", h3("Institution"), options = list(maxOptions = 10000),
                         choices = NULL)
                     ),
                     mainPanel(plotlyOutput("Imap3", height = 800, width = 1000),
                 )))
        ),
        useShinyjs(),
        uiOutput("back")
)

server <- function(input, output, session, clHist) {
    
    #add reactive data information. Dataset = built in diamonds data
    M <- read_csv("Mapping.csv")
    C <- read_csv("County.csv")
    I <- read_csv("click_state_gb.csv")
    
    S <- read_csv("inst_list.csv")
    L <- lapply(S, iconv, to = "UTF-8")
    n <- length(L[[1]])
    S <- structure(L, row.names = c(NA, -n), class = "data.frame")
    
    D <- read_csv("institutions.csv")
    L <- lapply(D, iconv, to = "UTF-8")
    n <- length(L[[1]])
    D <- structure(L, row.names = c(NA, -n), class = "data.frame")
    
    M <- as.data.frame(M)
    C <- as.data.frame(C)
    I <- as.data.frame(I)
    S <- as.data.frame(S) 
    D <- as.data.frame(D)
    S$sector = factor(S$sector, levels = c("Public, 4-year or above",
                                          "Private not-for-profit, 4-year or above",
                                          "Private for-profit, 4-year or above",
                                          "Public, 2-year",
                                          "Private not-for-profit, 2-year",
                                          "Private for-profit, 2-year",
                                          "Private not-for-profit, less-than 2-year",
                                          "Private for-profit, less-than 2-year"))
    
    ### Plotting top twenty citys for a kick ###
    # calculate frequencies
    # sort
    # extract 10 most frequent nationalities
    # subset of data frame
    
    l = append('Select institution', S$instnm)
    updateSelectizeInput(session, 'select', choices = append('Select institution', S$instnm), server = TRUE)
    updateSelectizeInput(session, 'selectState', choices = append('Select state', unique(I$region)), server = TRUE)
    
    #function for capitalization
    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)),
              substring(s, 2),
              sep = "",
              collapse = " ")
    }
    I <- I %>% mutate(state_pct = round(state_pct, 4)) %>% 
               mutate(region = str_to_lower(region)) %>% 
               filter(region != "district of columbia")
    
    M <- M %>% mutate(pct = round(pct, 4)) %>% filter(State != 'District of Columbia')

    M$code <- state.abb[match(M$State, state.name)]
    I$code <- state.abb[match(I$state, state.name)]
    D$code <- state.abb[match(D$long_state_res, state.name)]
    
    #making temp sets that fit for interactive maps

    #making quartiles for plotting size
    
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
    
    Inst <- function(index, curve, data1, data2) {
        print(curve)
        if (!is.na(curve)) {
            if (curve == 0){
                inst <- data1 %>% filter(sector == 'Public, 4-year or above') %>% 
                   mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 1){
                inst <- data1 %>%  filter(sector == 'Private not-for-profit, 4-year or above') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 2){
                inst <- data1 %>%  filter(sector == 'Private for-profit, 4-year or above') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 3){
                inst <- data1 %>%  filter(sector == 'Public, 2-year') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 4){
                inst <- data1 %>%  filter(sector == 'Private not-for-profit, 2-year') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 5){
                inst <- data1 %>% filter(sector == 'Private for-profit, 2-year') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 6){
                inst <- data1 %>% filter(sector == 'Private not-for-profit, less-than 2-year') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
            if (curve == 7){
                inst <- data1 %>% filter(sector == 'Private for-profit, less-than 2-year') %>% 
                    mutate(X1 = row_number()) %>% filter(X1 == index)
            }
        }
        
        else {
            inst <- data1 %>% 
                mutate(X1 = row_number()) %>% filter(X1 == index)
        }
        
        print(head(inst))
        
        inst <- inst %>% 
            merge(data2, by = c('instnm', 'barrons14', 'sector', 'tot_enr')) %>% 
            select(instnm, long_state_res, state_enr, tot_enr, sector, barrons14, code)
        name = inst$instnm[1]
        
        
        #print(name)
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
        inst <- inst %>% filter(!is.na(code))
        
        z <- inst$state_enr
        
        p <- plot_ly(inst,source = "InstLev",
                     z = z,
                     zmin = 0,
                     zmax = as.numeric(max(inst$tot_enr))^.85,
                     #text = ~ M$hover,
                     type = 'choropleth',
                     locations = ~ inst$code,
                     locationmode = "USA-states",
                     colorscale = 'Reds',
                     showscale = F,
                     reversescale = F
        ) %>% 
            colorbar(title = "Num students") %>%
            layout(
                title = paste0('Where do students who attend ', name, ' come from?'),
                geo = g
            ) 
        
        #s <- event_data("plotly_click")
        #print(s)
        p
        
    }
    
    In <- function(r,pop){
        selected = (simpleCap(r))
        s <- pop %>%
            filter(region == r)

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
        
        #s$code <- state.abb[match(s$state, state.name)]
        s <- s %>% filter(!is.na(code)) %>% 
            mutate(region = str_to_title(region))
        
        s$code_orig <- state.abb[match(s$region, state.name)]
        s <- s %>%  select(state, s_enrollment, code)
        z <- s$s_enrollment
        z1 <- z[z < max(z)]
        quantiles = unique(quantile(z1, seq(0,1,0.1)))
        p <- plot_ly(s,source = "CCM",
                     z = z,
                     zmin = 0,
                     zmax = max(z1) ^ 1.10,
                     #text = ~ M$hover,
                     type = 'choropleth',
                     locations = ~ s$code,
                     locationmode = "USA-states",
                     colorscale = 'Reds',
                    showscale = F,
                     reversescale = F
        ) %>% 
            colorbar(title = "Num students") %>%
            layout(
                title = paste0('Where do students who attend college in ', selected, ' come from?'),
                geo = g
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
            landcolor = toRGB("grey75"),
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white"),
            showlakes = TRUE,
            lakecolor = toRGB("white"),
            showsubunits = TRUE,
            showcountries = TRUE,
            resolution = 100,
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
                     z = 0,
                     text = NA,
                     type = 'choropleth',
                     locations = ~ M$code,
                     locationmode = "USA-states",
                     colorscale = "RdBu",
                     reversescale = F,
                     showscale = F
        ) %>% 
            colorbar(title = "Pct. Students In-state") %>%
            layout(
                title = 'Where do college students from your state come from?\nClick on any state to begin.',
                geo = g
            ) 
        
        if (is.null(event_data("plotly_click", source = "CCM"))) {
            isFlag = TRUE
            return(p)
        }
        else { # user clicked
            s <- event_data("plotly_click", source = "CCM")
            #main map
            ## state clicks
            {
            if(s$pointNumber == 0) {
                d = "alabama"
            }
            if(s$pointNumber == 1) {
                d = "alaska"
            }
            if(s$pointNumber == 2) {
                d = "arizona"
            }
            if(s$pointNumber == 3) {
                d = "arkansas"
            }
            if(s$pointNumber == 4) {
                d = "california"
            }
            if(s$pointNumber == 5) {
                d = "colorado"
            }
            if(s$pointNumber == 6) {
                d = "connecticut"
            }
            if(s$pointNumber == 7) {
                d = "delaware"
            }
            if(s$pointNumber == 8) {
                d = "florida"
            }
            if(s$pointNumber == 9) {
                d = "georgia"
            }
            if(s$pointNumber == 10) {
                d = "hawaii"
            }
            if(s$pointNumber == 11) {
                d = "idaho"
            }
            if(s$pointNumber == 12) {
                d = "illinois"
            }
            if(s$pointNumber == 13) {
                d = "indiana"
            }
            if(s$pointNumber == 14) {
                d = "iowa"
            }
            if(s$pointNumber == 15) {
                d = "kansas"
            }
            if(s$pointNumber == 16) {
                d = "kentucky"
            }
            if(s$pointNumber == 17) {
                d = "louisiana"
            }
            if(s$pointNumber == 18) {
                d = "maine"
            }
            if(s$pointNumber == 19) {
                d = "maryland"
            }
            if(s$pointNumber == 20) {
                d = "massachusetts"
            }
            if(s$pointNumber == 21) {
                d = "michigan"
            }
            if(s$pointNumber == 22) {
                d = "minnesota"
            }
            if(s$pointNumber == 23) {
                d = "mississippi"
            }
            if(s$pointNumber == 24) {
                d = "missouri"
            }
            if(s$pointNumber == 25) {
                d = "montana"
            }
            if(s$pointNumber == 26) {
                d = "nebraska"
            }
            if(s$pointNumber == 27) {
                d = "nevada"
            }
            if(s$pointNumber == 28) {
                d = "new hampshire"
            }
            if(s$pointNumber == 29) {
                d = "new jersey"
            }
            if(s$pointNumber == 30) {
                d = "new mexico"
            }
            if(s$pointNumber == 31) {
                d = "new york"
            }
            if(s$pointNumber == 32) {
                d = "north carolina"
            }
            if(s$pointNumber == 33) {
                d = "north dakota"
            }
            if(s$pointNumber == 34) {
                d = "ohio"
            }
            if(s$pointNumber == 35) {
                d = "oklahoma"
            }
            if(s$pointNumber == 36) {
                d = "oregon"
            }
            if(s$pointNumber == 37) {
                d = "pennsylvania"
            }
            if(s$pointNumber == 38) {
                d = "rhode island"
            }
            if(s$pointNumber == 39) {
                d = "south carolina"
            }
            if(s$pointNumber == 40) {
                d = "south dakota"
            }
            if(s$pointNumber == 41) {
                d = "tennessee"
            }
            if(s$pointNumber == 42) {
                d = "texas"
            }
            if(s$pointNumber == 43) {
                d = "utah"
            }
            if(s$pointNumber == 44) {
                d = "vermont"
            }
            if(s$pointNumber == 45) {
                d = "virginia"
            }
            if(s$pointNumber == 46) {
                d = "washington"
            }
            if(s$pointNumber == 47) {
                d = "west virginia"
            }
            if(s$pointNumber == 48) {
                d = "wisconsin"
            }
            if(s$pointNumber == 49) {
                d = "wyoming"
            }
                else {
                    plotly_empty()
                }
            }
        }
            isFlag = FALSE
            I2 = filter1()
            print(head(I2))
            In(d,I2)
    }
    )

        filter1 <- reactive({
            if (FALSE == FALSE) {
                    barrons = input$pickBarrons1
                    sec = input$pickSector1
                    if ((length(barrons) == 9) & length(sec) == 8){
                        print('done')
                        return(I %>% group_by(region, state, code) %>% 
                                   summarize(s_enrollment = sum(s_enrollment)))
                    }
                    else {
                        return(I %>% filter(barrons14 %in% barrons) %>%
                                   filter(sector %in% sec) %>% 
                                   group_by(region, state, code) %>% 
                                   summarize(s_enrollment = sum(s_enrollment)))
                    }
                }
    })
    

    filter_s <- reactive({
        barrons = input$pickBarrons2
        print(length(barrons))
        
        if (length(barrons) == 9){
            print('done')
            return(S)
        }
        print('barrons')
        print(head(S))
        S %>% filter(barrons14 %in% barrons) #%>% mutate(X1 = row_number())
    })
    
    output$Imap <- 
        renderPlotly({
            print('here')
        S1 = as_tibble(filter_s())
        # give state boundaries a white border
        l <- list(color = toRGB("black"), width = 2)
        S1$hover = with(S1, paste(instnm))
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            showland = TRUE,
            landcolor = toRGB("grey85"),
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white"),
            showlakes = TRUE,
            lakecolor = toRGB("white"),
            showsubunits = TRUE,
            showcountries = TRUE,
            resolution = 100,
            projection = list(type = 'usa',
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
        p <- plot_geo(S1, source = "InstLev") %>%
            add_markers(
                x = ~ S1$longi,
                y = ~ S1$lati,
                text = ~ S1$hover,
                size = ~ as.integer(S1$tot_enr),
                color = as.factor(S1$sector),
                showlegend = TRUE
            ) %>%
            layout(title = 'Click on institution to begin:', geo = g)
        print("hell0?")
        p
        }
        )
    
    output$Imap2 <- renderPlotly({
            s <- event_data("plotly_click", source = 'InstLev')
            print(s)
            if (is.null(s)){
                plotly_empty()
            }
            
            else
            {
                runjs("Shiny.setInputValue('plotly_selected-InstLev', null);")
                clickedmap = TRUE
                S1 = as_tibble(filter_s())
                index = S1 %>% filter()
                index = s$pointNumber + 1
                curve = s$curveNumber
                Inst(index, curve, S1, D)
            }
        }
        )
    
    output$Imap3 <- renderPlotly({
        filterState()
        imp = (input$select)
        S1 = S %>% filter(instnm == imp)
        index = S1$X1[1]
        curve = NULL
        inst <- S1 %>% 
            merge(D, by = c('instnm', 'barrons14', 'sector', 'tot_enr')) %>% 
            select(instnm, long_state_res, state_enr, tot_enr, sector, barrons14, code)
        
        name = inst$instnm[1]
        
        if (is.na(name)){
            print('nullname')
            name = 'your institution'
            }
        print(name)
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
        inst <- inst %>% filter(!is.na(code))
        
        z <- inst$state_enr
        
        p <- plot_ly(inst,source = "InstLev",
                     z = z,
                     zmin = 0,
                     zmax = as.numeric(max(inst$tot_enr))^.85,
                     #text = ~ M$hover,
                     type = 'choropleth',
                     locations = ~ inst$code,
                     locationmode = "USA-states",
                     colorscale = 'Reds',
                     showscale = F,
                     reversescale = F
        ) %>% 
            colorbar(title = "Num students") %>%
            layout(
                title = paste0('Where do students who attend ', name, ' come from?'),
                geo = g
            ) 
        
        #s <- event_data("plotly_click")
        #print(s)
        p
    }
    )
    
    filterState <- reactive({
        sel_state = input$selectState
        if (sel_state != 'Select state') {
            print(sel_state)
            D1 <- D %>% filter(long_state_inst == sel_state) %>% 
                select(instnm, long_state_inst) %>% distinct()
            print(head(D1))
            updateSelectizeInput(session, 'select' , choices = append('Select institution', D1$instnm), server = TRUE)
        }
        else {
            updateSelectizeInput(session, 'select', choices = append('Select institution', S$instnm), server = TRUE)
        }
    })
    
    output$clicked <- renderPrint({
        s <- event_data("plotly_click", source = "Src")
        s
    })
    output$selection <- renderPrint({
        s <- event_data("plotly_selected", source = "Src")
        s
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