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
library(vegan)
library(DT)
library(ggbeeswarm)

#setwd('C:\\Users\\ahearn\\Desktop\\gt\\ppol563\\migration')

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
                         selectizeInput("selectState", h3("State"), options = list(maxOptions = 10000),
                                        choices = c(NULL)),
                         selectizeInput("select", h3("Institution"), options = list(maxOptions = 10000),
                         choices = c(NULL))
                     ),
                     mainPanel(h1(''),
                     mainPanel(h1(textOutput("inst_header"))),
                     mainPanel(textOutput('inst_stats'),br()),
                     mainPanel(strong(textOutput('div_header')), textOutput('div_index'), br()),
                     mainPanel(plotlyOutput("Imap3", height = 800, width = 1000)),
                     mainPanel(h3(textOutput('table_header'))),
                     mainPanel(dataTableOutput('inst_tab'))
                 ))),
        tabPanel("Geographic Diversity Index", fluid = TRUE,
                    mainPanel(h1('Geographic Diversity Index')),
                    mainPanel(textOutput("GDI_text1"), br()),
                    mainPanel(textOutput("GDI_text2"), br()),
                    mainPanel(plotlyOutput("GDI", height = 500, width = 750)),
                    selectizeInput("pickSect", "Sector", choices = c(NULL)),
                    selectizeInput("pickBar", "Selectivity", choices = c(NULL)),
                    mainPanel(dataTableOutput("GDI_tab"))
                     )
        ),
        useShinyjs(),
        uiOutput("back")
)

server <- function(input, output, session, clHist) {
    
    #add reactive data information. Dataset = built in diamonds data
    M <- read_csv("Mapping.csv")
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
    
    D$sector <- factor(D$sector, levels = c("Public, 4-year or above",
                                           "Private not-for-profit, 4-year or above",
                                           "Private for-profit, 4-year or above",
                                           "Public, 2-year",
                                           "Private not-for-profit, 2-year",
                                           "Private for-profit, 2-year",
                                           "Private not-for-profit, less-than 2-year",
                                           "Private for-profit, less-than 2-year"))
    
    D <- D %>% 
        mutate(barrons14 = case_when(
        barrons14 == 'Not Ranked: <2yr' ~ "Not Ranked",
        barrons14 == 'Not Ranked: 2yr' ~ "Not Ranked",
        barrons14 == 'Not Ranked: 4yr' ~ "Not Ranked",
        barrons14 == 'Not Ranked: Adm. unit' ~ "Not Ranked",
        barrons14 == 'Noncompetitive' ~ "Noncompetitive",
        barrons14 == 'Less Competitive' ~ 'Less Competitive',
        barrons14 == 'Special' ~ 'Special Focus',
        barrons14 == 'Most Competitive' ~ 'Most Competitive',
        barrons14 == 'Highly Competitive' ~ 'Highly Competitive',
        barrons14 == 'Very Competitive' ~ 'Very Competitive',
        barrons14 == 'Competitive' ~ 'Competitive')) %>% 
        mutate(barrons14 = as.factor(barrons14)) %>% 
        mutate(barrons14 = factor(barrons14, levels = c(
            'Most Competitive',
            'Highly Competitive',
            'Very Competitive',
            'Competitive',
            'Less Competitive',
            'Noncompetitive',
            'Not Ranked',
            'Special Focus',
            ''
        )))
    
    l = append('Select institution', S$instnm)
    updateSelectizeInput(session, 'selectState', choices = append('Select state', unique(I$region)), server = TRUE)
    updateSelectizeInput(session, 'pickSect', choices = append('Select Sector', levels(D$sector)), server = TRUE)
    updateSelectizeInput(session, 'pickBar', choices = append('Select Selectivity', c(
        'Most Competitive',
        'Highly Competitive',
        'Very Competitive',
        'Competitive',
        'Less Competitive',
        'Noncompetitive',
        'Not Ranked',
        'Special Focus')), server = TRUE)
    
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
        inst <- data1 %>% 
                mutate(X1 = row_number()) %>% filter(X1 == index)
        inst <- inst %>% 
            merge(data2, by = c('instnm', 'barrons14', 'sector', 'tot_enr')) %>% 
            select(instnm, long_state_res, state_enr, tot_enr, sector, barrons14, code)
        name = inst$instnm[1]
        
        
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
        
        p <- plot_ly(inst,
                     z = z,
                     zmin = 0,
                     zmax = as.numeric(max(inst$tot_enr)),
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
        median(z)
        quantiles = unique(quantile(z, seq(0,1,0.1)))
        p <- plot_ly(s,source = "CCM",
                     z = z,
                     zmin = max(z)*.005,
                     zmax = max(z)*.25,
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
            In(d,I2)
    }
    )

        filter1 <- reactive({
            if (FALSE == FALSE) {
                    barrons = input$pickBarrons1
                    sec = input$pickSector1
                    if ((length(barrons) == 9) & length(sec) == 8){
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

        if (length(barrons) == 9){
            return(S)
        }
        S %>% filter(barrons14 %in% barrons) #%>% mutate(X1 = row_number())
    })
    
    filter_gdi_s <- reactive({
        sect = list(input$pickSect)
        bar = list(input$pickBar)
        if (is.null(sect)) {
            return(D)
        }
        
        if (sect[1] == 'Select Sector') { # no filter needed
            sect1 = unique(D$sector)
        }
        else {sect1 = sect}
        
        if (bar[1] == 'Select Selectivity') { # no filter needed
            bar1 = unique(D$barrons14)
        }
        else {bar1 = bar}
        
        D %>% filter(sector %in% sect1) %>% filter(barrons14 %in% bar1)
    }
    )

    
    output$Imap <- 
        renderPlotly({
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
        p
        }
        )
    
    output$Imap2 <- renderPlotly({
            s <- event_data("plotly_click", source = 'InstLev')
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
        print(S1)
        inst <- S1 %>% 
            merge(D, by = c('instnm', 'sector', 'tot_enr')) %>% 
            select(instnm, long_state_res, state_enr, tot_enr, sector, code)
        print(inst)
        name = inst$instnm[1]
        if (is.na(name)){
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
    
output$inst_header <-renderText({
    print(input$select)
    if (is.null(input$select)) {return("")}
    if (input$select == 'Select institution' | is.na(input$select)) { 
        return("")
    }
    else {
        paste0(input$select, " Geographic Profile")
    }
})

output$div_header <-renderText({
    if (is.null(input$select)) {return("")}
    if (input$select == 'Select institution' | is.na(input$select)) { 
        return("")
    }
    else {
        paste0("Geographic diversity index: ")
    }
})

output$div_index <-renderText({
    if (input$select == 'Select institution' | is.na(input$select)) { 
        return("")
    }
    else {
        D1 <- D %>% filter(instnm == input$select) %>%   
            filter(long_state_res != 'US total')
        div = round(diversity(as.numeric(D1$state_enr), "simpson"),3)
        paste(div)   
    }
})

output$inst_stats <-renderText({
    if (input$select == 'Select institution' | is.na(input$select)) { 
        return("")
    }
    else {
        territories = c("Guam", 
                        "Virgin Islands", 
                        "American Samoa",
                        "Marshall Islands",
                        "Federated States of Micronesia",
                        "Palau",
                        "Puerto Rico",
                        "District of Columbia")
        
        foreign = c('Foreign countries')
        
        other = c("US total", "Outlying areas total", "State unknown", "Residence not reported")
        
        
        num_states <- D %>% 
            filter(instnm == input$select) %>%
            filter(!(long_state_res %in% other)) %>% 
            filter(!(long_state_res %in% territories)) %>% 
            filter(!(long_state_res %in% foreign)) %>% 
            mutate(state_enr = ifelse(state_enr == 0, NA, state_enr)) %>% 
            select(state_enr, long_state_res) %>% 
            drop_na() %>% nrow()
        
        other_countries <- D %>% 
            filter(instnm == input$select) %>%
            filter(long_state_res %in% foreign) %>% 
            mutate(state_enr = ifelse(state_enr == 0, NA, state_enr)) %>% 
            select(state_enr, long_state_res) %>% 
            drop_na() %>% nrow()
        
        ter <- D %>% 
            filter(instnm == input$select) %>%
            filter(long_state_res %in% territories) %>% 
            mutate(state_enr = ifelse(state_enr == 0, NA, state_enr)) %>% 
            select(state_enr, long_state_res) %>% 
            drop_na() %>% nrow()
        
        if (num_states == 1) {states = 'state'} else {states = 'states'}
        if (ter == 1) {teri = 'territory'} else {teri = 'territories'}


        if (ter == 0 & other_countries == 0) {
            paste0(input$select, " represents ", num_states, " U.S. ", states, ".")
        }
            
        else if (ter == 0) {
            paste0(input$select, " represents ", 
                    num_states, " U.S. ", states, 
                    " plus foreign countries.")
        }
        else if (other_countries == 0) {
            paste0(input$select, " represents ", 
                   num_states, " U.S. ", states, 
                   " and ", ter, " U.S. ", teri, ".")
        }
        
        else {
            paste0(input$select, " represents ", 
                   num_states, " ", states, 
                   ", ", ter, " U.S. ", teri, ", plus foreign countries.")
            }
        
    }
})
    

output$inst_tab <- renderDataTable({
    if (input$select == 'Select institution' | is.na(input$select)) { 
        D1 <- data.frame("State" = NA, "# Students" = NA)
        colnames(D1) <- c('State', '# Students')
        return(D1)
    }
    else {
        other = c("US total", "Outlying areas total")
        D1 <- D %>% 
            filter(instnm == input$select) %>%
            filter(!(long_state_res %in% other)) %>% 
            mutate(state_enr = ifelse(state_enr == 0, NA, state_enr)) %>% 
            select(long_state_res, state_enr) %>%
            arrange(desc(as.numeric(state_enr))) %>% 
            rename(State = long_state_res, `# Students` = state_enr) %>% 
            drop_na()
    }
    return(D1)
    }
)

output$table_header <- renderText({
    if (input$select == 'Select institution' | is.na(input$select)) { 
        return("")
    }
    else{
        return("Enrollment breakdown of Fall 2018 entering class:")
    }
})

output$GDI <- renderPlotly({
    sdi_plot <- D %>% 
        filter(barrons14 !=  "Not Ranked: Sector Unkwn." ) %>%
        filter(long_state_res == long_state_inst) %>% 
        filter((sector == "Public, 4-year or above" |  
              (sector == "Private not-for-profit, 4-year or above" ) |
              (sector == 'Private for-profit, 4-year or above'))) %>% 
        filter(barrons14 !=  "Not Ranked: 2yr" ) %>%
        filter(barrons14 !=  "Not Ranked: Admin. Unit" ) %>%
        filter(as.numeric(tot_enr) >  50) %>%
        mutate(barrons14 = case_when(
            barrons14 == 'Not Ranked' ~ "Not Ranked",
            barrons14 == 'Noncompetitive' ~ "Less Competitive,\nNoncompetitive",
            barrons14 == 'Less Competitive' ~ 'Less Competitive,\nNoncompetitive',
            barrons14 == 'Special Focus' ~ 'Most Competitive,\nSpecial Focus',
            barrons14 == 'Most Competitive' ~ 'Most Competitive,\nSpecial Focus',
            barrons14 == 'Highly Competitive' ~ 'Highly\nCompetitive',
            barrons14 == 'Very Competitive' ~ 'Very\nCompetitive',
            barrons14 == 'Competitive' ~ 'Competitive')) %>% 
        filter(as.numeric(sdi) > 0) %>% filter(as.numeric(sdi) <=1) %>% 
        mutate(Control = control) %>% 
        
        mutate(barrons14 = factor(barrons14, levels = c(
            'Most Competitive,\nSpecial Focus',
            'Highly\nCompetitive',
            'Very\nCompetitive',
            'Competitive',
            'Less Competitive,\nNoncompetitive',
            'Not Ranked'))) %>% 
        mutate(index = as.numeric(sdi)) %>% 
        
        ggplot() +
        geom_quasirandom(aes(barrons14, as.numeric(index), col = Control, text = paste0(instnm, '<br>GDI: ', round(index, 3))), alpha = .5, shape = 20) + 
        xlab("Barron's Classification (2014)") +
        ylab("GDI") +
        theme_minimal() +
        scale_y_continuous(limits=c(0., 1), breaks=c(0, .25, .5, .75, 1)) +
        guides(fill = guide_legend(title = "Control"))+
        labs(title = 'Geographic Diversity Index by Selectivity: 4-year or above, 2018')
    
    ggplotly(p=sdi_plot,
             tooltip = 'text')
}
)

output$GDI_text1 <- renderText({
    text1 = "Geographic diversity index, derived from Simpson's Diversity Index, is a measure of how diverse an institution's geographic makeup is. This value ranges from 0 to 1, with 1 being the most diverse and 0 being the least diverse."
    return(text1)
}
)

output$GDI_text2 <- renderText({
    text2 = "If an institution pulls from all 50 states with an equal distribution from each state, the institution will have a diversity index very close to 1. On the contrary, if an institution's student body is composed of students from only 1 state, this institution will have a very low diversity index."
    return(text2)
})

output$GDI_tab <- renderDataTable({
    D1 <- filter_gdi_s()
    D1 <- D1 %>% 
        filter(long_state_res == long_state_inst) %>% 
        filter(barrons14 !=  "Not Ranked: Admin. Unit" ) %>%
        filter(as.numeric(tot_enr) >  50) %>%
        filter(as.numeric(sdi) > 0) %>% filter(as.numeric(sdi) <=1) %>% 
        mutate(Control = control) %>% 
        mutate(GDI = round(as.numeric(sdi), 4)) %>% 
        arrange(desc(GDI)) %>% 
        rename(Institution = instnm, 
               State = long_state_inst,
               Sector = sector,
               Selectivity = barrons14) %>% 
        select(Institution, State, Sector, Selectivity, GDI)
    
    return(D1)
})


}

shinyApp(ui, server)