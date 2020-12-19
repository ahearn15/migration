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
library(shinyWidgets)
library(ggbeeswarm)
library(waffle)

#setwd('C:\\Users\\ahearn\\Desktop\\gt\\ppol563\\migration')

ui <- fluidPage(theme = shinytheme('yeti'),
                titlePanel("College Migration by Adam Hearn"),
    tabsetPanel(
        tabPanel("Overview",
                 mainPanel(h1('Do Institutions Serve their Geographic Constituents? Evidence from Student Migratory Patterns')),
                 mainPanel("When a high school senior decides on their postsecondary home for the next few years, a critical component of the decision is the location of their prospective institutions. Will they stay in-state, or will they travel across state-borders to attend an institution in another state? Further, if they do attend an institution located outside of their home state, what factors play a role? While these decisions are made at the student level, this module will investigate these patterns from the institutional point of view.
                           ",
                           br(), br()),
                 mainPanel("In Fall 2018, over 75% of first-time degree/certificate seeking undergraduates remained in their states of residence to attend college (U.S. Department of Education, National Center for Education Statistics, 2020). On the contrary, the other quarter of students who matriculated into the postsecondary sphere after graduating from High School decided to leave their home-state, perhaps so their parents wouldn't constantly be visiting their dorm room. 
                           ", br(), br()),
                 mainPanel(h3('78% of students attend college in their home state')),
                 mainPanel(plotOutput("sorted_bar", width = 600, height = 400)),
                 mainPanel("Though the minority, the geographically-mobile group of students is an important bloc of postsecondary learners to investigate. Institutions may wish to study these the patterns of this group to improve academic profiles and maximize prestige, to offset stagnant or dwindling enrollments, or to generate tuition revenue at the higher out-of-state rate to counter declining state appropriations. 
                           ", br(), br()),
                 mainPanel("Some institutions have built enrollment management strategies around recruiting nonresident students through targeted recruitment (i.e., digital marketing, high school visits, Encoura data lab) and financial awards encouraging out-of-state enrollment (i.e., merit-based scholarships or tuition reciprocity programs). 
                           ", br(), br()),
                 mainPanel("In contrast, some university systems have adopted policies that prioritize resident over nonresident enrollments. For example, Texas's infamous and longstanding \"Top 10 Percent Rule\" guarantees admission to the state's most selective public universities based on high school class rank of native Texans. Similarly, the University of California (UC) system recently capped nonresident undergraduate enrollments to ensure greater access for California residents. 
                           ", br(), br()),
                 mainPanel("The Integrated Postsecondary Data System (IPEDS) provides robust georeferenced data, including institution location and first-time students' state of residence. These metrics allow researchers and analysts to unlock insights into the geography of institutional recruitment and college choice. This module will present unique analyses and visualizations on the geography of first-time college enrollment in the U.S. and provides variation in the geographic composition of first-time enrollees across institutional sector, admissions selectivity, and other defining features. 
                           ", br(), br()),
                 mainPanel(h3("Data and Production")),
                 mainPanel("The primary data source for this module is the 2018 IPEDS Fall Enrollment (EF) survey, which collects residence information for first-time degree/certificate seeking students. This data is collected at the institution level and wrangled at the state-level to produce visualization in this module. Further, this data are merged with selectivity indices from Barron's College Profiles. Data from Barron's are proprietary and may not be collected from this source.",
                           br(), br()),
                 mainPanel("This module was produced in RShiny, using data wrangling techniques in the dplyr library and visualizations using Plotly and ggplot. The code used to build this module and visualizations can be found on my GitHub", a(href="https://www.github.com.ahearn15/migration", "here."),
                           br(), br()),
                 mainPanel(h3("In-state enrollment depends heavily on selectivity"),
                           br(), br()),
                 mainPanel(plotOutput("barrons_plot", width = 600, height = 400)),
                 mainPanel("Selective institutions see a much higher proportion of students who come from out-of-state as opposed non-selective institutions. The most competitive institutions according to Barron's, including Georgetown University, Vanderbilt University, and The Ohio State University, yield an in-state rate of roughly 35%. For noncompetitive institutions, this number is roughly 84%. These statistics suggest that students are more willing to travel far from home to attend selective college and universities, whereas noncompetitive institutions yield a student body that more aligns with the campus's physical location.",
                           br(), br()),
                 mainPanel("These trends are consistent across states. For example, take the slider and see what happens to a state's geographic makeup as the minimum selectivity for institutions to include becomes more selective:",
                           br(), br()),                      
                 mainPanel(sliderTextInput("selectivity_slider","Selectivity" , 
                                           choices = c("Not Ranked", "Noncompetitive", "Less Competitive", "Competitive", "Very Competitive",
                                                       "Highly Competitive", "Most Competitive/Special Focus"), 
                                           selected = c("Not Ranked","Most Competitive/Special Focus"),
                                           dragRange = TRUE,
                                           to_fixed = TRUE),
                           plotlyOutput("slider_map", height = 800, width = 1000)
                           ),
                 mainPanel(strong('This can be explored in greater depth on the "State Level" tab above, where a user can filter by selectivity, control, and level and examine state-level breakdowns of enrollment.'),
                           br(), br()),
                 mainPanel(h2("Geographic Breakdown by Sector")),
                 mainPanel("Since we determined a strong correlation between geographic diversity and institutional selectivity, it shouldn't be a surprise there is a big difference in the geographic makeups of two-year institutions and four-year institutions. Despite greater geographic commitment, 4-year institutions see a greater proportion of students who attend from out-of-state than 2-year institutions. More students attend institutions from out-of-state to obtain a 4-year degree as opposed to a 2-year degree.",
                           br(), br()),
                mainPanel(plotOutput("sector_plot", height = 500, width = 800)),
                 mainPanel("Further, it isn't just in-state tuition rates that effect in-state enrollment. Even private institutions, whose only financial incentive for in-state students are merit-based scholarships, also see a high percentage of students who come from in-state.",
                           br(), br()),
                mainPanel(h2("The Institution Level")),
                mainPanel("Columbia University and CUNY City College are both 4-year institutions located in Upper Manhattan. Though these institutions are a mere 20 minute, 1.1 mile walk from one another, their constituents come from very different geographic backgrounds.",
                          br(), br()),
                mainPanel(fluidRow(splitLayout(cellWidths = c('50%', '50%'), plotlyOutput("columbia", height = 400, width = 500), 
                                               plotlyOutput("cuny", height = 400, width = 500)))),
                mainPanel("97% of first-time students at City College graduated high school from the Empire State. However, at Columbia, this number is a mere 17.77%. Further, 12.3% of the entering Fall 2018 class at Columbia is from California, yet less than 1% of students at City College call the Golden State home. Though these institutions are geographic neighbors and a measly $5 cab fare away from one another, the students they serve come from very different areas.",
                          br(), br()),
                mainPanel("This institutional-level data can be a valuable resource for institutional researchers or admissions counselors. For example, say a high school student would like to attend the University of Texas at Austin. Texas's top-10 percent rule guarantees admission to all state-funded universities should they finish in the top-10 percent of their high school graduating class. As such, many of these students choose to attend UT-Austin. By looking at UT-Austin's profile below, we can see that roughly 9 out of 10 students come from the Lonestar State.",
                          br(), br()),
                mainPanel(plotlyOutput("uta", height = 500, width = 800)),
                
                mainPanel("Meanwhile, only 25 students in UT-Austin's 2018 entering class call Virginia home, thus suggesting that the likelihood of acceptance is slim - but not impossible - for that student from Virginia.",
                    br(), br()),
                mainPanel(strong('Any institution\'s state-level enrollments can be found in the "Institution Level" tab above.'),
                br(), br()),
                 mainPanel(h2("Geographic Diversity Index")),
                mainPanel("Looking at the percentage of students from in-state is not the only metric available to consider. Instead, we can consider a holistic index as a measure of geographic diversity of a particular institution or system. The geographic diversity index (GDI) is derived from the IPEDS Fall Enrollment (EF) survey which includes the state information on the state of residence of the 2018 first-year class. This variable is calculated using Simpson's Diversity Index:"),
                mainPanel(withMathJax(),
                            '$$SDI=\\frac{N(N-1)}{\\sum{n(n-1)}}$$'),
                mainPanel("N represents the number of students in the institution's 2018 first-year class, while n represents the number of students who attend that institution from a particular state. This value ranges from 0 to 1, with 1 being the most geographically diverse. A GDI of zero means that the institution only serves students from one state, where a GDI of 1 means that an institution serves an equal number of students across multiple states.",
                          br(), br()),
                mainPanel("As established, geographic diversity varies heavily by selectivity. This can be explored visually with the plot below:",
                          br(), br()),
                mainPanel(plotlyOutput("GDI_main", height = 500, width = 750)),
                mainPanel("GDI can be used for researchers or prospective students as a proxy for \"institutional prestige,\" i.e. how well known an institution is across the country.", strong("To look up any institution's geographic diversity index and view rankings by geographic diversity, please visit the \"Geographic Diversity\" tab."),
                          br(), br()),
                mainPanel(h2("Conclusion")),
                mainPanel('While this module only touches on the institutional side of examining college migration, there is a whole other facet to consider: the student side. For example, where do high school students from a particular state end up attending? Roman Ruiz, coworker and Researcher at the American Institutes for Research, has touched some on this on', a(href="https://www.romanhighered.com/archive/2020/11/29/out-migration-to-college", "his blog."),
                           br(), br()),
                mainPanel("To answer the framing question of whether or not institutions of higher education serve their geographic constituents, it appears that the answer depends on a variety of factors, namely", strong("selectivity"), "and", strong('state-level policy'), "such as the top-10% rule in Texas, the Hope Scholarship in Georgia, and College Promise programs across the country.",
                          br(), br()),
                 mainPanel("In closing, it is my hope that this module can be used by higher education stakeholders, from institutional researchers, state higher education executive officers, or even college admissions counselors to gauge likelihood of admission by looking at number of students from a particular state that an institution selects.",
                           br(), br()),
                mainPanel(" ",
                          br(), br()),
                mainPanel(h4("Acknowledgements")),
                 mainPanel(helpText("This module was created as a part of my M.S. in Data Science for Public Policy curriculum at the McCourt School of Public Policy at Georgetown University. Thank you to Professor Taylor Corbett for teaching me the fundamentals of Data Viz and Rshiny, and Roman Ruiz and Pierina Luperdi Hernandez of AIR for data quality review and the theoretical framework for this web app."),
                           br(), br()),
                 ),
                 
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
                             "Not Ranked"), 
                             
                             selected = c("Most Competitive",
                                          "Highly Competitive",
                                          "Very Competitive",
                                          "Competitive",
                                          "Less Competitive",
                                          "Noncompetitive",
                                          "Special",
                                          "Not Ranked")),
                         
                         checkboxGroupInput("pickLevel", h3("Control"), 
                            choices = list(
                             "Public",
                             "Private not-for-profit",
                             "Private for-profit"), 
                             
                             selected = c("Public",
                                          "Private for-profit",
                                          "Private not-for-profit")),
                         
                         checkboxGroupInput("pickControl", h3("Level"), 
                                            choices = list(
                                                "4-year or above",
                                                "2-year",
                                                "Less-than 2-year"), 
                                            
                                            selected = c("4-year or above",
                                                         "2-year",
                                                         "Less-than 2-year")),
                         sliderTextInput("sensitivity1",h3("Scale Sensitivity"), 
                                         choices = c("Low", "High"), 
                                         selected = c("Low")),
                         helpText('By setting scale sensitivity to high, the scale does not become influenced by high in-state percentages. As such, more states will "light up."')
                         
                         ),
                 
                     mainPanel(plotlyOutput("Smap", height = 800, width = 1000),
                     mainPanel(dataTableOutput('state_tab'))
                     ))),
        
        tabPanel("Institution Level", fluid = TRUE,
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput("selectState", h3("State"), options = list(maxOptions = 10000),
                                        choices = c(NULL)),
                         selectizeInput("select", h3("Institution"), options = list(maxOptions = 10000),
                         choices = c(NULL)),
                         sliderTextInput("sensitivity",h3("Scale Sensitivity"), 
                                                   choices = c("Low", "High"), 
                                                   selected = c("Low")),
                         helpText('By setting scale sensitivity to high, the scale does not become influenced by high in-state percentages. As such, more states will "light up."')
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
                    mainPanel(withMathJax(),
                           '$$SDI=\\frac{N(N-1)}{\\sum{n(n-1)}}$$'),
                    mainPanel(textOutput("GDI_text2"), br()),
                    mainPanel(plotlyOutput("GDI", height = 500, width = 750)),
                    selectizeInput("pickSect", "Sector", choices = c(NULL)),
                    selectizeInput("pickBar", "Selectivity", choices = c(NULL)),
                    mainPanel(dataTableOutput("GDI_tab"))
                     ),
        tabPanel("About the Author", fluid = TRUE,
                 mainPanel(h2("About the Author")),
                 mainPanel("Adam Hearn is a graduate student at the McCourt School of Public Policy studying Data Science for Public Policy. In addition to his studies, Adam is a Research Associate Intern at the American Institutes for Research and a contract data scientist with College Transitions, LLC.",
                           br(),br()),
                 mainPanel("For contact information, please visit", a(href="https://www.achearn.com", "www.achearn.com.")))
        ),
        useShinyjs(),
        uiOutput("back")
)

server <- function(input, output, session, clHist) {
    
    #adding data and cleaning up messiness
    
    M <- read_csv("Mapping.csv")
    I <- read_csv("click_state_gb_2.csv")
    
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
    
    Q <- read_csv('state_chlor.csv')
    Q <- as.data.frame(Q)
    
    Y <- read_csv('state_chlor2.csv')
    Y <- as.data.frame(Y)
    
    D$state_enr = as.numeric(D$state_enr)
    D$tot_enr = as.numeric(D$tot_enr)
    D$resid_cat = as.factor(D$resid_cat)
    
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
    
    #when you click on a state
    St <- function(r, dat){
        selected = (simpleCap(r))
        s <- dat %>%
            filter(region == r)
        
        #getting geom
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
        
        s <- s %>% filter(!is.na(code)) %>% 
            mutate(region = str_to_title(region))
        
        s$code_orig <- state.abb[match(s$region, state.name)]
        s <- s %>%  select(state, s_enrollment, code)
        z <- s$s_enrollment

        sens = input$sensitivity1
        if (sens == "Low") {maxval = max(z)}
        else {
            scale = z[z != max(z)]
            maxval = max(scale)*2
        }
        s$pct = round(s$s_enrollment / sum(s$s_enrollment), 4)*100
        s$hover = paste0(s$state, '<br>', s$s_enrollment, '<br>', s$pct, '%')
        quantiles = unique(quantile(z, seq(0,1,0.1)))
        p <- plot_ly(s,source = "CCM",
                     z = z,
                     zmin = 0,
                     zmax = maxval,
                     #text = ~ M$hover,
                     type = 'choropleth',
                     locations = ~ s$code,
                     locationmode = "USA-states",
                     colorscale = 'Reds',
                    showscale = F,
                     reversescale = F,
                    hovertemplate =paste(
                        s$hover,
                        "<extra></extra>"
                    )
        ) %>% 
            colorbar(title = "Num students") %>%
            layout(
                title = paste0('Where do students who attend college in ', selected, ' come from?'),
                geo = g,
                yaxis=list(fixedrange=TRUE),
                xaxis=list(fixedrange=TRUE)
            ) 
        p
        
    }
    

    
    # state-level map
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
            St(d,I2) # getting state data
    }
    )

        filter1 <- reactive({
            if (FALSE == FALSE) {
                    barrons = input$pickBarrons1
                    lev = input$pickControl
                    con = input$pickLevel
                    print(barrons)
                    print(unique(I$barrons14))
                    if ((length(barrons) == 8) & length(lev) == 3 & length(con) == 3){
                        I2 <- I %>% group_by(region, state, code) %>% 
                                   summarize(s_enrollment = sum(s_enrollment))

                        return(I2)
                    }
                    else {
                        I2 <- I %>% 
                            mutate(barrons14 = as.factor(barrons14)) %>% 
                            mutate(level = as.factor(level)) %>% 
                            mutate(control = as.factor(control)) %>% 
                                   filter(barrons14 %in% barrons) %>%
                                    filter(level %in% lev) %>%
                                    filter(control %in% con) %>% 
                                   group_by(region, state, code) %>% 
                                   summarize(s_enrollment = sum(s_enrollment))
                        return(I2)
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

    
    output$Imap3 <- renderPlotly({
        filterState()
        imp = (input$select)
        sens = input$sensitivity
        S1 = D %>% filter(instnm == imp)
        index = S1$X1[1]
        curve = NULL
        inst <- S1
        name = S1$instnm[1]
        inst <- inst %>% filter(!is.na(code)) %>% 
            mutate(prop = round((as.numeric(state_enr)/as.numeric(tot_enr))*100,2))
        
        if (is.na(name)){
            name = 'your institution'
            hover = NA
        }
        else {
            inst$plural = ifelse(inst$state_enr == 1, 'student', 'students')
            inst$hover <- paste0(inst$long_state_res, '<br>', inst$state_enr, " ", inst$plural, "<br>", inst$prop, "%")
        }

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
        
        print(inst$prop)
        z = inst$prop
        #changing saale if sensitivity is set to high
        if (sens == 'High') {
            scale = z[z != max(z)]
            maxval = max(scale)*2
        }
        else{maxval = max(z)}
        p <- plot_ly(inst,source = "InstLev",
                     z = z,
                     zmin = 0,
                     zmax = maxval,
                     type = 'choropleth',
                     locations = ~ inst$code,
                     locationmode = "USA-states",
                     colorscale = 'Reds',
                     showscale = F,
                     reversescale = F,
                     hovertemplate =paste(
                         inst$hover,
                         "<extra></extra>"
                     )
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
            updateSelectizeInput(session, 'select' , choices = append('Select institution', sort(D1$instnm)), server = TRUE)
        }
        else {
            updateSelectizeInput(session, 'select', choices = append('Select institution', sort(S$instnm)), server = TRUE)
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
    
    
    
    output$slider_map2 <- renderPlotly({
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
        p <- plot_ly(M,source = "s2",
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
        
        if (is.null(event_data("plotly_click", source = "s2"))) {
            isFlag = TRUE
            return(p)
        }
        else { # user clicked
            s <- event_data("plotly_click", source = "s2")
            print(s)
            ## state clicks - no other way to do it
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
        In2(d)
    }
    )
    filter_Y <- reactive({
        range = input$selectivity_slider2
        if (range[1] == "Not Ranked") {
            return(Y)
        }
        else if (range[1] == "Noncompetitive") {
            Y1 <- Y %>% 
                filter(barrons14 != 'Not Ranked') %>% 
                filter(barrons14 != '')
            return(Y1)
        }
        else if (range[1] == "Less Competitive") {
            Y1 <- Y %>% 
                filter(barrons14 != 'Not Ranked') %>% 
                filter(barrons14 != 'Noncompetitive')
            return(Y1)
        }
        else if (range[1] == "Competitive") {
            Y1 <- Y %>% 
                filter(barrons14 != 'Not Ranked') %>% 
                filter(barrons14 != 'Noncompetitive') %>% 
                filter(barrons14 != 'Less Competitive')
            return(Y1)
        }
        else if (range[1] == "Very Competitive") {
            Y1 <- Y %>% 
                filter(barrons14 != 'Not Ranked') %>% 
                filter(barrons14 != 'Noncompetitive') %>% 
                filter(barrons14 != 'Less Competitive') %>% 
                filter(barrons14 != 'Competitive')
            return(Y1)
        }
        else if (range[1] == "Highly Competitive") {
            Y1 <- Y %>% 
                filter(barrons14 != 'Not Ranked') %>% 
                filter(barrons14 != 'Noncompetitive') %>% 
                filter(barrons14 != 'Less Competitive') %>% 
                filter(barrons14 != 'Competitive') %>% 
                filter(barrons14 != 'Very Competitive') 
            return(Y1)
        }
        else if (range[1] == "Most Competitive/Special Focus") {
            Y1 <- Y %>% 
                filter(barrons14 == "Most Competitive/Special Focus")
            return(Y1)
        }
        Y
    })
    

In2 <- function(d) {
    Y1 <- filter_Y()
    d = (simpleCap(d))
    Y1 <- Y1 %>%
        filter(long_state_inst == d) %>% 
        mutate(state = long_state_res)
    
    
    Y2 <- Y1 %>% group_by(state) %>% summarize(x = sum(enr)) 
    Y3 <- Y2 %>% summarize(y = sum(x)) 
    
    Y2$y <- Y3$y[1]
    
    Y2 <- Y2 %>% mutate(pct = x / y) %>% 
        select(state, pct)
    
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
    
    Y2$code <- state.abb[match(Y2$state, state.name)]
    Y2 <- Y2 %>% filter(!is.na(code))
    z <- Y2$pct

    p <- plot_ly(Y2,source = "s2",
                 z = z,
                 zmin = .01,
                 zmax = .25,
                 #text = ~ M$hover,
                 type = 'choropleth',
                 locations = ~ Y2$code,
                 locationmode = "USA-states",
                 colorscale = 'Reds',
                 showscale = F,
                 reversescale = F
    ) %>% 
        colorbar(title = "Num students") %>%
        layout(
            title = paste0('Where do students who attend college in ', d, ' come from?'),
            geo = g
        )
    p
    
}   
    
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

#filtering data depending on slider
filter_Q <- reactive({
    range = input$selectivity_slider
    print(range[1])
    if (range[1] == "Not Ranked") {
        return(Q)
    }
    else if (range[1] == "Noncompetitive") {
        Q1 <- Q %>% 
            filter(barrons14 != 'Not Ranked') %>% 
            filter(barrons14 != '')
        return(Q1)
    }
    else if (range[1] == "Less Competitive") {
        Q1 <- Q %>% 
            filter(barrons14 != 'Not Ranked') %>% 
            filter(barrons14 != 'Noncompetitive')
        return(Q1)
    }
    else if (range[1] == "Competitive") {
        Q1 <- Q %>% 
            filter(barrons14 != 'Not Ranked') %>% 
            filter(barrons14 != 'Noncompetitive') %>% 
            filter(barrons14 != 'Less Competitive')
        return(Q1)
    }
    else if (range[1] == "Very Competitive") {
        Q1 <- Q %>% 
            filter(barrons14 != 'Not Ranked') %>% 
            filter(barrons14 != 'Noncompetitive') %>% 
            filter(barrons14 != 'Less Competitive') %>% 
            filter(barrons14 != 'Competitive')
        return(Q1)
    }
    else if (range[1] == "Highly Competitive") {
        Q1 <- Q %>% 
            filter(barrons14 != 'Not Ranked') %>% 
            filter(barrons14 != 'Noncompetitive') %>% 
            filter(barrons14 != 'Less Competitive') %>% 
            filter(barrons14 != 'Competitive') %>% 
            filter(barrons14 != 'Very Competitive') 
        return(Q1)
    }
    else if (range[1] == "Most Competitive/Special Focus") {
        Q1 <- Q %>% 
            filter(barrons14 == "Most Competitive/Special Focus")
        return(Q1)
    }
    
    Q
})


output$slider_map <- renderPlotly({

    Q1 = as_tibble(filter_Q())

        Q2 <- Q1 %>% group_by(state) %>% summarize(x = sum(s_enr)) 
    Q3 <- Q1 %>% group_by(state) %>% summarize(y = sum(tot_s_enr)) 
    Q4 <- merge(Q2, Q3, by = 'state')
    
    Q4 <- Q4 %>% mutate(pct = x / y) %>% 
        select(state, pct)
    
    Q4$code <- state.abb[match(Q4$state, state.name)]
    print(input$selectivity_slider)
    filt = input$selectivity_slider
    if (filt[1] == "Not Ranked"){
        tit = 'Heatmap of percentage of students who remain in-state<br>All institutions'
    }
    if (filt[1] == "Noncompetitive"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "noncompetitive" and above'
    }
    if (filt[1] == "Less Competitive"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "less competitive" and above'
    }
    if (filt[1] == "Competitive"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "competitive" and above'
    }
    if (filt[1] == "Very Competitive"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "very competitive" and above'
    }
    if (filt[1] == "Highly Competitive"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "highly competitive" and above'
    }
    if (filt[1] == "Most Competitive/Special Focus"){
        tit = 'Heatmap of percentage of students who remain in-state<br>Institutions ranked "most competitive" or "special focus"'
    }
    Q4$hover = paste0(Q4$state, '<br>', round(Q4$pct, 4) * 100, '%')
    print(head(Q4))
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
                 z = (Q4$pct),
                 zmin = 0,
                 zmax = 1,
                 text = Q4$hover,
                 type = 'choropleth',
                 locations = ~ Q4$code,
                 locationmode = "USA-states",
                 colorscale = "Reds",
                 reversescale = F,
                 showscale = F,
                 hovertemplate =paste(
                     Q4$hover,
                     "<extra></extra>"
                 )
                 
    ) %>% 
        colorbar(title = "Pct. Students In-state") %>%
        
        layout(
            title = tit,
            geo = g,
            xaxis = list(fixedrange = TRUE), 
            yaxis = list(fixedrange = TRUE)
        ) 
    p
}
)
    

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
            drop_na() %>% 
            mutate(`%` = round(`# Students` / sum(`# Students`), 4)*100)
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
            filter(as.numeric(tot_enr) >  50) %>% # must have more than 50 students in first-year class
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
            
            #ggplotting
            ggplot() +
            #jittering
            geom_quasirandom(aes(barrons14, as.numeric(index), col = Control, text = paste0(instnm, '<br>GDI: ', round(index, 3))), alpha = .5, shape = 20) + 
            xlab("Barron's Classification (2014)") +
            ylab("GDI") +
            theme_minimal() +
            scale_y_continuous(limits=c(0., 1), breaks=c(0, .25, .5, .75, 1)) +
            guides(fill = guide_legend(title = "Control"))+
            labs(title = 'Geographic Diversity Index by Selectivity:\n4-year institutions, 2018')
        
        ggplotly(p=sdi_plot,
                 tooltip = 'text')
}
)

#duplicate of plot above for main page
output$GDI_main <- renderPlotly({
    sdi_plot1 <- D %>% 
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
        labs(title = 'Geographic Diversity Index by Selectivity:<br>4-year institutions, 2018')
    
    ggplotly(p=sdi_plot1,
             tooltip = 'text')
}
)


output$GDI_text1 <- renderText({
    text1 = "Geographic diversity index, derived from Simpson's Diversity Index, is a measure of how diverse an institution's geographic makeup is. This value ranges from 0 to 1, with 1 being the most diverse and 0 being the least diverse. The formula is as follows:"
    return(text1)
}
)

output$GDI_text2 <- renderText({
    text2 = "N represents the number of students in the institution's 2018 first-year class, while n represents the number of students who attend that institution from a particular state. If an institution pulls from all 50 states with an equal distribution from each state, the institution will have a diversity index very close to 1. On the contrary, if an institution's student body is composed of students from only 1 state, this institution will have a very low diversity index."
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

#columbia geographic profile
output$columbia <- renderPlotly({
    imp = ("Columbia University in the City of New York")
    S1 = D %>% filter(instnm == imp)
    index = S1$X1[1]
    curve = NULL
    inst <- S1
    name = S1$instnm[1]
    inst <- inst %>% filter(!is.na(code)) %>% 
        mutate(prop = round((as.numeric(state_enr)/as.numeric(tot_enr))*100,2))
    if (is.na(name)){
        name = 'your institution'
        hover = NA
    }
    #hover text
    else {
        inst$plural = ifelse(inst$state_enr == 1, 'student', 'students')
        inst$hover <- paste0(inst$long_state_res, '<br>', inst$state_enr, " ", inst$plural, "<br>", inst$prop, "%")
    }
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
    
    z = inst$prop # scale
    p <- plot_ly(inst,source = "InstLev",
                 z = z,
                 zmin = 0,
                 max = 5,
                 type = 'choropleth',
                 locations = ~ inst$code,
                 locationmode = "USA-states",
                 colorscale = 'Reds',
                 showscale = F,
                 reversescale = F,
                 hovertemplate =paste(
                     inst$hover,
                     "<extra></extra>"
                 )
    ) %>% 
        colorbar(title = "Num students") %>%
        layout(
            title = paste0('Columbia University Geographic Profile'),
            geo = g
        ) 

    p
}
)

# city college geographic profile
output$cuny <- renderPlotly({
    imp = ("CUNY City College")
    S1 = D %>% filter(instnm == imp)
    index = S1$X1[1]
    curve = NULL
    inst <- S1
    name = S1$instnm[1]
    inst <- inst %>% filter(!is.na(code)) %>% 
        mutate(prop = round((as.numeric(state_enr)/as.numeric(tot_enr))*100,2))
    
    if (is.na(name)){
        name = 'your institution'
        hover = NA
    }
    else {
        inst$plural = ifelse(inst$state_enr == 1, 'student', 'students')
        inst$hover <- paste0(inst$long_state_res, '<br>', inst$state_enr, " ", inst$plural, "<br>", inst$prop, "%")
    }
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
    
    z = inst$prop
    p <- plot_ly(inst,source = "InstLev",
                 z = z,
                 zmin = 0,
                 max = 5,
                 type = 'choropleth',
                 locations = ~ inst$code,
                 locationmode = "USA-states",
                 colorscale = 'Reds',
                 showscale = F,
                 reversescale = F,
                 hovertemplate =paste(
                     inst$hover,
                     "<extra></extra>"
                 )
    ) %>% 
        colorbar(title = "Num students") %>%
        layout(
            title = paste0('CUNY City College Geographic Profile'),
            geo = g
        ) 
    
    #s <- event_data("plotly_click")
    #print(s)
    p
}
)

#texas at austin geographic profile
output$uta <- renderPlotly({
    imp = ("The University of Texas at Austin")
    S1 = D %>% filter(instnm == imp) #filtering data
    index = S1$X1[1]
    curve = NULL
    inst <- S1
    name = S1$instnm[1]
    inst <- inst %>% filter(!is.na(code)) %>% 
        mutate(prop = round((as.numeric(state_enr)/as.numeric(tot_enr))*100,2))
    
    #if field is empty don't render plot
    if (is.na(name)){
        name = 'your institution'
        hover = NA
    }
    else {
        #creating hover text
        inst$plural = ifelse(inst$state_enr == 1, 'student', 'students')
        inst$hover <- paste0(inst$long_state_res, '<br>', inst$state_enr, " ", inst$plural, "<br>", inst$prop, "%")
    }
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
    
    z = inst$prop
    p <- plot_ly(inst,source = "InstLev",
                 z = z,
                 zmin = 0,
                 max = 5,
                 type = 'choropleth',
                 locations = ~ inst$code,
                 locationmode = "USA-states",
                 colorscale = 'Reds',
                 showscale = F,
                 reversescale = F,
                 hovertemplate =paste(
                     inst$hover,
                     "<extra></extra>"
                 )
    ) %>% 
        colorbar(title = "Num students") %>%
        layout(
            title = paste0('UT-Austin Geographic Profile'),
            geo = g
        ) 
    
    #s <- event_data("plotly_click")
    #print(s)
    p
}
)

#waffle plot; data generated from summary statistics
output$sorted_bar <- renderPlot({
    vec = c(`Same State (78.07%)`=1765313,
            `Bordering State (8.49%)`=192063, 
            `Non-Bordering State (10.57%)`=239125,
            `International Countries (2.47%)`=55740,
            `U.S. State Unknown (0.32%)`=7161)
    #`U.S. Minor Outlying Islands`=1777,
    
    colors = c("#7EBDC2","#B9CEB2", "#F3DFA2","#BB4430","#EFE6DD")
    waffle(vec/5000, rows = 18, size = 1, colors = colors,
           title = 'Geographic Breakdown of Fall 2018 Entering Class\nAll Postsecondary Institutions',
           xlab = 'Each square represents 5,000 students') 
})

output$barrons_plot <- renderPlot({
    colors = c('#999999',"#BB4430", "#EFE6DD", "#F3DFA2", "#B9CEB2", "#7EBDC2")
    
    ##ggplotly
    p <- D %>% filter(resid_cat != 'U.S. Total') %>%
        filter(barrons14 !=  "Not Ranked: Sector Unkwn." ) %>% 
        filter(barrons14 !=  "Not Ranked: <2yr" ) %>% 
        filter(barrons14 !=  "Not Ranked: 2yr" ) %>%
        filter(barrons14 !=  "Not Ranked: 4yr" ) %>% 
        filter(barrons14 !=  "Not Ranked: Admin. Unit" ) %>%
        group_by(barrons14, resid_cat) %>% 
        summarise(enrollment = sum(state_enr)) %>% 
        mutate(pct = (enrollment/sum(enrollment))) %>% 
        mutate(lab = paste0(round(pct * 100,0), '%')) %>% 
        mutate(lab = replace(lab, pct < .05, "")) %>% 
        mutate(resid_cat = factor(resid_cat, levels = c('Residence Not Reported', 
                                                        'International Countries', 
                                                        'U.S. State Unknown',
                                                        'Non-Bordering State',
                                                        'Bordering State',
                                                        'Same State')))  %>% 
        mutate(barrons14 = factor(barrons14, levels = c('Special Focus', 'Most Competitive', 'Highly Competitive', 'Very Competitive', 'Competitive', 'Less Competitive', 'Noncompetitive')))  %>% 
        filter(!is.na(barrons14)) %>% 
        ggplot(aes(fill = resid_cat, y = pct, x = barrons14)) +
        geom_bar(stat = 'identity') +
        ylab('') +
        xlab("Barron's Classification (2014)") +
        labs(title = "Proportion of first-time enrollment by\ngeography and institutional selectivity", caption = 'Enrollment includes first-time degree seeking students that graduated HS in past 12 months.\n\nSOURCE: U.S. Department of Education, National Center for Education Statistics,\nIntegrated Postsecondary Education Data System (IPEDS)', fill = 'Residence') + 
        theme_gray() + 
        geom_text(aes(label = lab), size = 4, position = position_stack(vjust = .55)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip() + 
        scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        scale_fill_manual(values = colors) +
        
        theme_classic() + 
        theme(plot.title = element_text(face = "bold", size = 14)) +
        theme(axis.text.y = element_text(color = 'black', size = 12, )) + 
        theme(axis.text.x = element_text(color = 'black', size = 10, ))
    p
})

#same as above but by sector
output$sector_plot <- renderPlot({
    colors = c('#999999',"#BB4430", "#EFE6DD", "#F3DFA2", "#B9CEB2", "#7EBDC2")
    
    p <- D %>% filter(resid_cat != 'U.S. Total') %>%
        filter(sector != 'Administrative Unit') %>% 
        group_by(sector, resid_cat) %>% 
        summarise(enrollment = sum(state_enr)) %>% 
        mutate(pct = (enrollment/sum(enrollment))) %>% 
        mutate(lab = paste0(round(pct * 100,0), '%')) %>% 
        mutate(lab = replace(lab, pct < .05, "")) %>% 
        filter(!is.na(sector)) %>% 
        mutate(resid_cat = factor(resid_cat, levels = c('Residence Not Reported', 
                                                        'International Countries', 
                                                        'U.S. State Unknown',
                                                        'Non-Bordering State',
                                                        'Bordering State',
                                                        'Same State')))  %>% 
        mutate(sector = factor(sector, levels = c('Private not-for-profit, 4-year or above',
                                                  'Private for-profit, 4-year or above',
                                                  'Private not-for-profit, 2-year', 
                                                  'Public, 4-year or above',
                                                  'Private for-profit, 2-year',
                                                  'Private not-for-profit, less-than 2-year',
                                                  'Public, 2-year', 
                                                  'Private for-profit, less-than 2-year'
        )))  %>% 
        ggplot(aes(fill = resid_cat, y = pct, x = sector)) +
        geom_bar(stat = 'identity') +
        ylab('') +
        xlab("Sector") +
        labs(title = "Proportion of first-time enrollment by\ngeography and institutional sector", caption = 'Enrollment includes first-time degree seeking students that graduated HS in past 12 months.\n\nSOURCE: U.S. Department of Education, National Center for Education Statistics,\nIntegrated Postsecondary Education Data System (IPEDS)', fill = 'Residence') + 
        theme_gray() + 
        geom_text(aes(label = lab), size = 4, position = position_stack(vjust = .55)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip() + 
        scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
        scale_x_discrete(expand = c(0,0)) +
        scale_fill_manual(values = colors) +
        
        theme_classic() + 
        theme(plot.title = element_text(face = "bold", size = 14)) +
        theme(axis.text.y = element_text(color = 'black', size = 12, )) + 
        theme(axis.text.x = element_text(color = 'black', size = 10, ))
    p
})

#state tables
output$state_tab <- renderDataTable({
    s = event_data("plotly_click", source = "CCM")
    print(s)
    if (is.null(s)) {
        D1 <- data.frame(" " = NA, "State" = NA, "# Students" = NA, "%" = NA)
        colnames(D1) <- c(" ", 'State', '# Students', "%")
        return(D1)
    }
     else {
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
     }
    print(d)
    I2 <- filter1() %>% 
        filter(region == d)
    
    I3 <- I2 %>% 
        ungroup() %>% 
        select(state, s_enrollment)
    
    I3 <- I3 %>% mutate(`%` = round(s_enrollment / sum(s_enrollment), 4)*100) %>% 
        arrange(-`%`) %>% 
        rename(` # Students` = s_enrollment) %>% 
        rename(State = state) %>% 
        mutate(' ' = simpleCap(d)) %>% 
        relocate(' ')
    return(I3)
}
)

}

shinyApp(ui, server)