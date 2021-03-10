##### Food Security App ###################################
##### Cristina Dobbelaere, March 2021  ####################


# Attach packages #########################################
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  # shiny
  shiny, shinythemes, shinycssloaders,
  # wrangling & plotting
  tidyverse, readxl, lubridate, plotly,
  # spatial
  sf, leaflet, leaflet.extras, geojsonio, rgdal, spatialEco,
  # other
  rcartocolor, glue, here, stringr, htmltools
)

# Style #########################################
bgcolor <- "#262626"
bg2 <- "#222223"

label_style <- list(
  "color"        =  "black",
  "font-family"  =  "default",
  "font-weight"  =  "bold",
  "box-shadow"   =  "3px 3px rgba(0,0,0,0.25)",
  "font-size"    =  "15px",
  "border-color" =  "rgba(0,0,0,0.5)")

# all colors (palette)
colors <- carto_pal(12, "Vivid")[c(3:5, 9)]

# turquoise palette
pal_t <- colorBin(
  c(0, "#24796C"),
  domain = states_sec2$food_insec_prevalence)

# purple palette
pal_p <- colorBin(
  c(0, rcartocolor::carto_pal(12, "Vivid")[9]),
  domain = states_sec2$v_low_food_sec_prevalence)

# Read in data #########################################
food_sec_hh_children <- read_excel("foodsecurity_datafile.xlsx", sheet = 3)
child_food_sec <- read_excel("foodsecurity_datafile.xlsx", sheet = 4)
educ_emp_disab_snap <- read_excel("foodsecurity_datafile.xlsx", sheet = 5)

food_sec_all_hh <- read_excel("foodsecurity_datafile.xlsx", sheet = 2) %>% 
  mutate(year = parse_date_time(Year, "y")) %>% 
  select(-Year) %>% 
  select(year, everything()) %>% 
  rename(
    food_sec_1000          = `Food secure-1,000`,
    food_sec_percent       = `Food secure-Percent`,
    food_insec_1000        = `Food insecure-1,000`,
    food_insec_percent     = `Food insecure-Percent`,
    low_food_sec_1000      = `Low food security-1,000`,
    low_food_sec_percent   = `Low food security-Percent`,
    v_low_food_sec_1000    = `Very low food security-1,000`,
    v_low_food_sec_percent = `Very low food security-Percent`)


# Food sec by state prep #########################################
food_sec_by_state <- read_excel("foodsecurity_datafile.xlsx", sheet = 6) %>% 
  rename(
    num_hh_avg                = `Number of households (average)`,
    num_hh_interviewed        = `Number of households interviewed`,
    food_insec_prevalence     = `Food insecurity prevalence`,
    food_insec_error          = `Food insecurity-Margin of error`,
    v_low_food_sec_prevalence = `Very low food security prevalence`,
    v_low_food_sec_error      = `Very low food security-Margin of error`
  ) %>% 
  select(-Year) %>% 
  mutate(State = as.factor(State)) %>% 
  group_by(State) %>% 
  summarize(
    num_hh_avg                = mean(num_hh_avg),
    num_hh_interviewed        = mean(num_hh_avg),
    food_insec_prevalence     = mean(food_insec_prevalence),
    food_insec_error          = mean(food_insec_error),
    v_low_food_sec_prevalence = mean(v_low_food_sec_prevalence),
    v_low_food_sec_error      = mean(v_low_food_sec_error)
  ) 

states_sp <-  readOGR(
  dsn = here::here("tl_2020_us_state/tl_2020_us_state.shp"), 
  stringsAsFactors = FALSE)

states_sec <- merge(states_sp, food_sec_by_state, by.x = "STUSPS", by.y = "State") 


# FUNCTION TO REMOVE NA's IN sp DataFrame OBJECT
#   x           sp spatial DataFrame object
#   margin      Remove rows (1) or columns (2) 
sp.na.omit <- function(x, margin = 1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame"))
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# Delete rows with NA
states_sec2 <- sp.na.omit(states_sec) 



# Ordered factor levels  ######################################
regions_ordered  <- c("West", "Midwest", "South", "Northeast")
income_ordered   <- c("Under 1.85", "Under 1.30", "Under 1.00",
                      "1.85 and over", "Income unknown")
race_eth_ordered <- c("Black non-Hispanic", "White non-Hispanic", "Hispanic", "Other") 
hh_comp_ordered  <- c("With no children < 18 yrs", "With children < 18 yrs", 
                      "With elderly")

# Food sec by region  #########################################

all_hh_geog_region <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == "Census geographic region") %>% 
  mutate(region = factor(Subcategory, levels = regions_ordered)) %>% 
  select(-Category, -Subcategory, -`Sub-subcategory`) %>% 
  select(year, region, everything()) %>% 
  group_by(year, region) 


# Food sec by HH comp #########################################
all_hh_comp <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == "Household composition") %>% 
  mutate(
    Subcategory       = factor(Subcategory, levels = hh_comp_ordered),
    `Sub-subcategory` = factor(`Sub-subcategory`)
  ) %>% 
  select(-Category) %>% 
  select(year, Subcategory, `Sub-subcategory`, everything()) %>% 
  group_by(year, Subcategory) 

# with subcategs
all_hh_comp_subcateg <- all_hh_comp %>% 
  filter(!is.na(`Sub-subcategory`)) 

# without subcategs
all_hh_comp_avg <- all_hh_comp %>% 
  filter(is.na(`Sub-subcategory`)) %>% 
  select(-`Sub-subcategory`)


# Food sec by metro area #########################################

all_hh_metro <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == "Area of residence") %>% 
  mutate(
    Subcategory = factor(case_when(
      `Sub-subcategory` == "In principal cities"        ~  "Inside metropolitan area & in principal cities",
      `Sub-subcategory` == "Not in principal cities"    ~  "Inside metropolitan area but not in principal cities",
      Subcategory      == 	"Outside metropolitan area" ~  "Outside metropolitan area"))
  ) %>% 
  filter(Subcategory != "Inside metropolitan area!") %>% 
  select(-`Sub-subcategory`) %>% 
  select(year, Category, Subcategory, everything()) %>% 
  group_by(year, Subcategory) 


# Food sec by race/ethnicity #########################################

all_hh_race <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == 	"Race/ethnicity of households") %>% 
  mutate(Subcategory = factor(Subcategory, levels = race_eth_ordered)) %>% 
  select(-`Sub-subcategory`) %>% 
  select(year, Category, Subcategory, everything()) %>% 
  group_by(year, Subcategory) 


# Food sec by income/pov ratio ######################################

all_hh_pov<- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == 	"Household income-to-poverty ratio") %>% 
  mutate(Subcategory = factor(Subcategory, levels = income_ordered)) %>% 
  select(-`Sub-subcategory`) %>% 
  select(year, Category, Subcategory, everything()) %>% 
  group_by(year, Subcategory) 


# UI #########################################
ui <- shiny::navbarPage(
  title = div(
    img(
      src = "foodlogo2.png",
      style =
        "margin-top: -14px;
         padding-right:10px;
         padding-bottom:10px",
      height = 60
      ),
    tags$head(
      tags$link(
        rel = "stylesheet", type = "text/css",
        href = "bootstrap.css"
        )
      )
    ),

  ##### UI: Home Page ################
  
  tabPanel("Home",
           
           tags$div(
             tags$head(
               tags$style(
                 type = "text/css",
                 "home img {max-width: 100%; width: 60%; height: auto}"
                 )
               ),
             withSpinner(
               imageOutput("home", inline = TRUE),
               color = "#52BCA3"
               )
             ),
           
           tags$div(
             mainPanel(
               h4("Welcome!", style = "font-weight: bold;"),
               p("On this site, I provide background information on U.S. 
                  food security and food access, allow users to visualize and
                  understand disparities in food security in the U.S. via 
                  interactive maps and plots, and present solutions that 
                  researchers and activists have proposed to combat these 
                 disparities, inequalities, and injustices."
                 )
               ), 
             style = "margin: 10px 0px 0px 0px; background: #222223"
             ),
           
           tags$footer(
             h6(strong("Inequalities Made Visible: An Interactive Look at Food Access 
                and Food Security in the United States")),
             h6(em("Created by Cristina Dobbelaere for Environmental Studies 149 
                (Food, Agriculture, and the Environment).")),
             style = 
               "float:      center;
                text-align: left
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    32px 0px 0px 0px;
                z-index:    1;"
           )
           
           
  ),  # end Home Panel
  
  
  ##### UI: Background Page ################
  
  tabPanel("Background", 
           
           fluidRow(
             h2("How is this data collected?"),
             p("We are visualizing temperature data collected using biomimetic
              temperature loggers called Robomussels. Robomussels are different from
              other temperature loggers because they thermally match living mussels,
              meaning they are similar in size, color, shape, and thermal inertia
              to living mussels. Using Robomussels rather than traditional
              temperature loggers allows us to more accurately measure the
              temperatures that mussels experience. The Robomussels specifically used in
              this dataset were designed to mimic one of the most abundant mussel
              species found along the northeastern Pacific coast, California mussels",
               em("(Mytilus californianus)."), "Multiple Robomussels are installed at each site.
              They are placed at different heights in the mussel bed to
              measure the wide range of temperatures experienced by mussels
              in the different zones of the rocky intertidal ecosystem."),
             br()
           ),

           style = "float; padding:20px"
           
  ), # end Methods Panel
  
  
  
  
  ##### UI: Maps Page ################
  
  navbarMenu(
    "Comparing Food Security",
    
    tabPanel("Maps & Spatial Comparisons",
             
             sidebarPanel(
               tags$div(
                 selectInput(
                   inputId = "SelectMetric",
                   label = "Select a metric",
                   choices =
                     c("Food insecurity"        = "food_insec_prevalence",
                       "Very low food security" = "v_low_food_sec_prevalence"
                       )
                   ),
                 strong("Food security"), p("is defined as…."),
                 strong("Very low food insecurity"), p("is...")
                 )
               ), # end sidebarPanel
             
             mainPanel(
               tags$div(
                 h1("Mapping U.S. food security trends",
                    style = "font-weight: bold"),
                 p("On this page, we'll look at spatial trends in food security 
                    and food access across the U.S. Select a metric to compare 
                    in the left panel.")),
                withSpinner(
                 leafletOutput(outputId = "map1",
                               width = "100%", 
                               height = "400"),
                 color = "#52BCA3"
                 )
               ) # end mainPanel
             
             ), # end Explore a Site Panel
    
    
    ##### UI: Comparing food access page ################
    tabPanel("Group & Temporal Comparisons",
             
             # Sidebar panel
             sidebarPanel(
               selectInput(
                 inputId = "SelectPlot",
                 label = "What type of plot would you like to see?",
                 choices = c("Food insecurity timeseries", "Food insecurity barplot")
                 ),
               
               tags$div(
                 uiOutput(outputId = "Select2"),
                 uiOutput(outputId = "Select3")
                 )
               ),
             
             # Main panel
             mainPanel(
               tags$div(
                 h1("Comparing food security trends", style = "font-weight: bold"),
                 p("On this page, we'll look at temporal and group-based trends
                    in food security and food access")
                 ),
               withSpinner(
                 plotlyOutput(outputId = "mainplot",
                              width = "100%", height = "600"),
                 color = "#52BCA3"
                 )
               ) # end mainPanel
             )  # end Compare plots
    
    
  ), # end compare/data viz
  
  
  ##### UI: Solutions  ################
  
  tabPanel("Solutions",
           sidebarPanel(
             tags$div("text goes here")
           )
  ),
  
  ##### UI: References  ################

  tabPanel("References",
           sidebarPanel(
             tags$div(
               h4("Want to learn more about food security & food access?", 
                  style = "font-weight: bold"),
               p("Check out the sources that I referenced when creating this site.
                 But keep in mind that these are only a tiny fraction of the food 
                 security papers and resources out there, and there are many more 
                 unheard or marginalized perspectives that are necessary to 
                 inform a thorough, inclusive understanding of current problems 
                 and solutions regarding food security and food access."),
               p("In other words, we're never done learning, so if this topic 
                 interests you, I highly encourage you to seek out additional 
                 resources on your own!")
               )
             ),
           mainPanel(
             tags$div(h1("References", style = "font-weight: bold")),
             tags$div(
               p(a("Burns, C. M., & Inglis, A. D. (2007).",
                   href ="https://doi.org/10.1016/j.healthplace.2007.02.005"), "Measuring food access in Melbourne: Access to healthy and fast foods by car, bus and foot in an urban municipality in Melbourne.", em("Health & Place,"), " 13(4), 877–885."), 
                 # br(),
               p(a("Mui, Y., Ballard, E., Lopatin, E., Thornton, R. L. J., Porter, K. M. P., & Gittelsohn, J. (2019).", href = "https://doi.org/10.1371/journal.pone.0216985"), "A community-based system dynamics approach suggests solutions for improving healthy food access in a low-income urban environment.", em("PLOS ONE,"), "14(5), e0216985."),
               # br(),
               p(a("Prosekov, A. Y., & Ivanova, S. A. (2018).", href =  "https://doi.org/10.1016/j.geoforum.2018.02.030"), "Food security: The challenge of the present.", em("Geoforum,"), "91, 73–77."),
               # br(),
               p(a("Siegner, A., Sowerwine, J., & Acey, C. (2018).", href = "https://doi.org/10.3390/su10092988"), "Does Urban Agriculture Improve Food Security? Examining the Nexus of Food Access and Distribution of Urban Produced Foods in the United States: A Systematic Review.", em("Sustainability,"), "10(9), 2988."),
               # br(),
               p(a("Smith, C., & Morton, L. W. (2009).", href = "https://doi.org/10.1016/j.jneb.2008.06.008"), "Rural Food Deserts: Low-income Perspectives on Food Access in Minnesota and Iowa.", em("Journal of Nutrition Education and Behavior,"), "41(3), 176–187."),
               # br(),
               p(a(em("Strategic Goals."), " (2021).", href = "https://www.soulfirefarm.org/about/goals/"),  "Soul Fire Farm."),
               # br(),
               p(a("U.S. Department of Agriculture. (2020).", href = "https://www.ers.usda.gov/media/10685/foodsecurity_datafile.xlsx"), em("Food Security Data File.")),
               # br(),
               p(a("Walker, R. E., Keane, C. R., & Burke, J. G. (2010).", href = "https://doi.org/10.1016/j.healthplace.2010.04.013"), "Disparities and access to healthy food in the United States: A review of food deserts literature.", em("Health & Place,"), "16(5), 876–884.")
               )
             )
           ) # end refs page
                 
               
  
) # end UI



##### Server ################

server <- function(input, output, session) {
  
  
  ##### Server: Welcome Page ################
  
  output$home <- renderImage({
    list(
      src = "www/home.png",
      contentType = "image/png",
      width = "100%", height = "100%")
    }, deleteFile = FALSE)
      

  ##### Server: Maps Page ################
  
  # chloropleth: food insecurity by state
  
  chloro_base <- leaflet(states_sec2) %>% 
    setView(-96, 37.8, 4) %>% 
    addProviderTiles(
      providers$Stamen.TonerBackground,
      options = providerTileOptions(opacity = 0.2)) %>% 
    setMapWidgetStyle(list(background = "black"))
  
  
  output$map1 <- renderLeaflet({
    
    if (input$SelectMetric == "food_insec_prevalence") {
      
      chloro_base %>% 
        
        addPolygons(
          fillColor = ~pal_t(states_sec2$food_insec_prevalence),
          weight = 2, opacity = 1, color = "white",
          dashArray = "1",
          fillOpacity = 1,
          label = ~STUSPS,
          labelOptions = labelOptions(
            direction = "bottom",
            offset = c(2,2), sticky = T,
            style = label_style),
          popup = paste0(
            "<b>", states_sec2$NAME, ":</b> <br>",
            round(states_sec2$food_insec_prevalence, digits = 2), 
            "% of households food insecure <br>",
            round(states_sec2$v_low_food_sec_prevalence, digits = 2), 
            "% of households with very low food security"
          ),
          popupOptions = popupOptions(
            maxWidth = 300, minWidth = 50, maxHeight = NULL,
            autoPan = T, keepInView = F, closeButton = T
          )
        ) %>%
        
        setView(
          lat = mean(as.numeric(states_sec2$INTPTLAT)) + 10,
          lng = mean(as.numeric(states_sec2$INTPTLON)) - 30, 
          zoom = 2
        ) %>%
        
        addEasyButton(
          easyButton(
            icon    = "fa-globe",
            title   = "Zoom out all the way",
            onClick = JS("function(btn, map){ map.setZoom(1); }"))
        ) %>% 
        
        addLegend(
          "topright", pal = pal_t, values = ~states_sec2$food_insec_prevalence,
          title = "Mean food </br> insecurity </br> prevalence",
          layerId = "legend",
          labFormat = labelFormat(suffix = "%"),
          opacity = 0.8)
      }
    
    else {
      
      chloro_base %>% 
        
        addPolygons(
          fillColor = ~pal_p(states_sec2$v_low_food_sec_prevalence),
          weight = 2, opacity = 1, color = "white",
          dashArray = "1",
          fillOpacity = 1,
          label = ~STUSPS,
          labelOptions = labelOptions(
            direction = "bottom",
            offset = c(2,2), sticky = T,
            style = label_style),
          popup = paste0(
            "<b>", states_sec2$NAME, ":</b> <br>",
            round(states_sec2$food_insec_prevalence, digits = 2), 
            "% of households food insecure <br>",
            round(states_sec2$v_low_food_sec_prevalence, digits = 2), 
            "% of households with very low food security"
          ),
          popupOptions = popupOptions(
            maxWidth = 300, minWidth = 50, maxHeight = NULL,
            autoPan = T, keepInView = F, closeButton = T
          )
        ) %>%
        
        setView(
          lat = mean(as.numeric(states_sec2$INTPTLAT)) + 10,
          lng = mean(as.numeric(states_sec2$INTPTLON)) - 30, 
          zoom = 2
        ) %>%
        
        addEasyButton(
          easyButton(
            icon    = "fa-globe",
            title   = "Zoom out all the way",
            onClick = JS("function(btn, map){ map.setZoom(1); }"))
        ) %>% 
        
        addLegend(
          "topright", pal = pal_p, values = ~states_sec2$v_low_food_sec_prevalence,
          title = "Mean prevalence </br> of very low </br> food security",
          layerId = "legend",
          labFormat = labelFormat(suffix = "%"),
          opacity = 0.8)
      }
    
    })
  
  
  ##### Server: Compare Page ################
  years <-  c(unique(substr(all_hh_comp_subcateg$year, 1, 4)))
  
  observe({
    # Select1
    plot_choice <- input$SelectPlot
    
    if (is.null(plot_choice)) {
      output$mainplot <- character(0)
    }
    
    output$Select2 <- renderUI({
      if (plot_choice == "Food insecurity barplot") {
        selectInput(
          inputId = "SelectTypeBar", 
          label = "I would like to plot food security by...",
          choices = c("Household composition (specific)", "Other"))
      } else if (plot_choice == "Food insecurity timeseries") {
        selectInput(
          inputId = "SelectType",
          label = "I would like to plot food insecurity by...",
          choices = c(
            "Region", 
            "Area of residence",
            "Household race/ethnicity",
            "Household income-to-poverty ratio",
            "Household composition (general)"
            )
          )
      }
    })
    
    # timeseries
    output$Select3 <- renderUI({
      
      if (plot_choice == "Food insecurity timeseries") {
        if (input$SelectType == "Region") {
          checkboxGroupInput(
            inputId = "SelectRegion",
            label = "Select geographic region(s) to compare",
            choices = c("West", "Midwest", "South", "Northeast"))
        } else if (input$SelectType == "Area of residence") {
          checkboxGroupInput(
            inputId = "SelectArea",
            label = "Select residential areas to compare",
            choices = c(levels(all_hh_metro$Subcategory))) 
        } else if (input$SelectType == "Household race/ethnicity") {
          checkboxGroupInput(
            inputId = "SelectRaceEth",
            label = "",
            choices = c(levels(all_hh_race$Subcategory)))   
        } else if (input$SelectType == "Household income-to-poverty ratio") {
          checkboxGroupInput(
            inputId = "SelectIncome",
            label = "",
            choices = c(levels(all_hh_pov$Subcategory)))   
        } else if (input$SelectType == "Household composition (general)") {
          checkboxGroupInput(
            inputId = "SelectHHGroups",
            label = "Select household groups to compare",
            choices = c(levels(all_hh_comp_avg$Subcategory)))
      } else print("")
    }
      
    else if (plot_choice == "Food insecurity barplot") {
      if (input$SelectTypeBar == "Household composition (specific)") {
        selectInput(
          inputId = "SelectYear",
          label = "Year",
          choices = c(unique(all_hh_comp_subcateg$year))
        )
      } else if (input$SelectType == "Other") {
        print("you selected other") 
      } else print("")
    }
      
  
  })
  
    
    
  ### PLOTS ###########################
  output$mainplot <- renderPlotly({
    
    ### BARPLOTS ######################
    if (plot_choice == "Food insecurity barplot") {
      
      # HH COMPOSITION BAR
      if (input$SelectTypeBar == "Household composition (specific)") {
        barplot_hh_comp <- plot_ly() %>% 
          add_trace(
            data = all_hh_comp_subcateg %>%
              filter(year == input$SelectYear),
            x = ~Subcategory, y = ~food_insec_percent,
            color = ~`Sub-subcategory`,
            colors = carto_pal(12, "Vivid")[c(1:6,8:10)],
            type = "bar",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')
            ) %>%
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\ndepending on houshold composition",
            xaxis = list(color = "white", title = "Household composition"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2,
            barmode = "group")
        barplot_hh_comp
      }
    }
      
    
    ### TIMESERIES ####################
    else if (plot_choice == "Food insecurity timeseries") {
      
      # REGION TIMESERIES
      if (input$SelectType == "Region") {
        plot_food_insec_geog <- plot_ly() %>% 
          add_trace(
            data = all_hh_geog_region %>%
              filter(region %in% input$SelectRegion),
            x = ~year, y = ~food_insec_percent,
            color = ~region,
            colors = colors,
            mode = "line",  type = "scatter",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')) %>% 
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\nacross U.S. geographic regions",
            xaxis = list(color = "white", title = "Year"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2)
        plot_food_insec_geog
      
      # AREA OF RESIDENCE TIMESERIES
      } else if (input$SelectType == "Area of residence") {
        plot_metro <- plot_ly() %>% 
          add_trace(
            data = all_hh_metro %>% filter(Subcategory %in% input$SelectArea),
            x = ~year, y = ~food_insec_percent,
            color = ~Subcategory, 
            colors = carto_pal(12, "Vivid")[c(3:5, 9)],
            mode = "line",  type = "scatter",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')) %>% 
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\ndepending on area of residence",
            xaxis = list(color = "white", title = "Year"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2) 
        plot_metro
        
      # HH RACE/ETHNICITY TIMESERIES
      } else if (input$SelectType == "Household race/ethnicity") {
        plot_race_ethnicity <- plot_ly() %>% 
          add_trace(
            data = all_hh_race %>% filter(Subcategory %in% input$SelectRaceEth),
            x = ~year, y = ~food_insec_percent,
            color = ~Subcategory, 
            colors = carto_pal(12, "Vivid")[c(3:5, 9)],
            mode = "line",  type = "scatter",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')) %>% 
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\ndepending on race/ethnicity",
            xaxis = list(color = "white", title = "Year"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2) 
        plot_race_ethnicity
        
      # HH INCOME-TO-POVERTY TIMESERIES
      } else if (input$SelectType == "Household income-to-poverty ratio") {
        plot_income_pov <- plot_ly() %>% 
          add_trace(
            data = all_hh_pov %>%  filter(Subcategory %in% input$SelectIncome),
            x = ~year, y = ~food_insec_percent,
            color = ~Subcategory, 
            colors = carto_pal(12, "Vivid")[c(3:5, 9)],
            mode = "line",  type = "scatter",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')) %>% 
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\ndepending on income-to-poverty ratio",
            xaxis = list(color = "white", title = "Year"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2) 
        plot_income_pov
        
      # HH GROUP TIMESERIES
      } else if (input$SelectType == "Household composition (general)") {
        plot_hh_comp_avg <- plot_ly() %>% 
          add_trace(
            data = all_hh_comp_avg %>% 
              filter(Subcategory %in% input$SelectHHGroups),
            x = ~year, y = ~food_insec_percent,
            color = ~Subcategory, 
            colors = carto_pal(12, "Vivid")[c(3:5, 9)],
            mode = "line",  type = "scatter",
            hoverinfo = "name+y",
            hovertemplate = paste('(%{y}, %{x})')) %>% 
          layout(
            margin = 5,
            font = list(color = "white"),
            title = "Percent of households food insecure\nacross U.S. geographic regions",
            xaxis = list(color = "white", title = "Year"),
            yaxis = list(color = "white", title = "Percent of households food insecure"),
            paper_bgcolor = bg2,
            plot_bgcolor  = bg2) 
        plot_hh_comp_avg
      }
    }
  }) # end plots
    
      
      
      

   
  }) # end observe 
    
    

}  # end server 





##### shinyApp() ###################
shinyApp(ui = ui, server = server)
