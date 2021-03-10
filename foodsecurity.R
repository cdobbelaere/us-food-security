

# Attach packages #########################################

if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  # wrangling & plotting
  tidyverse, readxl, lubridate, plotly,
  # spatial
  sf, leaflet, leaflet.extras, geojsonio, rgdal, spatialEco,
  # other
  rcartocolor, glue, here, stringr, htmltools
  )

# states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
# names(states)


# Read in data #########################################
food_sec_hh_children <- read_excel("foodsecurity_datafile.xlsx", sheet = 3)
child_food_sec <- read_excel("foodsecurity_datafile.xlsx", sheet = 4)
educ_emp_disab_snap <- read_excel("foodsecurity_datafile.xlsx", sheet = 5)

# Style #########################################
bgcolor <- "#262626"

label_style <- list(
  "color"        =  "black",
  "font-family"  =  "default",
  "font-weight"  =  "bold",
  "box-shadow"   =  "3px 3px rgba(0,0,0,0.25)",
  "font-size"    =  "15px",
  "border-color" =  "rgba(0,0,0,0.5)")

pal <- colorBin(
  c(0, rcartocolor::carto_pal(12, "Vivid")[9]),
  domain = states_sec2$food_insec_prevalence)


# Food sec by state #########################################

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


# chloropleth: food insecurity by state
chloro_base <- leaflet(states_sec2) %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(
    providers$Stamen.TonerBackground,
    options = providerTileOptions(opacity = 0.2)) %>% 
  setMapWidgetStyle(list(background = "black"))


map1 <- chloro_base %>% 
  
  addPolygons(
    fillColor = ~pal(states_sec2$food_insec_prevalence),
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
    "topright", pal = pal, values = ~states_sec2$food_insec_prevalence,
    title = "Mean food </br> insecurity </br> prevalence",
    layerId = "legend",
    labFormat = labelFormat(suffix = "%"),
    opacity = 0.8) 

map1


# Food sec by region timeseries #########################################

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

regions_ordered <- c("West", "Midwest", "South", "Northeast")

all_hh_geog_region <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == "Census geographic region") %>% 
  mutate(region = factor(Subcategory, levels = regions_ordered)) %>% 
  select(-Category, -Subcategory, -`Sub-subcategory`) %>% 
  select(year, region, everything()) %>% 
  group_by(year, region) 
# %>% 
#   mutate(
#     food_sec_1000          = mean(food_sec_1000),
#     food_sec_percent       = mean(food_sec_percent),
#     food_insec_1000        = mean(food_insec_1000),
#     food_insec_percent     = mean(food_insec_percent),
#     low_food_sec_1000      = mean(low_food_sec_1000),
#     low_food_sec_percent   = mean(low_food_sec_percent),
#     v_low_food_sec_1000    = mean(v_low_food_sec_1000),
#     v_low_food_sec_percent = mean(v_low_food_sec_percent)
#   ) 
  # summarise(across(food_sec_1000:v_low_food_sec_percent, mean = mean))

ggplot(all_hh_geog_region, aes(x = year, y = food_insec_percent, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Year", y = "Mean percent food insecurity") 

ggplot(all_hh_geog_region, aes(x = year, y = food_insec_percent, color = region)) +
  geom_line(stat = "identity") +
  labs(x = "Year", y = "Mean percent food insecurity") 


plot_food_insec_geog <- plot_ly() %>% 
  add_trace(
    data = all_hh_geog_region,
    x = ~year, y = ~food_insec_percent,
    color = ~region, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    mode = "line",  type = "scatter") %>% 
  layout(
    margin = 5,
    font = list(family = "Helvetica", color = "white"),
    title = "Percent of households food insecure\nacross U.S. geographic regions",
    xaxis = list(color = "white", title = "Year"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626") 

plot_food_insec_geog


carto_pal(12, "Vivid")[c(3:5, 9)]




# Household composition
all_hh_comp <- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == "Household composition") %>% 
  mutate(
    Subcategory       = factor(Subcategory),
    `Sub-subcategory` = factor(`Sub-subcategory`)
    ) %>% 
  select(-Category) %>% 
  select(year, Subcategory, `Sub-subcategory`, everything()) %>% 
  group_by(year, Subcategory) 

# with subcategs
all_hh_comp_subcateg <- all_hh_comp %>% 
  filter(!is.na(`Sub-subcategory`)) 

# without sub-subcategories
all_hh_comp_avg <- all_hh_comp %>% 
  filter(is.na(`Sub-subcategory`)) %>% 
  select(-`Sub-subcategory`)

plot_hh_comp_avg <- plot_ly() %>% 
  add_trace(
    data = all_hh_comp_avg,
    x = ~year, y = ~food_insec_percent,
    color = ~Subcategory, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    mode = "line",  type = "scatter") %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\nacross U.S. geographic regions",
    xaxis = list(color = "white", title = "Year"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626") 
plot_hh_comp_avg


# as a barplot
boxplot_hh_comp <- plot_ly() %>% 
  add_trace(
    data = all_hh_comp_subcateg %>% 
      filter(year == "2001-01-01"),
    x = ~Subcategory, y = ~food_insec_percent,
    color = ~`Sub-subcategory`, 
    colors = carto_pal(12, "Vivid")[c(1:6,8:10)],
    type = "bar", 
    hoverinfo = "name+y",
    hovertemplate = paste('(%{y}, %{x}, %{name})')
    ) %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\ndepending on houshold composition",
    xaxis = list(color = "white", title = "Household composition"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626",
    barmode = "stack") 
boxplot_hh_comp





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
  mutate(Subcategory = factor(Subcategory)) %>% 
  select(-`Sub-subcategory`) %>% 
  select(year, Category, Subcategory, everything()) %>% 
  group_by(year, Subcategory) 


# Food sec by income/pov ratio ######################################

income_ordered <- c("Under 1.85", "Under 1.30", "Under 1.00", 
                    "1.85 and over", "Income unknown")

all_hh_pov<- food_sec_all_hh %>% 
  mutate(year = as.Date(year)) %>% 
  filter(Category == 	"Household income-to-poverty ratio") %>% 
  mutate(Subcategory = factor(Subcategory, levels = income_ordered)) %>% 
  select(-`Sub-subcategory`) %>% 
  select(year, Category, Subcategory, everything()) %>% 
  group_by(year, Subcategory) 








# View(all_hh_metro)

# Barplot
barplot_metro <- plot_ly() %>% 
  add_trace(
    data = all_hh_metro %>% 
      filter(year == "2001-01-01"),
    x = ~Subcategory, y = ~food_insec_percent,
    color = ~`Subcategory`, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    type = "bar", 
    hoverinfo = "name+y",
    hovertemplate = paste('(%{y}, %{x})')
  ) %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\ndepending on area of residence",
    xaxis = list(color = "white", title = "Area of residence"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626",
    barmode = "stack") 
barplot_metro


# Timeseries
plot_metro <- plot_ly() %>% 
  add_trace(
    data = all_hh_metro,
    x = ~year, y = ~food_insec_percent,
    color = ~Subcategory, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    mode = "line",  type = "scatter") %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\ndepending on area of residence",
    xaxis = list(color = "white", title = "Year"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626") 
plot_metro


# Timeseries
plot_race_ethnicity <- plot_ly() %>% 
  add_trace(
    data = all_hh_race,
    x = ~year, y = ~food_insec_percent,
    color = ~Subcategory, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    mode = "line",  type = "scatter",
    hoverinfo = "name+y",
    hovertemplate = paste("%{y}")) %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\ndepending on race/ethnicity",
    xaxis = list(color = "white", title = "Year"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626") 
plot_race_ethnicity




# Timeseries

plot_income_pov <- plot_ly() %>% 
  add_trace(
    data = all_hh_pov,
    x = ~year, y = ~food_insec_percent,
    color = ~Subcategory, 
    colors = carto_pal(12, "Vivid")[c(3:5, 9)],
    mode = "line",  type = "scatter",
    hoverinfo = "name+y+x",
    hovertemplate = paste("%{y}", ",", "%{x}")) %>% 
  layout(
    margin = 5,
    font = list(color = "white"),
    title = "Percent of households food insecure\ndepending on income-to-poverty ratio",
    xaxis = list(color = "white", title = "Year"),
    yaxis = list(color = "white", title = "Percent of households food insecure"),
    paper_bgcolor = "#262626",
    plot_bgcolor  = "#262626") 
plot_income_pov


# m <- leaflet(states) %>%
#   setView(-96, 37.8, 4) %>%
#   addProviderTiles("MapBox", options = providerTileOptions(
#     id = "mapbox.light",
#     accessToken = Sys.getenv(
#       'pk.eyJ1IjoiY2RvYmJlbGFlcmUiLCJhIjoiY2tsd3Fzc2c1MWE1cDJwbXMzZm9lMG1idiJ9.L9WC0XVQSyRIIhjk5v3BKw')))
# 
# m

# names(states_sf)



 

# states_sf <- st_read(here::here("tl_2020_us_state/tl_2020_us_state.shp"))
# 
# 
# states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
# 
# states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
# 
# 
# 
# chloro_base <- leaflet(states_sf) %>% 
#   setView(-96, 37.8, 4) %>% 
#   
# chloro_base %>% 
#   addPolygons(
#     fillColor = c("white", "purple"),
#     weight = 2,
#     opacity = 1,
#     color = "white",
#     dashArray = "1",
#     fillOpacity = 0.7
#   )
# 
# chloro_base <- leaflet(states) %>% 
#   setView(-96, 37.8, 4) %>% 
#   addProviderTiles(provider = base)
#   
# chloro_base %>% 
#   addPolygons(
#     fillColor = "purple",
#     weight = 2,
#     opacity = 1,
#     color = "white",
#     dashArray = "1",
#     fillOpacity = 0.7)
#     
#   ) %>% 
#   











View(states_sf)
usa_scope <- list(
  resolution = 200,
  scope = "usa",
  projection = list(type = "albers usa"),
  showlakes = FALSE,
  bgcolor = bgcolor
)
  
states_chloro <- plot_geo(food_sec_by_state, locationmode = "USA-states") %>% 
  add_trace(
    z = ~food_insec_prevalence, text = ~State, locations = ~State,
    color = ~food_insec_prevalence, colors = "Purples"
    ) %>% 
  layout(
    geo = usa_scope,
    margin = 5,
    font = list(family = "Helvetica", color = "white"),
    title = "Percent of households food insecure across U.S.",
    paper_bgcolor = bgcolor,
    legend = list(title = list(text = "Percent of households food insecure"))
    )
 
states_chloro


states_chloro



library(rjson)
url <-  "https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json"
json_file <- RJSONIO::fromJSON(content = url)
json_file$features[[1]]$id













x_hh_geog <- xts(all_hh_geog_region, order.by = year)
  


redmonder.pal(8, "qMSOPu2")

ggplot(all_hh_geog_region, aes(x = Year, y = food_insec_percent)) +
  geom_bar(stat = "identity") +
  labs(x = "Geographic Region", y = "Mean percent food insecurity") 
  





View(all_hh_geog_region)






















food_sec_by_state_timeseries <- food_sec_by_state %>% 
  group_by(State) %>% 
  rename(
    Number_of_hh_avg = `Number of households (average)`,
    Number_of_hh_interviewed = `Number of households interviewed`,
    Food_insecurity_prevalence = `Food insecurity prevalence`,
    Food_insecurity_margin_of_error = `Food insecurity-Margin of error`,
    Very_low_food_sec_prevalence = `Very low food security prevalence`,
    Very_low_food_sec_margin_of_error = `Very low food security-Margin of error`
    ) %>% 
  #mutate(Year = as.Date(Year, format = "%Y")) %>% 
  filter(State=="CA") %>% 
  mutate(Year = parse_date_time(Year, orders = c("%Y-%Y", "%Y-%Y"), exact = TRUE, select_formats = "%Y[1]-%Y[2]"))
                                
                                
                                orders = "YY", select_formats = "Y!-Y!"))
  
  
  



View(food_sec_by_state_timeseries)


# ideally use dygraphs or plotly

ggplot(food_sec_by_state_timeseries, aes(x=Year, y=Food_insecurity_prevalence)) +
  geom_point()
    
           

