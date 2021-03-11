##### Food Security App ###################################
##### Cristina Dobbelaere, March 2021  ####################


# Attach packages #########################################
# if (!require(librarian)){
#   install.packages("librarian")
#   library(librarian)
# }
# shelf(
#   # shiny
#   shiny, shinythemes, shinycssloaders,
#   # wrangling & plotting
#   tidyverse, readxl, lubridate, plotly,
#   # spatial
#   sf, leaflet, leaflet.extras, geojsonio, rgdal, spatialEco,
#   # other
#   rcartocolor, glue, here, stringr, htmltools
# )



library(shiny)
library(shinythemes) 
library(shinycssloaders)
library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(rgdal)
library(spatialEco)
library(rcartocolor)
library(glue)
library(here)
library(stringr)
library(htmltools)


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
  title = tags$div(
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
               h4(strong("Welcome!")),
               p("On this site, I provide background information on U.S. 
                  food security and food access, allow users to visualize and
                  understand disparities in food security in the U.S. via 
                  interactive maps and plots, and present solutions that 
                  researchers and activists have proposed to combat these 
                 disparities, inequalities, and injustices."
                 ),
               br(),
               h4(strong("About this site")),
               p("Hello! My name is Cristina Dobbelaere, and I am a fourth-year environmental studies student at UC Santa Barbara. I created this site with the intention of visually communicating data demonstrating spatial, temporal, and categorical differences in food security in the United States. As someone with a range of interests spanning the sciences, social sciences, and arts, I thought that an interactive website would be an exciting and accessible method to convey complex data and really make visible the disparities in food security that correlate with categories such as geographic region, residential area, household race/ethnicity, state, poverty-to-income ratio, and household composition. I hope that this site will help individuals develop a deeper understanding of the patterns in food security in the U.S., by enabling users to examine how different factors specifically influence food security trends."),
               p("Nonetheless, I recognize that my data are fairly limited in breadth, since I only referenced one dataset to construct the plots and maps that I display in the “Comparing Food Security” section. Given additional time and resources, I would love to visualize data demonstrating individuals’ differential access to local foods, grocery stores, and accessible food prices, for these metrics are certainly more revealing than a “food insecure”/”food secure” division alone and thus bring nuance to a conversation that abounds with nuance and complexity. "),
               br(),
               br(), 
             style = "padding: 20px 100px 40px 20px; background: #222223;"
             ),
           
           tags$footer(
             h6(strong("Inequalities Made Visible: An Interactive Look at Food Access 
                and Food Security in the United States")),
             h6(em("Created by Cristina Dobbelaere for Environmental Studies 149 
                (Food, Agriculture, and the Environment).")),
             br(),
             style = 
               "float:      center;
                text-align: left;
                bottom:     40px;
                width:      100%;
                height:     10px;
                color:      gray;
                padding:    30px 100px 40px 20px;
                z-index:    1;"
           )
           
           
  ),  # end Home Panel
  
  
  ##### UI: Background & Solutions Page ################
  
  tabPanel("Background & Solutions", 
             
           tags$div(
           
             #### BACKGROUND ####
             tags$div(
                  
               h1(strong("Background")),
               p("There’s no question that food insecurity is a complex problem, global in nature and influenced by a range of interacting factors; the 2020 year and the coronavirus pandemic have made this especially clear. That is, COVID-19 has exposed and steeply exacerbated group-based disparities in physical and realized access to adequate, healthy, and culturally appropriate foods."),
               p(strong("But these inequalities are not new. "), "Like COVID-19, the Global Financial Crisis affected the dynamics of poverty and, because poverty is a major cause of hunger and malnutrition, consequently led to greater food insecurity across the globe (Prosekov & Ivanova, 2018, p. 73)."), 
               p("And even in times of presumed financial stability, inequalities in food access and food security persist. In a 2010 review paper, for instance, Walker et al. note that low-income and primarily BIPOC (Black, Indigenous, and people of color) neighborhoods tend to “have an increased exposure to unhealthy advertisements for tobacco and alcohol, fewer pharmacies with fewer medications, and fewer supermarkets which offer a larger variety of affordable and healthy foods compared to smaller convenience stores” (p. 876). "),
               p("What causes these inequalities? And perhaps more importantly, what can we do to diminish them and create a more equitable and just food system, in which everyone has physical and financial access to healthy foods? "),
               
               h3(strong("Food environments, food deserts, and food insecurity")),
               p("We can start to understand the complex dynamics of food security by examining them through the lens of one’s food environment or food landscape. In a 2009 study, Smith and Morton define a food environment as “places to purchase food to take home and/or prepare to eat, places to eat out, and food programs,” as well as roadside stands and gardens (p. 182). When such a landscape maintains barriers to accessing healthy food, it is often called a food desert and results in low food security—or even food insecurity—among residents. Walker et al. (2010) note that although the term food desert is defined, measured, and used differently by different researchers, there remains a general consensus that food deserts are more common in low-income and BIPOC neighborhoods, and that they exist primarily when supermarkets are absent (p. 876). Indeed, in studies such as that conducted by Burns and Inglis (2007) in a suburb of Melbourne, Australia, access to a chain supermarket has been used as a proxy for access to healthy food and access to a fast-food outlet as a proxy for access to unhealthy food. But supermarket access alone doesn’t tell the whole story. Plus, what even determines what an individual’s food environment looks like? What determines whether a person lives closer to a supermarket or a fast-food outlet to begin with?"),
               
               h3(strong("Historical development of food deserts")),
               p("The social and racial history of U.S. cities can be extremely informative of present-day food environment dynamics, in which histories of racial segregation and its effects on consumer purchasing power continue to influence individuals’ food environments and pathways to access food today."),
               p("Specifically, the 1970s and ‘80s saw increased economic segregation and changing demographics of larger U.S. cities, as more affluent people moved from inner cities to suburban areas. As a result, the median income of inner-city residents decreased, causing “nearly one-half of the supermarkets in the three largest U.S. cities to close (Walker et al., 2010, p. 877). Simultaneously, large chain supermarkets expanded and outcompeted independent, neighborhood grocery stores, thereby creating areas where “affordable, varied food is available [only] to those who have access to a car, or those able to pay public transportation accessible costs” (Walker et al., 2010, p. 876). "),
               p("Underlying all of these factors is a history of capitalism, segregation, and redlining, in which large supermarkets established market monopolies, outcompeted local stores that may have provided healthy food at accessible prices, and simultaneously refused to establish new stores in areas that they perceived to be “risky” due to what often came down to inaccurate perceptions of inner cities informed by racist and classist stereotypes (Walker et al., p. 877). "),
               
               h3(strong("Realities of food insecurity")),
               p("The result of these historical factors is a present-day food landscape in which some people have lower food security than others. In Detroit, Michigan, Black residents have access to fewer supermarkets—both in general and nearby (with supermarkets further away on average)—and have lower car ownership, thus rendering faraway supermarkets that do exist physically inaccessible (Walker et al., pp. 878-879). Furthermore, stores in lowest-income neighborhoods have higher food prices with “poorer, often inedible” food quality, along with smaller quantities and varieties of items offered (Walker et al., p. 880). "),
               p("Likewise, rural residents with food insecurity face similar obstacles to aquiring healthy food. In a focus-group study involving low-income residents in food deserts in rural Minnesota and Iowa, Smith and Morton (2009) found that participants suffered from low quality of food in stores, high costs of healthy food, high costs of transportation to access food, and a general lack of variety of food. According to Smith and Morton, although “personal factors [impact] eating behavior for people living in rural food deserts, the physical and social environments [place] the greatest constraints on food access,” even in communities with high civic engagement, because food deserts inherently “restrict food availability and access” (p. 183). As one Minnesotan participant said, “The cost of living is so high, …when we go to the grocery store, we got to buy pretty much what’s on sale, what will last us. I mean, we can’t go buy all the healthy foods we want and all that, ’cause we have to budget our money because we all have bills…” (Smith & Morton, 2009, p. 180). Further, residents’ limited access to affordably priced healthy food prevents adherence to diets made necessary by diabetes, cardiovascular disease, and other diet-related illnesses and diseases, thus compounding existing health consequences (Smith & Morton, 2009, p. 184). "),
               h3(strong("Barriers to food security and food access")),
               p("Food access is limited by a number of interacting factors—both structural and ground-level—that operate at a variety of scales. "),
               h4(strong("Socioeconomic advantage	")),
               p("In a capitalist system with limited food safety net systems, one’s level of socioeconomic advantage is often a strong predictor of food access. In a dynamic GIS analysis, Burns and Inglis (2007) found that areas that were classified as more socioeconomically advantaged were significantly closer to supermarkets, while less socioeconomically advantaged areas were closer to fast-food outlets (p. 882). "),
               h4(strong("Transportation infrastructure, the built environment, and other factors")),
               p("Thus, access to transportation interacts with residential location and income level to play a pronounced role in determining whether residents in lower-income areas can access food from supermarkets—food that tends to be healthier than that found at fast-food joints (Burns & Inglis). In regions where the majority of transport infrastructure is private car–based, like in much of the U.S., car access can significantly increase one’s access to healthy food, such that residents without a car are left disadvantaged, with their monetary and temporal costs of traveling to obtain healthy food significantly higher than those of car owners (Burns & Inglis, p. 883). This strong dependency of food access on car access is particularly a problem for low-income groups, the elderly, the disabled, women with young children, and other groups of people who may not have the privilege to easily take public transit or walk long distances to access a supermarket (Burns & Inglis p. 883). "),
               p("Likewise, rural residents felt that the built environment and their particular spatial positionality in a rural community were the primary factors driving their lack of access to food (Smith & Morton, p. 182). And as one rural Iowan in the Smith and Morton study noted, “If you can’t afford to pay the price of the food that they have in town, then you probably can’t afford to drive to another town to pay cheaper prices, so you’re kind of stuck either way” (Smith & Morton, p. 182). "),
               p("In addition to inadequate and costly transportation infrastructure and streets that are unsafe for walking, Walker et al. (2010) identify time constraints due to work and/or parenting as a barrier to food security, making meal preparation difficult even if healthy food is physically available (p. 878). From an urban lens, Siegner et al. (2018) also draw particular attention to geographic distance to healthy food, the high cost of locally produced (i.e., urban-produced) food, and competing high costs of living and food (p. 8). "),
               h4(strong("Neighborhood-level feedback loops")),
               p("However, both systems-level problems, like inadequate government funding and support, and small-scale, neighborhood-level dynamics influence one’s food landscape and realized food access. In a community-level model-building workshop in low-income urban communities in Central West Baltimore, Maryland, Mui et al. (2019) developed a community-informed framework to describe the ways in which neighborhood-level factors influence food access, uncovering several feedback loops that interact to determine residents’ food environments. For example, they found that local corner storeowners perceive unhealthy food to be in greater demand than healthy food and therefore stock unhealthy food at greater quantities than healthy food. Consequently, consumers may be coerced to purchase more unhealthy food than healthy food, which reinforces storeowners’ perceptions of the high demand for healthy food. Simultaneously, local storeowners perceive healthy food as carrying greater “risk” than unhealthy food given their limited profit margins, preventing them from investing in food that consumers are not guaranteed to purchase. This is only one of the many interacting feedback loops that Mui et al. describe; together, these feedbacks generate a complex landscape that may be oversimplified in research and solutions. "),
               br()
               ),
             
             #### SOLUTIONS #####
             tags$div(
               h1(strong("Solutions")),
               h3(strong("A portfolio of approaches")),
               p("When asked how we can combat food insecurity in a world with a growing population, many people—including Prosekov and Ivanova (2018)—suggest that we simply scale up global food production. But increased global production does not necessarily yield increased physical food access, and increased physical food access does not necessarily yield increased food security or healthy food consumption (Siegner et al., 2018). Indeed, regional and international disparities in food security are also influenced by political systems and unequal food distribution (Prosekov & Ivanova, 2018), and in order to truly develop solutions to disparities in food security that persist today, we need to address the systems at the root of these disparities—systems which frequently relate to histories of colonization and exploitation (Siegner et al., 2018; Smith & Morton, 2009). To achieve this goal requires a portfolio of interacting solutions, since food deserts are created by a range of intersecting factors—not merely the cost of, physical access to, or distance from food, but also the power structures, capitalist dynamics, and histories of exploitation that create, perpetuate, and reinforce such inequalities."),
               p("Below are some approaches to combatting food insecurity that researchers and activists alike have shared."),
               
               h4(strong("Community-informed solutions")),
               p("Mui et al. demonstrate the importance of bringing commonly marginalized voices to the forefront of the discussion of the dynamics of and solutions to food insecurity. Indeed, some of the complexities that they identify would likely be lost without placing community members at the center of their research endeavor, for only those directly immersed in a particular food landscape would recognize its nuances and unintended consequences. "),
               
               h4(strong("Pedestrianization")),
               p("Burns and Inglis suggest increasing zoning diversity to prioritize multiuse zones—hubs that place residential and commercial areas in close proximity to each other to encourage an active, pedestrian-centered lifestyle. In conjunction with improving transport systems, pedestrianization simultaneously reduces the dependency on cars to access supermarkets and healthy food in general (p. 884). "),
               
               h4(strong("Urban/community agriculture and local produce")),
               p("Sourcing local produce in stores (Mui et al.) and pursuing urban agriculture (Seigner et al.) are two additional approaches to combating U.S. food insecurity. Siegner et al. identify that successful, economically viable urban agriculture projects have diversified revenue streams (e.g., from grants, donations, and educational activities), a lack of reliance on produce sales, and civic engagement and coalitions to achieve political recognition and funding. As with other heralded solutions to food insecurity, they note that urban agriculture is “not a panacea” (p. 20) but rather, only one in a portfolio of solutions that must be implemented in concert in order to address and combat food insecurity. "),
               p("One successful action-based, grassroots solution to food insecurity in the United States can be found in Soul Fire Farm, an activism-centered community farm founded by Leah Penniman with the aim of uprooting “racism in the food system” via food sovereignty and the reclamation of stolen land, sustenance, and power. "),
               
               h4(strong("Civic engagement")),
               p("Finally, many researchers agree that community engagement is a powerful tool to reduce food insecurity in a community. Smith and Morton (2009) discuss the importance of civic engagement in enabling increased food access, citing communities with high engagement as having greater access to food resources due to increased physical and social infrastructure; moreover, they note that high civic engagement shaped residents’ public perception and social norms such that using food safety net resources was considered more socially acceptable (p. 183). "),
               p("Walker et al. (2010) cite several examples of local leadership- and policy-based solutions that have been put forth in the U.S., in which partnerships between government and non-government actors have proven successful in reducing disparities in food access. These partnerships likely would not be possible without a civically engaged public to visibly identify the need for them. Similarly, it is important to communicate the benefits of food security projects, such as urban agriculture, to policymakers via civic engagement in order to elevate these endeavors’ prominence in political conversation and attain the funding that is necessary to make them economically viable and beneficial to the food insecure communities they seek to support (Siegner et al.). "),
               p("As such, Mui et al. suggest building wealth via community engagement and organizing and advocating for city policies and funding to establish new stores that stock healthy food options (p. 8). Likewise, Smith and Morton advocate for the revitalization of civic engagement and investment in rural communities through incentives to grocery store owners, as well as the strengthening of infrastructure in the normal food system, in the food safety net system, and in alternative systems and programs, like community gardens and informal transportation networks (p. 185)."),
               p("Ultimately, as Soul Fire Farm and Siegner et al. note, ", strong("working for food sovereignty, ensuring individuals’ physical and financial access to culturally appropriate foods, and uplifting the voices of communities of color "), "should be prioritized in determining approaches to combatting inequalities in U.S. food security."),
               p("Now, let’s take a look at how some of these inequalities appear in food security data. ➤➤➤"),
               br()
               ),
             
             style = "float; padding: 0px 100px 40px 20px"
             
             )
           
           
           
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
                 p("The ", strong("food insecurity chloropleth "), "maps food insecurity prevalence across the 50 U.S. states, where food insecurity is defined as", a("a household-level economic and social condition of limited or uncertain access to adequate food ", href = "https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-us/definitions-of-food-security.aspx#:~:text=Food%20insecurity%E2%80%94the%20condition%20assessed,may%20result%20from%20food%20insecurity"), "and is measured as the percent of households that are food insecure."),
                 p("The ", strong("very low food security chloropleth "), "maps very low food security prevalence across the 50 U.S. states, where households with very low food security", a("have disrupted eating patterns due to insufficient money and other resources for food, ", href = "https://www.ers.usda.gov/topics/food-nutrition-assistance/food-security-in-the-us/definitions-of-food-security.aspx#characteristics"), "thus causing household members to reduce food intake at times during the year. Like food insecurity, it is measured as a percent of total households interviewed.")
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
                 ),
               tags$div(
                 p("Examining both the food insecurity and very low food security maps provides a much deeper understanding of the prevalence of food insecurity among U.S. households than one map alone could provide. Importantly, these maps indicate that inadequate access to affordable, culturally relevant foods is much more common than the threshold-limited category “food insecure” implies. That is, households that are categorized as having “very low food security” still suffer from inadequate access to food."),
                 style = "padding: 10px 10px 0px 0px"
                 ),
               tags$footer(
                 p("These maps were created using data provided by the ", a("USDA’s 2020 Food Security Data File,", href ="https://www.ers.usda.gov/media/10685/foodsecurity_datafile.xlsx"), "which documents the percent of the U.S. population that is both food secure and food insecure according to different categories. Note that this dataset, despite being published in 2020, only contains data from 2001 through 2019 and thus does not shed light on the multitude of complex ways in which COVID-19 has impacted U.S. households’ food security."), 
                 br(),
                 br(),
                 style = 
                   "float:      center;
                    text-align: left;
                    font-size:  small;
                    bottom:     40px;
                    width:      100%;
                    height:     10px;
                    color:      gray;
                    padding:    10px 10px 10px 0px;
                    z-index:    1;"
                 ),
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
                 ),
               tags$footer(
                 p("These plots were created using data provided by the ", a("USDA’s 2020 Food Security Data File,", href ="https://www.ers.usda.gov/media/10685/foodsecurity_datafile.xlsx"), "which documents the percent of the U.S. population that is both food secure and food insecure according to different categories. Note that this dataset, despite being published in 2020, only contains data from 2001 through 2019 and thus does not shed light on the multitude of complex ways in which COVID-19 has impacted U.S. households’ food security."),
                 br(),
                 br(),
                 style = 
                   "float:      center;
                    text-align: left;
                    font-size:  small;
                    bottom:     40px;
                    width:      100%;
                    height:     10px;
                    color:      gray;
                    padding:    10px 10px 10px 0px;
                    z-index:    1;"
               ),
               ) # end mainPanel
             )  # end Compare plots
    
    
  ), # end compare/data viz
  
  
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
