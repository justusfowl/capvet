if (!require("pacman")) install.packages("pacman")
pacman::p_load(shinydashboard, leaflet, geojsonio, dplyr, shiny, shinyjs, readr, stringr)

load("models.Rda")

path_prefix <- ""

df_lookup_city <- read_csv(paste0(path_prefix,"data/checked_lookup_va_med_city.csv"))
df_sail_data <- read_csv(paste0(path_prefix,"data/SAIL.csv"))
df_suicide_data <- read_csv(paste0(path_prefix,"data/state_suicide_data_2_agerange.csv"))

df_suicide_data <- rename(df_suicide_data, 'State' = 'State of Death') 

df_states_mapping <- read_csv(paste0(path_prefix,"data/states.csv"))

va_exp_cols <- cols(
  StateLong = col_character(),
  State = col_character(),
  `Veteran Population` = col_double(),
  TotalExpenditures = col_double(),
  CompensationAndPension = col_double(),
  Construction = col_double(),
  EducationAndVocRehab = col_character(),
  LoanGuaranty = col_double(),
  OperatingExp = col_double(),
  InsuranceAndIndemnities = col_double(),
  MedicalAndGOE = col_double(),
  UniquePatient = col_double(),
  Year = col_double()
)

df_va_exp <- read_delim(paste0(path_prefix,"data/va_expenditures_state_2005-2016.csv"), ";", col_types=va_exp_cols)

# clean for individual states only
df_suicide_data_rel  <- df_suicide_data[df_suicide_data$State != 'Total U.S.' & df_suicide_data$State != 'All' ,]

# join state abbreviations
df_suicide_base <- left_join(df_suicide_data_rel, df_states_mapping, by='State')
df_suicide_base <- df_suicide_base %>% dplyr::select(-State) %>% rename(State = "Abbreviation")

#rename columns

df_suicide_base <- rename(df_suicide_base, VetSuicideRatePer100k=`Veteran Suicide Rate per 100,000`)
df_suicide_base <- rename(df_suicide_base, GenPopSuicideRatePer100k=`General Population Rate per 100,000`)

clean_rate <- function(x, col){
  
  val <- x[col]
  
  val <- str_replace_all(val, "[*]", "")
  val <- str_replace_all(val, "-", "")
  
  val <- as.numeric(val)
  
}

# clean suicide rates 
df_suicide_base$VetSuicideRatePer100k <- apply(df_suicide_base, 1, function(x) clean_rate(x, col="VetSuicideRatePer100k"))
df_suicide_base[is.na(df_suicide_base$VetSuicideRatePer100k),]$VetSuicideRatePer100k <- 0

df_suicide_base$GenPopSuicideRatePer100k <- apply(df_suicide_base, 1, function(x) clean_rate(x, col="GenPopSuicideRatePer100k"))
df_suicide_base[is.na(df_suicide_base$GenPopSuicideRatePer100k),]$GenPopSuicideRatePer100k = 0

# focus on state-based rates overall only
df_suicide_base <- df_suicide_base[df_suicide_base$`Age Group` == 'Total', ]

# subset on the ones where suicide rate > 0 
df_suicide_base <- df_suicide_base[df_suicide_base$VetSuicideRatePer100k > 0, ]

df_states <- rename(df_states_mapping, StateLong = State)

df_suicide_base <- dplyr::left_join(df_suicide_base, df_states, by=c("State" = "Abbreviation"))

df_base_overtime <- left_join(df_suicide_base, df_va_exp, by=c("State"="State", "Year"="Year"))


predict_vs_rate <- function(target_model, in_state, in_med, in_vet_pop, in_insurance, in_year){
  
  df_pred <- data.frame(matrix(ncol = 5, nrow = 1))
  x <- c("State", "MedicalAndGOE", "VeteranPopulation", "InsuranceAndIndemnities", "Year")
  colnames(df_pred) <- x
  
  df_pred$State <- factor(in_state,  levels = unique(df_states$Abbreviation))
  df_pred$MedicalAndGOE <- in_med
  df_pred$VeteranPopulation <- in_vet_pop
  df_pred$InsuranceAndIndemnities <- in_insurance
  df_pred$Year <- in_year
  
  if (target_model == "lm"){
    res <- predict(lm_model, df_pred)
    
  }else if (target_model == "svm"){
    res <- predict(svm_model, df_pred)
  }else{
    stop("Unknown model type")
  }
  
  res
  
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  get_data <- reactive({
    year <- input$displayYear
    
    # Requested State -> Long format
    req_state <- input$stateSelect
    
    out <- df_suicide_base[df_suicide_base$Year == year,]
    
    if (req_state != "ALL"){
      out <- out[out$StateLong == req_state,]
    }
    
    out
    
  })
   
  output$stateMap <- renderLeaflet({
    
    # create breaks from histogram
    states <- geojsonio::geojson_read("us_states_geojson.geojson", what = "sp")
    
    data <- get_data()
    
    if (input$stateSelect != 'ALL'){
      states <- states[states$name == input$stateSelect,]
    }
    
    pal <-  colorNumeric("Blues", df_suicide_base$VetSuicideRatePer100k, na.color = "#808080", alpha = TRUE, reverse = FALSE)

    df_states_mapping_json <- rename(df_states_mapping, StateLong = State)
    df_states_mapping_json <- rename(df_states_mapping_json, State = Abbreviation)
    
    # join geojson data lookup values of states
    states@data <- dplyr::left_join(states@data, df_states_mapping_json, by=c("name" = "StateLong"))
    states@data <- dplyr::left_join(states@data, data, by=c("State" = "State"))
    
    out <- leaflet(states) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                  fillColor = ~pal((VetSuicideRatePer100k)),
                  label = ~paste0(name, ": ", formatC(VetSuicideRatePer100k, big.mark = ",")))
    
    if (input$stateSelect == 'ALL'){
      out <- out %>% addLegend("bottomright", pal = pal, values = ~VetSuicideRatePer100k,
                  title = "Vet. Suicide Rates (per 100k)",
                  labFormat = labelFormat(prefix = " "),
                  opacity = 1
        )
    }
    
    out
    
    
  })
  
  output$actualSuicideRate <- renderValueBox({
    
    data <- get_data()
    valueBox(
      paste0(round(mean(data$VetSuicideRatePer100k),2)), "AVG. VET SUICIDES (PER 100k)", icon = icon("user"),
      color = "blue"
    )
  })
  
  output$predictedSuicideRate <- renderValueBox({
    
    target_model <- "lm"
    
    in_state <- df_states[df_states$StateLong == input$scenarioStateSelect,]$Abbreviation
    
    in_med <- input$scenarioMedExp
    in_vet_pop <- input$scenarioVetPop
    in_insurance <- input$scenarioInsurance
    in_year <- 2016
    
    res <- predict_vs_rate(target_model, in_state, in_med = in_med, in_vet_pop = in_vet_pop, in_insurance = in_insurance, in_year = in_year)
    
    valueBox(
      paste0(round(res,2)), "AVG. VET SUICIDES (PER 100k)", icon = icon("user"),
      color = "blue"
    )
    
  })
  
  
  output$details <- renderUI({includeHTML(path = file.path("dev.html"))})
  
  output$frame <- renderUI({
    tags$iframe(src="dev.html")
  })
  
  output$detailsPanel <- renderText({
    
    url <- paste0("dev.html")
    return(paste('<iframe style="height:800px; width:100%" src="', url, '"></iframe>', sep = ""))
  })
  
  
})
