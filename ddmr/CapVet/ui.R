if (!require("pacman")) install.packages("pacman")
pacman::p_load(shinydashboard, plotly, leaflet, shinycssloaders, shinyjs, readr, rlang)

path_prefix <- ""

df_states_mapping <- read_csv(paste0(path_prefix,"data/states.csv"))


state_choices <- c("All" = "ALL")
scenario_state_choices <- c()

for (v in unique(df_states_mapping$State)){
  state_choices <- c(state_choices, v)
  scenario_state_choices <- c(scenario_state_choices, v)
}

state_choices <- state_choices %>% rlang::set_names(state_choices)
scenario_state_choices <- scenario_state_choices %>% rlang::set_names(scenario_state_choices)

dashboardPage(
  dashboardHeader(title = "Capstone | Veteran Suicide Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(width=12, h1("Actuals of Veteran Suicide Rates across the US"), p("The following chart illustrates the veteran suicide rates per different states in the US for a given year.")),
                box( width = 3,
                  selectInput("stateSelect", h4("STATE:"), choices = state_choices)
                ),
                box( width = 6,
                     sliderInput("displayYear", h4("YEAR:"), min=2005, max=2016, value=2016, step=1)
                ),
                
                infoBoxOutput("actualSuicideRate", width=3),
                box(width=12, withSpinner(leafletOutput("stateMap")))
              ),
              fluidRow(
                box(width=12, h1("Planning VA Expenditure in Dependency of Vet. Suicide Rates"), 
                    p("The tool can be used for planning purposes and scenario modelling of VA expenditure and resulting suicide rates simulating effects for year 2016"), 
                    selectInput("scenarioStateSelect", "", choices = scenario_state_choices)),
                box( width = 3,
                     sliderInput("scenarioMedExp", h4("Medical Expenditures:"), min=0, max=7000000, value=400000, step=1000)
                ),
                box( width = 3,
                     sliderInput("scenarioVetPop", h4("Veteran Population:"), min=0, max=3000000, value=2000000, step=1000)
                ),
                box( width = 3,
                     sliderInput("scenarioInsurance", h4("InsuranceAndIndemnities:"), min=0, max=200000, value=10000, step=1000)
                ),
                infoBoxOutput("predictedSuicideRate", width=3)
              )
              
      )
    )
  )
)