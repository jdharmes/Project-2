library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggplot2)


# Store path for cancer data by state from 2006-2015
url_cancer <- "https://raw.github.ncsu.edu/jdharmes/Project-2/master/Data.txt?token=AAAp0b3_OPVyDFnnYaExAUcRWirkqciKks5b_iiSwA%3D%3D"

# Read TSV data
cancer <- read_tsv(file = url_cancer, 
                   n_max = 6329,
                   col_names = c("Notes", "Year", "YearCode", "State", "StateCode", "Cancer", 
                                 "CancerCode", "MortIncAAR", "Deaths", "Population", "MortAAR",
                                 "Incidence", "IncAAR"),
                   col_types = "ciiccccciinin",
                   skip = 1)

# Rename redundant variables of overall data
cancer <- cancer %>% select(2, 4, 6, 8:13) 

# Remove "(Unreliable)" markers from MortIncAAR column and place back into tbl
clean <- str_split_fixed(cancer$MortIncAAR, " ", n = 2)
cancer$MortIncAAR <- as.numeric(clean[,1])


ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "US Cancer Statistics"),
  
  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Mortality/Incidence by State", tabName = "bystate"),
      menuItem("Trends by Cancer Site", tabName = "trend"),
      menuItem("Raw Data", tabName = "raw")
    )
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(
          box(
            width = 7,
            h4(strong("Introduction to the Application")),
            
            div(p("This application was designed to provide the user an easy, yet insightful, 
                  way to analyse cancer mortality and incidence trends over the past 10 years 
                  in the US. This data was obtained by request at the CDC WONDER webpage, 
                  requesting the mortality and incidence data by state and year from 2006-2015 
                  for only the top 13 cancer sites (representing over 80% of cancer cases in 
                  the US)."), 
                "You can find the ",
                a("webpage here.", href = "https://wonder.cdc.gov/cancer.html", target = "_blank")
            )
            )
          ),
        fluidRow(
          box(width = 12,
              column(width = 6,
                     h4(strong("About the Data (per CDC)")),
                     div(
                       "The",
                       a("United States Cancer Statistics", 
                         href = "http://www.cdc.gov/cancer/npcr/uscs/index.htm"),
                       "(USCS) are the official federal statistics on cancer incidence from 
                       registries having high-quality data and cancer mortality statistics for 50 
                       states, the District of Columbia, and Puerto Rico. USCS are produced by the 
                       Centers for Disease Control and Prevention (CDC) and the National Cancer 
                       Institute (NCI), in collaboration with the North American Association of 
                       Central Cancer Registries (NAACCR). Incidence data are provided by:",
                       tags$ul(
                         tags$li("The Centers for Disease Control and Prevention National Program 
                                 of Cancer Registries (NPCR); incidence data for years 1999-2015 are 
                                 provided through November 30, 2017 for NPCR."),
                         tags$li("The National Cancer Institute Surveillance, Epidemiology and End 
                                 Results (SEER) program; incidence data for years 1999-2015 are 
                                 provided through November 1, 2017 for SEER.")
                         ),
                       "Mortality data are provided by the Centers for Disease Control and 
                       Prevention (CDC), National Center for Health Statistics (NCHS), National 
                       Vital Statistics System (NVSS).",
                       p("About mortality incidence rate ratios:"),
                       tags$ul(
                         tags$li("Caution should be used when interpreting these Mortality Incidence
                                 Rate Ratios as coding for death certificates and cancer incidence 
                                 records may vary significantly. The cancer incidence records are likely
                                 to be coded to a more specific topographic site which may give a false
                                 impression of an elevated rate ratio. Ratio values greater than 1 are 
                                 flagged as 'Unreliable'.")
                         )
                         )),
              column(width = 6,
                     div(
                       tags$ul(
                         tags$li("For consistency with the data on cancer incidence, the cancer 
                                 sites in mortality data were grouped according to the revised SEER 
                                 recodes dated January 27, 2003 (see SEER Cause of Death Recodes). 
                                 Because NCHS uses different groupings for some sites, the death rates 
                                 in this report may differ slightly from those published by NCHS. In 
                                 addition, under the International Classification of Diseases (ICD), 
                                 there are differences in mortality and incidence coding. For example, 
                                 there are several codes for mesothelioma in ICD-10 (depending on the 
                                 primary site). However in ICD-O-3, one code captures all of the primary
                                 sites that mesothelioma affects. Note that Kaposi sarcoma deaths 
                                 included in this dataset are only those deaths with underlying cause 
                                 of death attributed to Kaposi Sarcoma, and do not include deaths where 
                                 the condition was a contributing cause of death, or subsequent to 
                                 another underlying condition."),
                         tags$li("The population used to age-adjust the rates in this report is the
                                 2000 U.S. standard population, which is in accordance with a 1998 
                                 recommendation of the US. Department of Health and Human Services. The 
                                 2000 U.S. standard population is based on the proportion of the 2000 
                                 population in specific age groups (younger than 1 year, 1-4 years, 
                                 5-9 years, 10-14 years, 15-19 years, . . . 85 years or older); the 
                                 proportions of the 2000 population in these age groups serve as weights
                                 for calculating age-adjusted incidence and death rates. NCHS, however,
                                 uses a different set of age groups in its age adjustment of death 
                                 rates, and thus the cancer death rates in this report may differ 
                                 slightly from those published by NCHS."),
                         tags$li("Deaths of persons of unknown age are not included in this data 
                                 set. Death counts and rates may differ slightly from other reports 
                                 where deaths of persons of unknown age are included.")
                         )
                      )
                     
                    )
              
              )
          )
      ),
      
      tabItem(
        tabName = "bystate", 
        fluidRow(tabName = "bystate",
                 tabBox(width = 8, 
                        id = "tabset",
                        tabPanel(title = "Mortality/Incidence by State", 
                                 value = "tab1",
                                 plotlyOutput("bar")),
                        tabPanel(title = "Mortality-Incidence AAR",
                                 value = "tab2",
                                 plotlyOutput("scatter"))),
                 box(width = 4, 
                     uiOutput("controls"))
        )
      ),
      
      tabItem(
        tabName = "trend",
        fluidRow(
          box(width = 8,
              plotlyOutput("scatter2")),
          box(width = 4,
              title = "Controls for Scatter Plot",
              solidHeader = TRUE,
              status = "primary",
              selectInput("cancer", 
                          "Select Cancer Site:",
                          choices = as.list(levels(as.factor(cancer$Cancer))),
                          selected = "Female Breast"),
              radioButtons("mortaar2", 
                           "Select Continuous Variable:",
                           choices = list("Mortality-Incidence Rate Ratio" = "MortIncAAR", 
                                          "Mortality Age-Adjusted Rate" = "MortAAR", 
                                          "Incidence Age-Adjusted Rate" = "IncAAR"),
                           selected = "MortIncAAR"),
              hr(),
              #downloadButton("dlplot", "Download Plot"),
              downloadButton("dldata3", "Download Data")
          )
        )
      ),
      tabItem(
        tabName = "raw",
        fluidRow(
          box(width = 12, 
              h4("US Cancer Incidence & Mortality Statistics by State, 2006-2015", 
                 style = "color:navy"),
              h5("From Centers for Disease Control & Prevention",
                 style = "color:navy"),
              hr(),
              dataTableOutput("rawdata")
            )
          )
        )
    )
  )
)


############################################################################################

# Define server logic
server <- function(input, output) {
  
  # Store path for cancer data by state from 2006-2015
  url_cancer <- "https://raw.github.ncsu.edu/jdharmes/Project-2/master/Data.txt?token=AAAp0b3_OPVyDFnnYaExAUcRWirkqciKks5b_iiSwA%3D%3D"
    
  # Read TSV data
  cancer <- read_tsv(file = url_cancer, 
                     n_max = 6329,
                     col_names = c("Notes", "Year", "YearCode", "State", "StateCode", "Cancer", 
                                   "CancerCode", "MortIncAAR", "Deaths", "Population", "MortAAR",
                                   "Incidence", "IncAAR"),
                     col_types = "ciiccccciinin",
                     skip = 1)
  
  # Rename redundant variables of overall data
  cancer <- cancer %>% select(2, 4, 6, 8:13) 
  
  # Remove "(Unreliable)" markers from MortIncAAR column and place back into tbl
  clean <- str_split_fixed(cancer$MortIncAAR, " ", n = 2)
  cancer$MortIncAAR <- as.numeric(clean[,1])
  
  # Dynamic UI components based on user activity
  output$controls <- renderUI({
    
    if (input$tabset == "tab1") {
      
      # Control panel options for bar plot
      box(title = "Controls for Bar Plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          radioButtons("mortinc", 
                       "Mortality or Incidence:",
                       choices = list(Mortality = "Deaths", "Incidence", "Both"),
                       selected = "Deaths"),
          selectInput("state",
                      "Select the desired US state:",
                      selected = "North Carolina",
                      choices = as.list(levels(as.factor(cancer$State)))),
          hr(),
          #downloadButton("dlplot", "Download Plot"),
          downloadButton("dldata1", "Download Data")
      )
    } else {
      
      # Control panel options for scatter plot
      box(title = "Controls for Scatter Plot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          radioButtons("mortaar", 
                       "Select Continuous Variable:",
                       choices = list("Mortality-Incidence Rate Ratio" = "MortIncAAR", 
                                      "Mortality Age-Adjusted Rate" = "MortAAR", 
                                      "Incidence Age-Adjusted Rate" = "IncAAR"),
                       selected = "MortIncAAR"),
          selectInput("state",
                      "Select the desired US state:",
                      selected = "North Carolina",
                      choices = as.list(levels(as.factor(cancer$State)))),
          hr(),
          #downloadButton("dlplot", "Download Plot"),
          downloadButton("dldata2", "Download Data")
      )
    }
  })
  
  # Create reactive object for scatter data
  getData1 <- reactive({
    cancer %>% filter((Year == 2015) & (State == input$state))
  })
  
  # Create bar plot object for output
  output$bar <- renderPlotly({
    
    # Filter data as required
    cancer2015 <- getData1()
    
    if (input$mortinc == "Deaths") {
      
      # Create bar plot of mortality (for all 13 types of cancer)
      barmort <- ggplot(cancer2015, aes(x = Cancer, y = Deaths)) + 
        geom_col(fill = "cornflower blue") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(labels = c("Colon", "Uterus", "Breast (F)", "Kidney", "Leukemias",
                                    "Liver", "Lung", "Melanoma", "NH Lymphoma",
                                    "Pancreas", "Prostate","Thyroid", "Bladder")) +
        ylab("Deaths") +
        ggtitle(paste0("Mortality for Top 13 Cancers in ", input$state))
      
      # Run through plotly
      ggplotly(barmort)
      
    } else if (input$mortinc == "Incidence") {
      
      # Create bar plot of Incidence (for all cancer types)
      barinc <- ggplot(cancer2015, aes(x = Cancer, y = Incidence)) + 
        geom_col(fill = "cornflower blue") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(labels = c("Colon", "Uterus", "Breast (F)", "Kidney", "Leukemias",
                                    "Liver", "Lung", "Melanoma", "NH Lymphoma",
                                    "Pancreas", "Prostate","Thyroid", "Bladder")) +
        ylab(input$mortinc) +
        ggtitle(paste0("Incidence for Top 13 Cancers in ", input$state))
      
      # Run through plotly
      ggplotly(barinc)
      
    } else {
      
      # Create bar plot of both incidence and mortality
      barmortinc <- ggplot(cancer2015, aes(x = Cancer, y = Incidence)) + 
        geom_col(fill = "cornflower blue") + 
        geom_col(aes(x = Cancer, y = Deaths), fill = "grey") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_x_discrete(labels = c("Colon", "Uterus", "Breast (F)", "Kidney", "Leukemias",
                                    "Liver", "Lung", "Melanoma", "NH Lymphoma",
                                    "Pancreas", "Prostate","Thyroid", "Bladder")) +
        ylab("Count") + 
        ggtitle(paste0("Mortality and Incidence for Top 13 in ", input$state))
      
      # Run through plotly
      ggplotly(barmortinc)
      
    }
  })
  
  # Download data for bar plot
  output$dldata1 <- downloadHandler(
    filename = function() {
      paste("cancerdata", "csv", sep = ".")
    },
    content = function(file) {
      write_csv(getData1(), file)
    }
  )
  
  # Create reactive object for scatter data
  getData2 <- reactive({
    cancer %>% filter((State == input$state))
  })
  
  # Create scatter plot output
  output$scatter <- renderPlotly({
    # Filter data only by state
    cancer_scat <- getData2()
    
    # Coerce data into appropriate types for plotting
    cancer_scat$Cancer <- as.factor(cancer_scat$Cancer)
    cancer_scat$MortIncAAR <- as.numeric(cancer_scat$MortIncAAR)
    
    # Create scatter plots conditionally based on input
    if (input$mortaar == "MortIncAAR") {
      # Scatter plot
      scatmortinc <- ggplot(cancer_scat, aes(x = Year, y = MortIncAAR, color = Cancer)) + 
        geom_point() + 
        geom_line() +
        scale_x_discrete(limits = c(2006:2015)) +
        ylab("Mortality-Incidence Rate Ratio")
      
      # Run through plotly
      ggplotly(scatmortinc)
      
    } else if (input$mortaar == "MortAAR") {
      # Scatter plot
      scatmort <- ggplot(cancer_scat, aes(x = Year, y = MortAAR, color = Cancer)) + 
        geom_point() + 
        geom_line() +
        scale_x_discrete(limits = c(2006:2015)) +
        ylab("Mortality Age-Adjusted Rate (per 100k)")
      
      # Run through plotly
      ggplotly(scatmort)
      
    } else {
      # Scatter plot
      scatinc <- ggplot(cancer_scat, aes(x = Year, y = IncAAR, color = Cancer)) + 
        geom_point() + 
        geom_line() +
        scale_x_discrete(limits = c(2006:2015)) +
        ylab("Incidence Age-Adjusted Rate (per 100k)")
      
      # Run through plotly
      ggplotly(scatinc)
    }
  })
  
  # Download data for scatter plot 1
  output$dldata2 <- downloadHandler(
    filename = function() {
      paste("cancerdata", "csv", sep = ".")
    },
    content = function(file) {
      write_csv(getData2(), file)
    }
  )
  
  getData3 <- reactive({
    # Store filtered data
    cancer_scat2 <- cancer %>% filter(Cancer == input$cancer)
  })
  
  output$scatter2 <- renderPlotly({
    # Store filtered data
    cancer_scat2 <- getData3()
    
    # Make Year variable factor
    cancer_scat2$Year <- as.factor(cancer_scat2$Year)
    
    if (input$mortaar2 == "MortIncAAR") {
      
      # Plot of ratio of mortality AAR and incidence AAR
      g1 <- ggplot(cancer_scat2, aes(x = Year, y = MortIncAAR, color = State)) + 
        geom_point() +
        geom_line(aes(group = State)) +
        ylab("Mortality-Incidence Rate Ratio")
      
      # Run through plotly
      ggplotly(g1)
      
    } else if (input$mortaar2 == "MortAAR") {
      
      # Plot by mortality AAR
      g2 <- ggplot(cancer_scat2, aes(x = Year, y = MortAAR, color = State)) + 
        geom_point() +
        geom_line(aes(group = State)) +
        ylab("Mortality Age-Adjusted Rate (per 100k)")
      
      # Run through plotly
      ggplotly(g2)
      
    } else {
      
      # Plot by incidence AAR
      g3 <- ggplot(cancer_scat2, aes(x = Year, y = IncAAR, color = State)) + 
        geom_point() +
        geom_line(aes(group = State)) +
        ylab("Incidence Age-Adjusted Rate (per 100k)")
      
      # Run through plotly
      ggplotly(g3)
      
    }
  })
  
  # Download plot image (may clip edges of plot)
  #output$dlplot <- downloadHandler(
  #filename = function() {
  # paste("cancerPlotly", "png", sep = ".")
  #},
  #content = function(file) {
  #  export(p = last_plot(), file = file, vwidth = 2000, vheight = 2000)
  #}
  #)
  
  # Download data for scatter plot 2
  output$dldata3 <- downloadHandler(
    filename = function() {
      paste("cancerdata", "csv", sep = ".")
    },
    content = function(file) {
      write_csv(getData3(), file)
    }
  )
  
  output$rawdata <- renderDataTable(
    cancer
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

