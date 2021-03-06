library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggplot2)
#library(webshot)
library(ciTools)
library(tree)
library(randomForest)

# Store path for cancer data by state from 2006-2015
url_cancer <- "https://raw.githubusercontent.com/jdharmes2/Project-2/master/Data.txt"

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

##########################################################################################

ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "US Cancer Statistics"),
  
  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Mortality/Incidence by State", tabName = "bystate"),
      menuItem("Trends by Cancer Site", tabName = "trend"),
      menuItem("Raw Data", tabName = "raw"),
      menuItem("Linear Regression Model", tabName = "linear"),
      menuItem("Bootstrap Aggregation Model", tabName = "bag"),
      menuItem("Principal Components Analysis", tabName = "pca")
    )
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "intro",
        fluidRow(
          box(width = 8,
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
          ),
          box(width = 4,
              withMathJax(),
              h5(strong("Equation"), "for calculating the Mortality-Incidence Rate Ratio 
                (both rates age-adjusted): $$\\frac{Mortality  Rate}{Incidence  Rate}$$")
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
                                 records may vary significantly. The cancer incidence records are 
                                 likely to be coded to a more specific topographic site which may 
                                 give a false impression of an elevated rate ratio. Ratio values 
                                 greater than 1 are flagged as 'Unreliable'.")
                        )
              )),
              column(width = 6,
                     div(
                       tags$ul(
                         tags$li("For consistency with the data on cancer incidence, the cancer 
                                 sites in mortality data were grouped according to the revised SEER 
                                 recodes dated January 27, 2003 (see SEER Cause of Death Recodes). 
                                 Because NCHS uses different groupings for some sites, the death 
                                 rates in this report may differ slightly from those published by 
                                 NCHS. In addition, under the International Classification of 
                                 Diseases (ICD), there are differences in mortality and incidence 
                                 coding. For example, there are several codes for mesothelioma in 
                                 ICD-10 (depending on the primary site). However in ICD-O-3, one 
                                 code captures all of the primary sites that mesothelioma affects. 
                                 Note that Kaposi sarcoma deaths included in this dataset are only 
                                 those deaths with underlying cause of death attributed to Kaposi 
                                 Sarcoma, and do not include deaths where the condition was a 
                                 contributing cause of death, or subsequent to another underlying 
                                 condition."),
                         tags$li("The population used to age-adjust the rates in this report is the
                                 2000 U.S. standard population, which is in accordance with a 1998 
                                 recommendation of the US. Department of Health and Human Services. 
                                 The 2000 U.S. standard population is based on the proportion of 
                                 the 2000 population in specific age groups (younger than 1 year, 
                                 1-4 years, 5-9 years, 10-14 years, 15-19 years, ... 85 years or 
                                 older); the proportions of the 2000 population in these age groups 
                                 serve as weights for calculating age-adjusted incidence and death 
                                 rates. NCHS, however, uses a different set of age groups in its 
                                 age adjustment of death rates, and thus the cancer death rates in 
                                 this report may differ slightly from those published by NCHS."),
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
              #downloadButton("dlscat2", "Download Plot"),
              #br(),
              #br(),
              downloadButton("dldata3", "Download Data"),
              br(),
              br(),
              p("To download plot, open app in browser and use 'Download plot as png' 
                button at top-left.")
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
      ),
      
      tabItem(
        tabName = "linear",
        fluidRow(
          box(width = 8,
              verbatimTextOutput("lmSum")
          ),
          box(width = 4,
              title = "Controls for Linear Regression Model",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "primary",
              radioButtons("response", 
                           "Select response variable to model:",
                           choices = list("Mortality-Incidence Rate Ratio" = "MortIncAAR", 
                                          "Mortality Age-Adjusted Rate" = "MortAAR", 
                                          "Incidence Age-Adjusted Rate" = "IncAAR",
                                          "Mortality" = "Deaths",
                                          "Incidence"),
                           selected = "MortIncAAR"),
              selectizeInput("predictor",
                             label = "Choose variables to predict on:",
                             choices = list("Cancer",
                                            "Population",
                                            "State",
                                            "Year"),
                             selected = "Cancer",
                             multiple = TRUE),
              checkboxInput("interaxn", label = "Model interactions?")
          )
        ),
        fluidRow(
          box(width = 12, 
              h4("Linear Regression Model: Test Data with Predictions & Residuals"),
              hr(),
              dataTableOutput("lm"))
        )
      ),
      
      tabItem(
        tabName = "bag",
        fluidRow(
          box(width = 8,
              verbatimTextOutput("rfSum")
          ),
          box(width = 4,
              title = "Controls for Bootsrap Aggregation Model",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "primary",
              radioButtons("response2", 
                           "Select response variable to model:",
                           choices = list("Mortality-Incidence Rate Ratio" = "MortIncAAR", 
                                          "Mortality Age-Adjusted Rate" = "MortAAR", 
                                          "Incidence Age-Adjusted Rate" = "IncAAR",
                                          "Mortality" = "Deaths",
                                          "Incidence"),
                           selected = "MortIncAAR"),
              numericInput("mtry",
                           "Number of variables used per split:",
                           min = 1,
                           max = 8,
                           value = 3),
              sliderInput("ntree", 
                          "Select number of trees:", 
                          min = 50, 
                          max = 500,
                          value = 200)
          )
        ),
        fluidRow(
          box(width = 12, 
              h4("Bootstrap Aggregation Model"),
              hr(),
              plotOutput("RF"))
        )
      ),
      
      tabItem(
        tabName = "pca",
        fluidRow(
          box(width = 8,
              verbatimTextOutput("pcaSum")
          ),
          box(width = 4,
              title = "Controls for PCA",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              status = "primary",
              selectizeInput("remove", 
                            "Select variables to remove from PCA:",
                            choices = list("None" = "",
                                           "Year",
                                           "Mortality-Incidence Rate Ratio" = "MortIncAAR", 
                                           "Mortality Age-Adjusted Rate" = "MortAAR", 
                                           "Incidence Age-Adjusted Rate" = "IncAAR",
                                           "Mortality" = "Deaths",
                                           "Incidence",
                                           "Population"),
                           selected = "None",
                           multiple = TRUE),
              radioButtons("pca_plot", 
                           "Choose the PCA plot shown below:",
                           choices = list("Biplot", "Screeplot")),
              numericInput("comps", 
                           "Number of components:",
                           min = 1, 
                           max = 7,
                           value = 7,
                           step = 1)
          )
        ),
        fluidRow(
          box(width = 12, 
              h4("Principal Components Analysis Plots"),
              hr(),
              plotOutput("pcaPlot"))
        )
      )
    )
  )
)

############################################################################################

# Define server logic
server <- function(input, output) {
  
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
          #downloadButton("dlbar", "Download Plot"),
          #br(),
          #br(),
          downloadButton("dldata1", "Download Data"),
          br(),
          br(),
          p("To download plot, open app in browser and use 'Download plot as png' button 
            at top-left.")
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
          #actionButton("dlscat1", "Download Plot"),
          #br(),
          #br(),
          downloadButton("dldata2", "Download Data"),
          br(),
          br(),
          p("To download plot, open app in browser and use 'Download plot as png' button 
            at top-left.")      
      )
    }
  })
  
  ############################################# Bar Plot ###########################################
  
  # Create reactive object for scatter data
  getData1 <- reactive({
    cancer %>% filter((Year == 2015) & (State == input$state))
  })
  
  barPlotly <- reactive({ 
    if (is.null(input$state) || is.null(input$mortinc))
      return()
    else {
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
          ggtitle(paste0("2015 Mortality for Top 13 Cancers in ", input$state))
        
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
          ggtitle(paste0("2015 Incidence for Top 13 Cancers in ", input$state))
        
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
          ggtitle(paste0("2015 Mortality and Incidence for Top 13 in ", input$state))
        
        # Run through plotly
        ggplotly(barmortinc)
        
      }
    }
  })
  
  # Create bar plot object for plotly output
  output$bar <- renderPlotly({
    if (is.null(barPlotly())) return()
    barPlotly()
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
  
  ########################################### Scatter 1 #########################################@
  
  # Create reactive object for scatter data
  getData2 <- reactive({
    cancer %>% filter((State == input$state))
  })
  
  # Create scatter plotly object
  scatterPlotly <- reactive({
    if (is.null(input$state) || is.null(input$mortaar))
      return()
    else {
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
    }
    
  })
  
  # Create bar plot object for plotly output
  output$scatter <- renderPlotly({
    if (is.null(scatterPlotly())) return()
    scatterPlotly()
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
  
  ##################################### Scatter 2 ############################################
  
  # Data for second scatter plot
  getData3 <- reactive({
    # Store filtered data
    cancer %>% filter(Cancer == input$cancer)
  })
  
  # Create second scatter plotly object
  scatterPlotly2 <- reactive({
    
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
  
  # Create bar plot object for plotly output
  output$scatter2 <- renderPlotly({
    if (is.null(scatterPlotly2())) return()
    scatterPlotly2()
  })
  
  # Download data for scatter plot 2
  output$dldata3 <- downloadHandler(
    filename = function() {
      paste("cancerdata", "csv", sep = ".")
    },
    content = function(file) {
      write_csv(getData3(), file)
    }
  )
  
  # Set seed for reproducibility
  set.seed(100)
  
  # Sample rows randomly from the full data for an 80/20 (training/test) split
  train <- sample(1:nrow(cancer), size = floor(nrow(cancer)*0.8))
  test <- setdiff(1:nrow(cancer), train)
  cancTrain <- cancer[train, ]
  cancTest <- cancer[test, ]
  
  fit <- reactive({
    if (!input$interaxn) {
      # Allow user to select variables used in model
      string <- paste(input$predictor, collapse = "+")
      resp <- paste(input$response, " ~")
      formula <- paste(resp, string, sep = " ")

      # Linear model without interaction effects
      lm(formula = as.formula(formula), data = cancTrain)
      
    } else {
      # Allow user to select variables used in model
      string <- paste(input$predictor, collapse = "*")
      resp <- paste(input$response, " ~")
      formula <- paste(resp, string, sep = " ")

      # Linear model with interaction effects
      lm(formula = as.formula(formula), data = cancTrain)
    }
  })
  
  output$lm <- renderDataTable({
    # Make predictions on test set
    linPred <- predict(object = fit(), newdata = cancTest, type = "response")
    
    # Create data frame with predictions and residuals
    predDF <- cancTest %>% 
      mutate(Predicted = linPred, Residuals = linPred - get(input$response)) %>%
      as_tibble()
    
    predDF 
  })
  
  output$lmSum <- renderPrint({
    # Make predictions on test set
    linPred <- predict(object = fit(), newdata = cancTest, type = "response")
    
    # Create data frame with predictions and residuals
    predDF <- cancTest %>% 
      mutate(Predicted = linPred, Residual = linPred - get(input$response)) %>%
      as_tibble()
    
    # Return the ANOVA summary of model with test RMSE for evaluation
    list(
    ANOVA = anova(fit()),
    TestRMSE = knitr::kable(summarize(predDF, TestRMSE = sqrt(mean(predDF$Residual^2))))
      
    )
  })
  
  ############################################ Download Buttons ####################################
  
  # Download bar plot image 
  #observeEvent(input$dlbar, {
  #filename = paste("barPlotly", "png", sep = ".")
  #export(barPlotly(), file.path(getwd(), filename))
  #})
  
  # Download plot image (may clip edges of plot)
  #output$dlbar <- downloadHandler(
  #  filename = function() {
  #    paste("barPlotly", "png", sep = ".")
  #  },
  #  content = function(file) {
  #    export(p = barPlotly(), file = file, expand = c(0, 500, 500, 0))
  #  }
  #)
  
  # Download first scatter plot image 
  #observeEvent(input$dlscat1, {
  #  filename = paste("scatterPlotly", "jpeg", sep = ".")
  #  export(p = scatterPlotly(), file = file.path(getwd(), filename))
  #})
  
  # Download second scatter plot image 
  #observeEvent(input$dlscat2, {
  #  filename = paste("scatterPlotly2", "png", sep = ".")
  #  export(p = scatterPlotly2(), file = file.path(getwd(), filename))
  #})
  
  ########################################## Raw Data ############################################
  
  # Create output object with raw data table
  output$rawdata <- renderDataTable(
    cancer
  )
  
  ########################################## Bagged Model ##########################################
  
  bagFit <- reactive({
    cancTrain$State <- as.factor(cancTrain$State)
    cancTrain$Cancer <- as.factor(cancTrain$Cancer)
    cancTrain$Year <- as.factor(cancTrain$Year)
    
    # Allow user to select variables used in model
    formula <- paste(input$response2, " ~ .")

    # Bootstrap aggregation model allowing user input on ntrees
    bagged <- randomForest(formula = as.formula(formula), 
                   data = cancTrain, 
                   ntree = input$ntree,
                   mtry = input$mtry,
                   importance = TRUE)
  
  })
  
  # Create summary of bagged model
  output$rfSum <- renderPrint({
    cancTest$State <- as.factor(cancTest$State)
    cancTest$Cancer <- as.factor(cancTest$Cancer)
    cancTest$Year <- as.factor(cancTest$Year)
    
    bag <- bagFit()
    
    predRF <- predict(object = bag, newdata = select(cancTest, -contains(input$response2)))
    bagPredDF <- mutate(cancTest, 
                        Predicted = predRF, 
                        Residual = predRF - get(input$response2),
                        TestRMSE = sqrt(mean(Residual^2)))
    
    # Named list as output
    list(BaggedModel = bag, 
         ResidualSummary = summary(bagPredDF$Residual), 
         TestRMSE = bagPredDF$TestRMSE[1])
  })
  
  # Create plot showing the model error by ntrees used
  output$RF <- renderPlot({
    bag <- bagFit()
    plot(bag)
  })
  
  
  ############################################### PCA #############################################
  
  # Principal Components Analysis (PCA)
  pcaFit <- reactive({
    cancTrain <- cancTrain %>% select(-State, -Cancer)
    
    if (is.null(input$remove)|| input$remove == "None") {
      
      # PCA on scaled/centered training set (minus input)
      pca <- prcomp(x = cancTrain, 
                    center = TRUE, 
                    scale. = TRUE,
                    rank. = input$comps)
      pca
    } else {
      
      # Create string to hold names of user-selected variables to keep
      remove <- unlist(str_split(paste(input$remove, collapse = ","), ","))

      # PCA on scaled/centered training set (minus input)
      pca <- prcomp(x = select(cancTrain, -one_of(remove)), 
                    center = TRUE, 
                    scale. = TRUE,
                    rank. = input$comps)
      pca
    }
  })
  
  # Create summary of bagged model
  output$pcaSum <- renderPrint({
    list(Summary = summary(pcaFit()), 
         PCA = pcaFit())
  })
  
  # Create plot showing the model error by ntrees used
  output$pcaPlot <- renderPlot({
    pca <- pcaFit()
    
    # Allow user to pick plot visible
    if (input$pca_plot == "Biplot") {
      # Render biplot
      factoextra::fviz_pca_biplot(pca)
    } else {
      # Render screeplot (cumulative variance explained)
      factoextra::fviz_screeplot(pca)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

