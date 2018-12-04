
url_cancer <- "https://raw.github.ncsu.edu/jdharmes/Project-2/master/Data.txt?token=AAAp0dqIiBCiL4i_7RsMVwSfDR2dxlQ9ks5b4zCgwA%3D%3D"
file <- paste0(getwd(), "/Data.txt")

cancer <- read_tsv(file, n_max = 6329)

cancer1 <- read_tsv(url_cancer, n_max = 6329)

# Rename variables of overall data
cancer <- cancer %>% 
  select(Year, State, Cancer = `Cancer Sites`, 
         MortIncAAR = `Mortality-Incidence Age-Adjusted Rate Ratio`, 
         Deaths = `Death Counts`, Population = `Mortality Population`, 
         MortAAR = `Mortality Age-Adjusted Rate`, Incidence = `Incidence Counts`,
         IncAAR = `Incidence Age-Adjusted Rate`)

# Remove "(Unreliable)" markers from MortIncAAR column and place back into tbl
clean <- str_split_fixed(cancer$MortIncAAR, " ", n = 2)
cancer$MortIncAAR <- as.numeric(clean[,1])
  
cancer15nc <- cancer %>% filter((Year == 2015) & (State == "North Carolina"))

cancernc <- cancer %>% filter(State == "North Carolina")

cancernc$Cancer <- as.factor(cancernc$Cancer)

# Create bar plot of Incidence (for all types in NC)
test <- ggplot(cancernc, aes(x = Year, y = as.numeric(MortIncAAR), color = as.factor(Cancer))) + 
  geom_point() + 
  geom_line() +
  scale_x_discrete(limits = c(2006:2015)) +
  ylab("Mortality-Incidence Rate Ratio") +
  scale_color_discrete(name = "Cancer Site")

ggplotly(test)

  scale_x_discrete(labels = c("Colon", "Uterus", "Breast (F)", "Kidney", "Leukemias",
                              "Liver", "Lung", "Melanoma", "NH Lymphoma",
                              "Pancreas", "Prostate","Thyroid", "Bladder"))

# Create interactive plot that allows zooming and downloading
plot_ly(data = cancernc, x = ~Year, y = ~MortAAR, color = ~Cancer, 
        type = "scatter", mode = "markers+lines")

# ggvis plot (ugly for line plots...)
cancernc %>% 
  ggvis(x = ~Year, y = ~MortAAR, fill = ~Cancer) %>%
  layer_points() %>%
  layer_lines()

  scale_x_discrete(limits = c(2006:2015))
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
bc <- cancer %>% filter((Cancer == "Female Breast"))
bc$Cancer <- as.factor(bc$Cancer)
bc$State <- as.factor(bc$State)
bc$Year <- as.factor(bc$Year)
bc$MortIncAAR <- as.numeric(bc$MortIncAAR)

ggplot(bc, aes(x = Year, y = MortIncAAR, color = State)) + 
  geom_point() +
  geom_line(aes(group = State)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
######

# Create reactive object for bar data
getData1 <- reactive({
  # Filter data as required
  cancer %>% dplyr::filter((Year == 2015) & (State == input$state))
})

# Coerce data into appropriate types for plotting
cancer$Cancer <- as.factor(cancer$Cancer)
cancer$MortIncAAR <- as.numeric(cancer$MortIncAAR)

col_names = c("Notes", "Year", "Year_Code", "State", "State_Code",
              "Cancer_Sites", "Cancer_Sites_Code", "MortIncAAR",
              "Deaths", "Population", "MortAAR", "Incidence",
              "IncAAR"),
skip = 1)

# Store path for cancer data by state from 2006-2015
file_cancer <- "/Users/J/Documents/ST558/Datasets/US Cancer 2006-2015 (By State).txt"

# Read TSV data
cancer <- read_tsv(file_cancer)
cancer <- cancer %>%
  dplyr::select(Year, State, Cancer = `Cancer Sites`, 
                MortIncAAR = `Mortality-Incidence Age-Adjusted Rate Ratio`, 
                Deaths = `Death Counts`, Population = `Mortality Population`, 
                MortAAR = `Mortality Age-Adjusted Rate`, Incidence = `Incidence Counts`,
                IncAAR = `Incidence Age-Adjusted Rate`) 

################################################ Last ditch effort to make work again

tabBox(width = 8, 
       id = "tabset",
       tabPanel(title = "Mortality/Incidence by State", 
                value = "tab1",
                plotlyOutput("bar")),
       tabPanel(title = "Mortality-Incidence AAR",
                value = "tab2",
                plotlyOutput("scatter"))),
box(width = 4, uiOutput("controls"))

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
                     selected = "Mortality"
        ),
        
        selectInput("state",
                    "Select the desired US state/territory:",
                    selected = "North Carolina",
                    choices = as.list(levels(as.factor(cancer$State)))
        ),
        hr(),
        downloadButton("dlplot", "Download Plot"),
        br(),
        br(),
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
                     selected = "MortIncAAR"
        ),
        
        selectInput("state",
                    "Select the desired US state/territory:",
                    selected = "North Carolina",
                    choices = as.list(levels(as.factor(cancer$State)))
        ),
        hr(),
        downloadButton("dlplot", "Download Plot"),
        br(),
        br(),
        downloadButton("dldata2", "Download Data")
    )
    
  }
})

# Create dynamic title
output$title <- renderUI({
  paste0(input$mortinc, " for the Top 13 Cancers for the State of ", input$state)
})


# Store path for cancer data by state from 2006-2015
file_cancer <- "/Users/J/Documents/ST558/Datasets/US Cancer 2006-2015 (By State).txt"

# Read TSV data
cancer <- read_tsv(file_cancer, n_max = 6329)

# Rename variables of overall data
cancer <- cancer %>% select(Year, State, Cancer = `Cancer Sites`, 
                            MortIncAAR = `Mortality-Incidence Age-Adjusted Rate Ratio`, 
                            Deaths = `Death Counts`, Population = `Mortality Population`, 
                            MortAAR = `Mortality Age-Adjusted Rate`, Incidence = `Incidence Counts`,
                            IncAAR = `Incidence Age-Adjusted Rate`)

###################################################

# Download data for scatter plot 2
output$dldata3 <- downloadHandler(
  filename = function() {
    paste("cancerdata", "csv", sep = ".")
  },
  content = function(file) {
    write_csv(getData3(), file)
  }
)

# Download data for bar plot
output$dldata1 <- downloadHandler(
  filename = function() {
    paste("cancerdata", "csv", sep = ".")
  },
  content = function(file) {
    write_csv(getData1(), file)
  }
)

# Download data for scatter plot 1
output$dldata2 <- downloadHandler(
  filename = function() {
    paste("cancerdata", "csv", sep = ".")
  },
  content = function(file) {
    write_csv(getData2(), file)
  }
)

# Download plot image (may clip edges of plot)
output$dlplot <- downloadHandler(
  filename = function() {
    paste("cancerPlotly", "png", sep = ".")
  },
  content = function(file) {
    export(p = last_plot(), file = file, vwidth = 2000, vheight = 2000)
  }
)

########################################### Project 3 ############################################

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

colonCancerNC <- cancer %>% filter((State == "North Carolina") & (Cancer == "Colon and Rectum"))
colonCancer <- cancer %>% filter((Cancer == "Colon and Rectum"))

# Set seed for reproducibility
set.seed(101)

# Split into training and test sets
train <- sample(1:nrow(cancer), nrow(cancer)*0.8)
test <- setdiff(1:nrow(cancer), train)
cancTrain <- cancer[train, ]
cancTest <- cancer[test, ]

# Linear regression model
g <- ggplot(colonCancer, aes(x = Population, y = Deaths)) +
  geom_point(aes(color = State)) +
  geom_smooth(method = "glm", se = FALSE) 
  #scale_x_discrete(limits = c(2006:2015))
ggplotly(g)

lm(formula = Deaths ~ Year + State + Cancer + Population, data = cancer)
linFit <- glm(formula = MortIncAAR ~ State*Cancer*Year, data = cancer)


# Use ANOVA to improve model fit (whether interactions improve fit)
anova(
  lm(MortIncAAR ~ State*Cancer*Population + Year, data = cancer)
)

linPred <- predict(object = linFit, newdata = predDF, type = "response")


# Create new data frame with empty response for predictions
#predDF = cancer %>% 
#  select(1:3, 6, 8) %>% 
#  filter(Year == 2015) %>% 
#  mutate(Year1 = 2016) %>%
#  select(-1) %>%
#  select(Year = Year1, everything())

if (input$interaxn == "Yes") {
  # Allow user to select variables used in model
  thing <- glue(paste("State", "Cancer", "Year", sep = "*"))
  formula <- paste("MortIncAAR~", thing)
  
  # Linear model with interaction effects
  linFit <- lm(formula = MortIncAAR ~ State*Cancer*Year, data = cancTrain)
  
  # Make predictions on test set
  linPred <- predict(object = linFit, newdata = cancTest, type = "response")
  
  # Create data frame with predictions and residuals
  predDF <- cancTest %>% mutate(PredMortIncAAR = linPred, Residuals = PredMortIncAAR - MortIncAAR)
  
} else {
  # Allow user to select variables used in model
  thing <- paste( "State", "Cancer", "Year", sep = "+")
  formula <- paste("MortIncAAR~", thing)
  
  # Linear model without interaction effects
  linFit <- lm(as.formula(formula), data = cancTrain)
  
  # Make predictions on test set
  linPred <- predict(object = linFit, newdata = cancTest, type = "response")
  
  # Create data frame with predictions and residuals
  predDF <- cancTest %>% mutate(PredMortIncAAR = linPred, Residuals = PredMortIncAAR - MortIncAAR)
  
}

cancTrain$State <- as.factor(cancTrain$State)
cancTrain$Cancer <- as.factor(cancTrain$Cancer)
cancTrain$Year <- as.factor(cancTrain$Year)

rfTree <- randomForest(formula = MortIncAAR ~ ., data = cancTrain,
                       mtry = 6, ntree = 200, importance = TRUE)

Tree <- tree(formula = MortIncAAR ~ ., data = cancTrain)                       
summary(Tree)


