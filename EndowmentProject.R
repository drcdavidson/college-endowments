## Install Libraries ##
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

#Load Data Files
EnrollmentFASB <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/EndowmentFASB.csv")
EnrollmentGASB <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/EndowmentGASB.csv")
FTE <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/FTE.csv")
GRAD <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/GraduationRate.csv")
Headcount <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Headcount.csv")
HighDegree <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/HighestDegree.csv")
HighDegreeValue <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/HighestDegreeValues.csv")
INST_Control <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Control.csv")
INST_ControlValue <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_ControlValues.csv")
Retention <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Retention.csv")
Tuition <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Tuition.csv")
INST_Level <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Level.csv")
INST_LevelValues <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_LevelValue.csv")

###################################################
## Clean Data and Combine Files into the Dataset ##
names(INST_Level) <- c("UnitID", "INST", "INST_Level_Code")
names(INST_LevelValues) <- c("Variable", "INST_Level_Code", "INST_Level")

Colleges <- INST_Level %>% left_join(INST_LevelValues, by = 'INST_Level_Code')   #Join INST Levels with Values
Colleges <- select(Colleges, -3,-4)   #Remove unneeded columns
rm(INST_Level, INST_LevelValues)  #Remove merged datasets 

#Add Public & Private Control to Colleges
Colleges <- Colleges %>% mutate(Sector_Code = INST_Control$INST_Control)
rm(INST_Control)
names(INST_ControlValue) <- c("Variable","Sector_Code","Sector") #Rename Sectors
Colleges <- Colleges %>% mutate(Sector_Code = INST_Control_Code) %>%      #Create Sector Codes
  left_join(INST_ControlValue, by = 'Sector_Code')     #Join Sectors
Colleges <- select(Colleges, -4,-5,-6)  #Remove unneeded columns
rm(INST_ControlValue)

#Add Highest Degree
names(HighDegree) <- c("UnitID","INST","HighestDegree_Code")
HighDegreeValue  <- select(HighDegreeValue,-1)      #Remove unneeded column 
names(HighDegreeValue) <- c("HighestDegree_Code","HighestDegree")     #Rename Degrees

Colleges <- Colleges %>% mutate(HighestDegree_Code = HighDegree$HighestDegree_Code) %>%  #create column
  left_join(HighDegreeValue, by = 'HighestDegree_Code')       #join highest degree codes
Colleges <- select(Colleges, -5)      #remove columns
rm(HighDegree, HighDegreeValue)     #remove unneded data

#Add Headcount

