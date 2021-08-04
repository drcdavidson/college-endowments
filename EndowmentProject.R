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
Tuition_In <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/In-State_Tuition.csv")
Tuition_Out <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Out-of-State_Tuition.csv")
INST_Level <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Level.csv")
INST_LevelValues <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_LevelValue.csv")

###################################################
## Clean Data and Combine Files into the Dataset ##
names(INST_Level) <- c("UnitID", "INST", "INST_Level_Code")
names(INST_LevelValues) <- c("Variable", "INST_Level_Code", "INST_Level")

Colleges <- INST_Level %>% left_join(INST_LevelValues, by = 'INST_Level_Code')   #Join INST Levels with Values
Colleges <- select(Colleges, -3,-4)   #Remove unneeded columns
rm(INST_Level, INST_LevelValues)  #Remove merged datasets 

##Add Public & Private Control to Colleges
Colleges <- Colleges %>% mutate(Sector_Code = INST_Control$INST_Control)
rm(INST_Control)
names(INST_ControlValue) <- c("Variable","Sector_Code","Sector") #Rename Sectors
Colleges <- Colleges %>% left_join(INST_ControlValue, by = 'Sector_Code')     #Join Sectors
Colleges <- select(Colleges, -4,-5)  #Remove unneeded columns
rm(INST_ControlValue)

##Add Highest Degree
names(HighDegree) <- c("UnitID","INST","HighestDegree_Code")
HighDegreeValue  <- select(HighDegreeValue,-1)      #Remove unneeded column 
names(HighDegreeValue) <- c("HighestDegree_Code","HighestDegree")     #Rename Degrees

Colleges <- Colleges %>% mutate(HighestDegree_Code = HighDegree$HighestDegree_Code) %>%  #create column
  left_join(HighDegreeValue, by = 'HighestDegree_Code')       #join highest degree codes
Colleges <- select(Colleges, -5)      #remove columns
rm(HighDegree, HighDegreeValue)     #remove unneeded data

##Clean In-State Tuition
names(Tuition_In) <- c("UnitID", "InstName", 2020:2011) #rename columns 
InState <- Tuition_In %>% select(UnitID) %>% mutate(Year = 2020, "In" = Tuition_In$`2020`)   #create subset of rates

#Bind rows into a tidy format 
InState <- bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID,"Year" = 2019,"In" = Tuition_In$`2019`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2018, "In" = Tuition_In$`2018`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2017, "In" = Tuition_In$`2017`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2016, "In" = Tuition_In$`2016`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2015, "In" = Tuition_In$`2015`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2014, "In" = Tuition_In$`2014`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2013, "In" = Tuition_In$`2013`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2012, "In" = Tuition_In$`2012`)) %>%
  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2011, "In" = Tuition_In$`2011`))

#Rename column
names(InState)[3]<-"In-State"

#Remove Data
rm(Tuition_In)

##Clean Out-of-State Tuition
names(Tuition_Out) <- c("UnitID", "InstName", 2020:2011) #rename columns 
OutState <- Tuition_Out %>% select(UnitID) %>% mutate(Year = 2020, "Out" = Tuition_Out$`2020`)   #create subset of rates

#Bind rows into a tidy format 
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID,"Year" = 2019,"Out" = Tuition_Out$`2019`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2018, "Out" = Tuition_Out$`2018`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2017, "Out" = Tuition_Out$`2017`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2016, "Out" = Tuition_Out$`2016`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2015, "Out" = Tuition_Out$`2015`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2014, "Out" = Tuition_Out$`2014`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2013, "Out" = Tuition_Out$`2013`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2012, "Out" = Tuition_Out$`2012`)) %>%
  bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2011, "Out" = Tuition_Out$`2011`))

#Rename column
names(OutState)[3]<-"Out-of-State"

#Remove Data
rm(Tuition_Out)

##Clean Retention
names(Retention) <- c("UnitID", 2019:2010)
Fall_Ret <- Retention %>% select(UnitID) %>% mutate(Year = 2019, Retention = Retention$`2019`)  #create subset of rates

#Bind rows into a tidy format 
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2018,"Retention" = Retention$`2018`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2017,"Retention" = Retention$`2017`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2016,"Retention" = Retention$`2016`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2015,"Retention" = Retention$`2015`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2014,"Retention" = Retention$`2014`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2013,"Retention" = Retention$`2013`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2012,"Retention" = Retention$`2012`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2011,"Retention" = Retention$`2011`)) %>%
  bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2010,"Retention" = Retention$`2010`))

#Remove Data
rm(Retention)
  
## Clean Headount

