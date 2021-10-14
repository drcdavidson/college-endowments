## Install Libraries ##
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(flextable)) install.packages("flextable", repos = "http://cran.us.r-project.org")
if(!require(gtsummary)) install.packages("gtsummary", repos = "http://cran.us.r-project.org")
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")

#Load Data Files
EndowFASB <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/EndowmentFASB.csv")
EndowGASB <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/EndowmentGASB.csv")
Endow2020 <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/2020EndowmentsPrelim.csv")
FTE <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/FTE.csv")
GRAD <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/GraduationRate.csv")
U_Headcount <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Headcount.csv")
HighDegree <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/HighestDegree.csv")
HighDegreeValue <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/HighestDegreeValues.csv")
INST_Control <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Control.csv")
INST_ControlValue <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_ControlValues.csv")
Retention <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Retention.csv")
Tuition_In <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/In-State_Tuition.csv")
Tuition_Out <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Out-of-State_Tuition.csv")
INST_Level <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Level.csv")
INST_LevelValues <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_LevelValue.csv")
StateAbbr <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/StateAbbr.csv")
StateAbbrLabels <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/StateAbbrValues.csv")
INSTsize <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_Size.csv")
INSTsizeLabels<- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/INST_SizeValues.csv")
Variables <- read.csv("https://raw.githubusercontent.com/drcdavidson/college-endowments/main/IPEDS_Data/Variables.csv")

###################################################
## Clean Data and Combine Files into the Dataset ##
names(INST_Level) <- c("UnitID", "INST", "INST_Level_Code")
names(INST_LevelValues) <- c("Variable", "INST_Level_Code", "INST_Level")

Colleges <- INST_Level %>% left_join(INST_LevelValues, by = 'INST_Level_Code')   #Join INST Levels with Values
Colleges <- select(Colleges, -3,-4)   #Remove unneeded columns
rm(INST_Level, INST_LevelValues)  #Remove merged datasets 

##Add Public & Private Control to Colleges
names(INST_ControlValue) <- c("Variable","INST_Control","Sector") #edit column names for join
INST_Control <- INST_Control %>% left_join(INST_ControlValue, by = 'INST_Control') #join tables
rm(INST_ControlValue) #remove data table
Colleges <- Colleges %>% left_join(INST_Control, by = 'UnitID') #add sectors to Colleges table
Colleges <- Colleges[-c(5:6)]  # remove unneeded columns
rm(INST_Control)  #remove table

##Add Highest Degree
names(HighDegreeValue) <- c("Variable","HighestDegree","Highest_Degree")
HighDegree <- HighDegree %>% left_join(HighDegreeValue, by = 'HighestDegree')
rm(HighDegreeValue)
Colleges <- Colleges %>% left_join(HighDegree, by ='UnitID')
Colleges <- Colleges[-c(4,6:8)]
rm(HighDegree)

##Add INSTsize
INSTsize <- INSTsize %>% left_join(INSTsizeLabels, by = 'INST_Size_Value')
rm(INSTsizeLabels)
Colleges <- Colleges %>% left_join(INSTsize, by = 'UnitID')
Colleges <- Colleges[-6]
rm(INSTsize)

##Add StateAbbr
Colleges <- Colleges %>% left_join(StateAbbr, by = 'UnitID') %>%
  left_join(StateAbbrLabels, by = 'State_Abbr')
Colleges <- Colleges[-7]
rm(StateAbbr, StateAbbrLabels)

##########
## Clean EndowGASB
EndowG <- EndowGASB
names(EndowG) <- c("UnitID", 2019:2010)
EndowGASB <- EndowG %>% select(UnitID) %>% mutate(Year=2019, EndowmentG = EndowG$`2019`) #create subset of rates

EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2018,"EndowmentG" = EndowG$`2018`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2017,"EndowmentG" = EndowG$`2017`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2016,"EndowmentG" = EndowG$`2016`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2015,"EndowmentG" = EndowG$`2015`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2014,"EndowmentG" = EndowG$`2014`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2013,"EndowmentG" = EndowG$`2013`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2012,"EndowmentG" = EndowG$`2012`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2011,"EndowmentG" = EndowG$`2011`))
EndowGASB <- bind_rows(EndowGASB, data.frame("UnitID" = EndowG$UnitID,"Year" = 2010,"EndowmentG" = EndowG$`2010`))

## Clean EndowFASB
EndowF <- EndowFASB
names(EndowF) <- c("UnitID", 2019:2010)
EndowFASB <- EndowF %>% select(UnitID) %>% mutate(Year=2019, EndowmentF = EndowF$`2019`) #create subset of rates

#Bind rows into a tidy format
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2018,"EndowmentF" = EndowF$`2018`))
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2017,"EndowmentF" = EndowF$`2017`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2016,"EndowmentF" = EndowF$`2016`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2015,"EndowmentF" = EndowF$`2015`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2014,"EndowmentF" = EndowF$`2014`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2013,"EndowmentF" = EndowF$`2013`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2012,"EndowmentF" = EndowF$`2012`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2011,"EndowmentF" = EndowF$`2011`)) 
EndowFASB <- bind_rows(EndowFASB, data.frame("UnitID" = EndowF$UnitID,"Year" = 2010,"EndowmentF" = EndowF$`2010`))

#Clean Endow2020
Endow2020 <- Endow2020 %>% 
  mutate(Endowment = ifelse(unitid == Endow2020$unitid & year == Endow2020$year &
                              is.na(Endow2020$F1920_F2.Value.of.endowment.assets.at.the.end.of.the.fiscal.year),
                                    Endow2020$F1920_F1A.Value.of.endowment.assets.at.the.end.of.the.fiscal.year, 
                                    Endow2020$F1920_F2.Value.of.endowment.assets.at.the.end.of.the.fiscal.year))
Endow2020 <- Endow2020[-c(2,4,5)]
names(Endow2020) <- c("UnitID","Year","Endowment")

#Remove Data
rm(EndowF, EndowG)

#Create Combined Endowment 
Endowment <- EndowFASB %>% cbind("EndowmentG" = EndowGASB$EndowmentG)
Endowment <- Endowment %>% 
  mutate(Endowment = ifelse(UnitID == EndowGASB$UnitID & Year == EndowGASB$Year & 
                              is.na(EndowGASB$EndowmentG), EndowmentF, EndowGASB$EndowmentG))
Endowment <- Endowment[-c(3,4)]

Endowment <- Endowment %>%
  bind_rows(Endow2020, data.frame("UnitID" = Endow2020$UnitID, "Year" = 2020, "Endowment" = Endow2020$Endowment))
                                    
#Remove Unneeded Data
rm(EndowFASB,EndowGASB, Endow2020)

#Add Endowment to Colleges & Remove unneeded data file
Colleges <- Colleges %>% right_join(Endowment, by = 'UnitID')
rm(Endowment)

##########
##Clean In-State Tuition
names(Tuition_In) <- c("UnitID", "InstName", 2020:2011) #rename columns 
InState <- Tuition_In %>% select(UnitID) %>% mutate(Year = 2020, "In" = Tuition_In$`2020`)   #create subset of rates

#Bind rows into a tidy format 
InState <- bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID,"Year" = 2019,"In" = Tuition_In$`2019`))
InState <- bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2018, "In" = Tuition_In$`2018`)) 
InState <- bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2017, "In" = Tuition_In$`2017`)) 
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2016, "In" = Tuition_In$`2016`)) 
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2015, "In" = Tuition_In$`2015`))
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2014, "In" = Tuition_In$`2014`)) 
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2013, "In" = Tuition_In$`2013`))
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2012, "In" = Tuition_In$`2012`)) 
InState <-  bind_rows(InState, data.frame("UnitID" = Tuition_In$UnitID, "Year" = 2011, "In" = Tuition_In$`2011`))

#Rename column
names(InState)[3]<-"InState"

#Remove Data
rm(Tuition_In)

#Add to Colleges Dataset
Colleges <- left_join(Colleges,InState,by=c('UnitID','Year'))

#Remove unneeded data file
rm(InState)

##########
##Clean Out-of-State Tuition
names(Tuition_Out) <- c("UnitID", "InstName", 2020:2011) #rename columns 
OutState <- Tuition_Out %>% select(UnitID) %>% mutate(Year = 2020, "Out" = Tuition_Out$`2020`)   #create subset of rates

#Bind rows into a tidy format 
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2019, "Out" = Tuition_Out$`2019`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2018, "Out" = Tuition_Out$`2018`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2017, "Out" = Tuition_Out$`2017`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2016, "Out" = Tuition_Out$`2016`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2015, "Out" = Tuition_Out$`2015`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2014, "Out" = Tuition_Out$`2014`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2013, "Out" = Tuition_Out$`2013`))
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2012, "Out" = Tuition_Out$`2012`)) 
OutState <- bind_rows(OutState, data.frame("UnitID" = Tuition_Out$UnitID, "Year" = 2011, "Out" = Tuition_Out$`2011`))

#Rename column
names(OutState)[3]<-"OutState"

#Remove Data
rm(Tuition_Out)

#Add to Colleges Data
Colleges <- Colleges %>% left_join(OutState, by = c('UnitID','Year'))
rm(OutState)

###############
##Clean Retention
names(Retention) <- c("UnitID", 2019:2010)
Fall_Ret <- Retention %>% select(UnitID) %>% mutate(Year = 2019, Retention = Retention$`2019`)  #create subset of rates

#Bind rows into a tidy format 
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2018,"Retention" = Retention$`2018`)) 
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2017,"Retention" = Retention$`2017`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2016,"Retention" = Retention$`2016`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2015,"Retention" = Retention$`2015`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2014,"Retention" = Retention$`2014`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2013,"Retention" = Retention$`2013`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2012,"Retention" = Retention$`2012`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2011,"Retention" = Retention$`2011`))
Fall_Ret <- bind_rows(Fall_Ret, data.frame("UnitID" = Retention$UnitID,"Year" = 2010,"Retention" = Retention$`2010`))

#Remove Data
rm(Retention)

#Add to Colleges Data & Remove data file
Colleges <- Colleges %>% left_join(Fall_Ret, by = c('UnitID','Year'))
rm(Fall_Ret)

########## 
## Clean Headcount
names(U_Headcount) <- c("UnitID", 2019:2010)
Headcount <- U_Headcount %>% select(UnitID) %>% mutate(Year=2019, Headcount = U_Headcount$`2019`) #create subset of rates

#Bind rows into a tidy format 
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2018,"Headcount" = U_Headcount$`2018`))
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2017,"Headcount" = U_Headcount$`2017`))
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2016,"Headcount" = U_Headcount$`2016`)) 
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2015,"Headcount" = U_Headcount$`2015`)) 
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2014,"Headcount" = U_Headcount$`2014`)) 
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2013,"Headcount" = U_Headcount$`2013`)) 
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2012,"Headcount" = U_Headcount$`2012`))
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2011,"Headcount" = U_Headcount$`2011`))
Headcount <- bind_rows(Headcount, data.frame("UnitID" = U_Headcount$UnitID,"Year" = 2010,"Headcount" = U_Headcount$`2010`))

#Remove Data
rm(U_Headcount)

#Add to Colleges Data & Remove data file
Colleges <- Colleges %>% left_join(Headcount, by = c('UnitID','Year'))
rm(Headcount)

###############
##Clean FTE
names(FTE) <- c("UnitID","Name",2019:2010)
FullTimeE <- FTE
FTE <- FullTimeE %>% select(UnitID) %>% mutate(Year=2019, FTE=FullTimeE$`2019`)     #create subset of rates

#Bind rows into a tidy format 
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2018,"FTE" = FullTimeE$`2018`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2017,"FTE" = FullTimeE$`2017`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2016,"FTE" = FullTimeE$`2016`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2015,"FTE" = FullTimeE$`2015`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2014,"FTE" = FullTimeE$`2014`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2013,"FTE" = FullTimeE$`2013`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2012,"FTE" = FullTimeE$`2012`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2011,"FTE" = FullTimeE$`2011`))
FTE <- bind_rows(FTE, data.frame("UnitID" = FullTimeE$UnitID,"Year" = 2010,"FTE" = FullTimeE$`2010`))
  
#Remove Data
rm(FullTimeE)

#Add to Colleges Data & Remove data file
Colleges <- Colleges %>% left_join(FTE, by = c('UnitID','Year'))
rm(FTE)

###############
## Clean Grad
Graduate <- GRAD
names(Graduate) <- c("UnitID", 2019:2010)
GRAD <- Graduate %>% select(UnitID) %>% mutate(Year=2019, GRAD=Graduate$`2019`)   #create subset of rates

#Bind rows into a tidy format
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2018,"GRAD" = Graduate$`2018`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2017,"GRAD" = Graduate$`2017`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2016,"GRAD" = Graduate$`2016`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2015,"GRAD" = Graduate$`2015`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2014,"GRAD" = Graduate$`2014`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2013,"GRAD" = Graduate$`2013`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2012,"GRAD" = Graduate$`2012`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2011,"GRAD" = Graduate$`2011`)) 
GRAD <- bind_rows(GRAD, data.frame("UnitID" = Graduate$UnitID,"Year" = 2010,"GRAD" = Graduate$`2010`))

#Remove Data
rm(Graduate)

#Add to Colleges Data & Remove data file
Colleges <- Colleges %>% left_join(GRAD, by = c('UnitID','Year'))
rm(GRAD)

# Reorder Columns & Rename FINAL DataSet
Colleges <- Colleges[,c(1:5,7,6,8:15)]

#Remove cases with at least one null value
Colleges <- Colleges[complete.cases(Colleges),]

#####################################################################
#Create data.frame with variables and definitions
flextable(Variables) %>% 
  set_table_properties(width = .75, layout = "autofit") %>%
  set_caption(caption = " IPEDS Definitions for Variables") %>%
  footnote(i=1, j=2, 
           value= as_paragraph("Some definitions were modified for gramatical purposes."),
           ref_symbols = "a",
           part = "header") %>%
  theme_vanilla()

#####################################################################
## Descriptives of College Dataset
#Categorical Variables 
t1 <- Colleges %>%
  select("INST_Level","Sector","Highest_Degree","INST_Size") %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_categorical() ~ "{n}"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**",
                     stat_0 ~ "**N**")) %>%
  modify_footnote(list(stat_0 ~ NA))

#Export to Flextable
College_Categ <- as_flex_table(t1, include = everything(), return_calls = FALSE,
                              strip_md_bold = TRUE) 

College_Categ %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  set_caption(caption = " Frequencies for Categorical Variables") %>%
  theme_vanilla()

#Remove unneeded table
rm(t1)

################
# Continuous Variables
t1 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD") %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**",stat_0 ~ "**Mean (SD)**")) %>%
  modify_footnote(list(stat_0 ~ NA))

t2 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD")  %>%
  tbl_summary(by = NULL, label = NULL,
              statistic = list(all_continuous() ~ "{min}"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**",stat_0 ~ "**Minimum**")) %>%
  modify_footnote(list(stat_0 ~ NA))

t3 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD")  %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_continuous() ~ "{max}"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**", stat_0 ~ "**Maximum**")) %>%
  modify_footnote(list(stat_0 ~ NA))

t4 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD")  %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_continuous() ~ "{median}"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**",stat_0 ~ "**Median**")) %>%
  modify_footnote(list(stat_0 ~ NA)) 

t5 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD") %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_continuous() ~ "{p25}"),
              missing = "no") %>%
  modify_header(list(label ~ "**Variable**", stat_0 ~ "**IQR Lower**")) %>%
  modify_footnote(list(stat_0 ~ NA)) 

t6 <- Colleges %>%
  select("Endowment","InState","OutState","Retention","Headcount","FTE","GRAD")  %>%
  tbl_summary(by = NULL, label = NULL, 
              statistic = list(all_continuous() ~ "{p75}"),
              missing = "no") %>% 
  modify_header(list(label ~ "**Variable**",stat_0 ~ "**IQR Upper**")) %>%
  modify_footnote(list(stat_0 ~ NA)) 

Col_Desc <- tbl_merge(list(t1,t2,t3,t4,t5,t6)) %>%
  modify_spanning_header(everything() ~ NA_character_)

# Remove Unneeded Values & Tables
rm(t1,t2,t3,t4,t5,t6)

#Export to Flextable
College_Desc <- as_flex_table(Col_Desc, include = everything() , return_calls = FALSE,
                              strip_md_bold = TRUE)
College_Desc <- College_Desc %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  set_caption(caption = " Descriptive Statistics for Continuous Variables") %>%
  theme_vanilla()

# Remove Uneeded Table
rm(Col_Desc)

######################################################################
#Top 10 Endowments 
#Subset data into one year 
FinalYr <- filter(Colleges, Year == 2019)

#Arrange data from largest to smallest Endowment
Top <- arrange(FinalYr,desc(Endowment)) %>% select(INST, Sector, Endowment) 

#Get Top 10 Endowments
Top10 <- head(Top,10)

#Convert Endowment to currency 
Top10 <- gt(Top10)
Top10 <- fmt_currency(Top10, "Endowment", currency = "USD",sep_mark = ",") %>%
  as.data.frame()

#Final Table for presentation
flextable(Top10) %>% 
  set_table_properties(width = .75, layout = "autofit") %>%
  set_caption(caption = " Top 10 Highest Endowments - All Institutions") %>%
  theme_vanilla()

#Get Top 10 Private Endowments 
Top10_Pri <- Top %>% filter(Sector == 'Private not-for-profit')
Top10_Pri <- head(Top10_Pri,10)
Top10_Pri <- gt(Top10_Pri)
Top10_Pri <- fmt_currency(Top10_Pri, "Endowment", currency = "USD",sep_mark = ",") %>%
  as.data.frame()

flextable(Top10_Pri) %>% 
  set_table_properties(width = .75, layout = "autofit") %>%
  set_caption(caption = " Top 10 Highest Endowments - Private Non-for-Profit Institutions") %>%
  theme_vanilla()

#Get Top 10 Public Endowments 
Top10_Pub <- Top %>% filter(Sector == 'Public')
Top10_Pub <- head(Top10_Pub,10)
Top10_Pub <- gt(Top10_Pub)
Top10_Pub <- fmt_currency(Top10_Pub, "Endowment", currency = "USD",sep_mark = ",") %>%
  as.data.frame()

flextable(Top10_Pub) %>% 
  set_table_properties(width = .75, layout = "autofit") %>%
  set_caption(caption = " Top 10 Highest Endowments - Public Institutions") %>%
  theme_vanilla()

