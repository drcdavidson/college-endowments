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

## Clean Data and Combine Files into the Dataset ##
Colleges <- INST_Characteristics %>% 
  mutate(INST_Level="Four or more years")

Colleges$INST_Control[Colleges$INST_Control == 1] <- "Public"
Colleges$INST_Control[Colleges$INST_Control == 2] <- "Private not-for-profit"

INST_Characteristics_Values <- INST_Characteristics_Values[-c(1:3),]

Colleges$Highest_Degree[Colleges$Highest_Degree == 11] <- "Doctor's degree - research/scholarship and professional practice"
Colleges$Highest_Degree[Colleges$Highest_Degree == 12] <- "Doctor's degree - research/scholarship"
Colleges$Highest_Degree[Colleges$Highest_Degree == 13] <- "Doctor's degree -  professional practice"
Colleges$Highest_Degree[Colleges$Highest_Degree == 14] <- "Doctor's degree - other"
Colleges$Highest_Degree[Colleges$Highest_Degree == 20] <- "Master's degree"
Colleges$Highest_Degree[Colleges$Highest_Degree == 30] <- "Bachelor's degree"
Colleges$Highest_Degree[Colleges$Highest_Degree == 40] <- "Associate's degree"
Colleges$Highest_Degree[Colleges$Highest_Degree == 0]  <- "Non-degree granting"

#Remove unneeded data
rm(INST_Characteristics, INST_Characteristics_Values)

Colleges <- Colleges %>% left_join()
