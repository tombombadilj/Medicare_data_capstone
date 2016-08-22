#Loading dataset into R
#rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
#install.packages("party")
library(party)
#install.packages("gmodels")
library(gmodels)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
#install.packages("car")
library(car)
library(rpart)
library(rpart.plot)
#install.packages("DMwR")
library(DMwR)
library(caret)
library(e1071)
#install.packages("party")
library(party)
library(randomForest)
#install.packages("rattle")
#install.packages("RColorBrewer")
library(rattle)
library(RColorBrewer)
#install.packages("tree")
library(tree)

home<-"/Users/Apple/Desktop/Foundation of Data Science/"
path2015 <- "/Users/Apple/Desktop/Foundation of Data Science/Medicare_data/2015/"
HCAHPS2015 <- fread(paste(path2015,"HCAHPS - Hospital.csv", sep=""))
HCAHPS2015 <- tbl_df(HCAHPS2015)

#Examining the data
dim(HCAHPS2015)
names(HCAHPS2015)
glimpse(HCAHPS2015)

#Examining the not applicable vs. Not available
x <- HCAHPS2015 %>% select(`HCAHPS Measure ID`,`HCAHPS Answer Percent`) %>% 
    filter(`HCAHPS Answer Percent`=="Not Applicable"| `HCAHPS Answer Percent`=="Not Available") %>%
    group_by(`HCAHPS Measure ID`,`HCAHPS Answer Percent`) %>% arrange(`HCAHPS Answer Percent`) %>%
    summarise(new = n())
#View(x)
unique(HCAHPS2015$`HCAHPS Measure ID`)

#Creating a data dictionary for all the HCAHPS items included in the dataset
data_list <- HCAHPS2015 %>% group_by(`HCAHPS Measure ID`,`HCAHPS Question`) %>%
    select(`HCAHPS Measure ID`,`HCAHPS Question`) %>%
    summarise(n=n()) 
#View(data_list)
write.csv(data_list,paste(home,"HCAHPS_Questions.csv",sep=""))
#View(HCAHPS2015)

#creating a subset of data -always for patients who expressed high satisfaction
y <- grepl(pattern = "_A_P", unique(HCAHPS2015$`HCAHPS Measure ID`)) | 
    grepl(pattern = "_SA", unique(HCAHPS2015$`HCAHPS Measure ID`)) |
    grepl(pattern = "_9_10", unique(HCAHPS2015$`HCAHPS Measure ID`)) |
    grepl(pattern = "_DY", unique(HCAHPS2015$`HCAHPS Measure ID`)) |
    grepl(pattern = "_Y_P", unique(HCAHPS2015$`HCAHPS Measure ID`))
z <- unique(HCAHPS2015$`HCAHPS Measure ID`)[y]
    
always <- HCAHPS2015 %>% select(`Provider ID`, `HCAHPS Measure ID`,
        `HCAHPS Answer Percent`) %>% filter(`HCAHPS Measure ID` %in% z)
dim(always)

#start here
#Explore NAs
names(HCAHPS2015)
unique(HCAHPS2015$`HCAHPS Answer Percent Footnote`)
Missing <- HCAHPS2015 %>% 
    filter(`HCAHPS Answer Percent`=="Not Available") %>%
    select(`HCAHPS Measure ID`,`HCAHPS Answer Percent`,`HCAHPS Answer Percent Footnote`)
table(Missing$`HCAHPS Answer Percent Footnote`)

#reassign Not Available to NA
always$`HCAHPS Answer Percent`[always$`HCAHPS Answer Percent`=="Not Available"] <- NA
always$`HCAHPS Answer Percent`<-as.numeric(always$`HCAHPS Answer Percent`)
summary(always)

#Changing always from long form to wide form
al_wide <- always %>% spread(`HCAHPS Measure ID`,`HCAHPS Answer Percent`)
#View(al_wide)
names(al_wide)
summary(al_wide)
dim(al_wide)

#There are 4643 hospitals in 2015
#Cleanliness
boxplot(al_wide$H_CLEAN_HSP_A_P, horizontal = TRUE, 
        main = "Cleanliness (Always Clean)",
        xlab = "% of Patients")
hist(al_wide$H_CLEAN_HSP_A_P, main = "Cleanliness (Always Clean)",
     xlab = "% of Patients")
summary(al_wide$H_CLEAN_HSP_A_P)

#Nurse Communication
boxplot(al_wide$H_COMP_1_A_P, horizontal = TRUE, 
        main = "Nurse Communication \n(Always Communicated Well)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_1_A_P, main = "Nurse Communication \n(Always Communicated Well)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_1_A_P)

#Doctor Communication
boxplot(al_wide$H_COMP_2_A_P, horizontal = TRUE, 
        main = "Doctor Communication \n(Always Communicated Well)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_2_A_P, main = "Doctor Communication \n(Always Communicated Well)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_2_A_P)

#Staff Responsiveness
boxplot(al_wide$H_COMP_3_A_P, horizontal = TRUE, 
        main = "Staff Responsiveness \n(Always Received Help)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_3_A_P, main = "Staff Responsiveness \n(Always Received Help)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_3_A_P)

#Staff Responsiveness
boxplot(al_wide$H_COMP_4_A_P, horizontal = TRUE, 
        main = "Pain Control \n(Always Well)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_4_A_P, main = "Pain Control \n(Always Well)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_4_A_P)

#Communication about Medicines
boxplot(al_wide$H_COMP_5_A_P, horizontal = TRUE, 
        main = "Communication about Medicines \n(Always Well)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_5_A_P, main = "Communication about Medicines \n(Always Well)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_5_A_P)

#Discharge Information
boxplot(al_wide$H_COMP_6_Y_P, horizontal = TRUE, 
        main = "Discharge Information \n(Yes)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_6_Y_P, main = "Discharge Information \n(Yes)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_6_Y_P)

#Transition Care
boxplot(al_wide$H_COMP_7_SA, horizontal = TRUE, 
        main = "Transition Care \n(Strongly Agree)",
        xlab = "% of Patients")
hist(al_wide$H_COMP_7_SA, main = "Transition Care \n(Strongly Agree)",
     xlab = "% of Patients")
summary(al_wide$H_COMP_7_SA)

#Rating
boxplot(al_wide$H_HSP_RATING_9_10, horizontal = TRUE, 
        main = "Patient Rating 9-10",
        xlab = "% of Patients")
hist(al_wide$H_HSP_RATING_9_10, main = "Patient Rating 9-10",
     xlab = "% of Patients")
summary(al_wide$H_HSP_RATING_9_10)

#Patient Recommendation
boxplot(al_wide$H_RECMND_DY, horizontal = TRUE, 
        main = "Patient Recommendation \n(Definitely Yes)",
        xlab = "% of Patients")
hist(al_wide$H_RECMND_DY, main = "Patient Recommendation \n(Definitely Yes)",
     xlab = "% of Patients")
summary(al_wide$H_RECMND_DY)

#Quietness
boxplot(al_wide$H_QUIET_HSP_A_P, horizontal = TRUE, 
        main = "Quietness \n(Always)",
        xlab = "% of Patients")
hist(al_wide$H_QUIET_HSP_A_P, main = "Quietness \n(Always)",
     xlab = "% of Patients")

summary(al_wide$H_QUIET_HSP_A_P)

summary(al_wide)

#Correlation matrix

al_dt <- data.table(al_wide)
al_corr <- al_dt[,2:12, with=FALSE]
class(al_corr)
names(al_corr)
setnames(al_corr, names(al_corr),as.character(c(1:11)))
al_corr
M <- cor(al_corr, use = 'pairwise.complete.obs',method='pearson')
M
corrplot(M, method="circle", type='upper')

#Import 2014 data
path2014 <- "/Users/Apple/Desktop/Foundation of Data Science/Medicare_data/2014/"
HCAHPS2014 <- fread(paste(path2014,"HCAHPS - Hospital.csv", sep=""))
HCAHPS2014 <- tbl_df(HCAHPS2014)
View(HCAHPS2014)

dim(HCAHPS2014)
names(HCAHPS2014)
glimpse(HCAHPS2014)
summary(HCAHPS2014)

x <- HCAHPS2014 %>% select(`HCAHPS Measure ID`,`HCAHPS Answer Percent`) %>% 
    filter(`HCAHPS Answer Percent`=="Not Applicable"| `HCAHPS Answer Percent`=="Not Available") %>%
    group_by(`HCAHPS Measure ID`,`HCAHPS Answer Percent`) %>% arrange(`HCAHPS Answer Percent`) %>%
    summarise(new = n())
View(x)

length(unique(HCAHPS2014$`Provider ID`))

always2014 <- HCAHPS2014 %>% select(`Provider ID`, `HCAHPS Measure ID`,
                                `HCAHPS Answer Percent`) %>% filter(`HCAHPS Measure ID` %in% z)
View(always2014)
always2014$`HCAHPS Answer Percent`[always2014$`HCAHPS Answer Percent`=="Not Available"] <- NA
always2014$`HCAHPS Answer Percent`<-as.numeric(always2014$`HCAHPS Answer Percent`)
summary(always2014)


al_wide_2014 <- always2014 %>% spread(`HCAHPS Measure ID`,`HCAHPS Answer Percent`)
View(al_wide_2014)
dim(al_wide_2014)
summary(al_wide_2014)
names(al_wide_2014)

#Cleanliness
ggplot(al_wide_2014, aes(x = factor(0), y = H_CLEAN_HSP_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Cleanliness (Always Clean)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_CLEAN_HSP_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Cleanliness (Always Clean)", x = "% of Patients")

#Nurse Communication
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_1_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Nurse Communication \n(Always Communicated Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_1_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Nurse Communication \n(Always Communicated Well)", 
         x = "% of Patients")

#Doctor Communication
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_2_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Doctor Communication \n(Always Communicated Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_2_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Doctor Communication \n(Always Communicated Well)", 
         x = "% of Patients")

#Staff Responsiveness
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_3_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Staff Responsiveness \n(Always Received Help)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_3_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Staff Responsiveness \n(Always Received Help)", 
         x = "% of Patients")

#Staff Responsiveness
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_4_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Pain Control \n(Always Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_4_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Pain Control \n(Always Well)", 
         x = "% of Patients")

#Communication about Medicines
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_5_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Communication about Medicines \n(Always Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_5_A_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Communication about Medicines \n(Always Well)", 
         x = "% of Patients")

#Discharge Information
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_6_Y_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Discharge Information \n(Yes)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_6_Y_P,data=al_wide_2014, geom="histogram") +
    labs(title = "Discharge Information \n(Yes)", 
         x = "% of Patients")

#Transition Care
ggplot(al_wide_2014, aes(x = factor(0), y = H_COMP_7_SA)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Transition Care \n(Strongly Agree)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_7_SA,data=al_wide_2014, geom="histogram") +
    labs(title = "Transition Care \n(Strongly Agree)", 
         x = "% of Patients")

#Rating
ggplot(al_wide_2014, aes(x = factor(0), y = H_HSP_RATING_9_10)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Patient Rating 9-10") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_HSP_RATING_9_10, data=al_wide_2014, geom="histogram") +
    labs(title = "Patient Rating 9-10", 
         x = "% of Patients")

#Patient Recommendation
ggplot(al_wide_2014, aes(x = factor(0), y = H_RECMND_DY)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Patient Recommendation \n(Definitely Yes)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_RECMND_DY, data=al_wide_2014, geom="histogram") +
    labs(title = "Patient Recommendation \n(Definitely Yes)", 
         x = "% of Patients")

#Quietness
ggplot(al_wide_2014, aes(x = factor(0), y = H_QUIET_HSP_A_P)) +
    geom_boxplot() + xlab("% of Patients") + 
    ggtitle("Quietness \n(Always)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_QUIET_HSP_A_P, data=al_wide_2014, geom="histogram") +
    labs(title = "Quietness \n(Always)", 
         x = "% of Patients")
summary(al_wide_2014$H_QUIET_HSP_A_P)

#correlation analysis
al_dt_2014 <- data.table(al_wide_2014)
al_corr_2014 <- al_dt_2014[,2:12, with=FALSE]
class(al_corr_2014)
names(al_corr_2014)
setnames(al_corr_2014, names(al_corr_2014),as.character(c(1:11)))
al_corr_2014
M <- cor(al_corr_2014, use = 'pairwise.complete.obs',method='pearson')
M
corrplot(M, method="circle", type='upper')
rcorr(as.matrix(al_corr_2014))

#bringing in 2013 data
path2013 <- "/Users/Apple/Desktop/Foundation of Data Science/Medicare_data/2013/"
HCAHPS2013 <- fread(paste(path2013,"HCAHPS Measures.csv", sep=""))
HCAHPS2013 <- tbl_df(HCAHPS2013)
View(HCAHPS2013)
head(HCAHPS2013)
summary(HCAHPS2013)
names(HCAHPS2013)
names(al_wide_2014)
HCAHPS2013 <- data.table(HCAHPS2013)
#selecting the necessary variables
always2013 <- HCAHPS2013 %>% select(c(1,28,13,16,19,22,25,32,36,31,39))
y <- names(al_wide_2014)
z <- y[c(1:8,10:12)]
al_wide_2013 <- setnames(always2013,names(always2013), z)
View(al_wide_2013)
names(always2013)
length(names(al_wide_2013))


for (i in 2:11){al_wide_2013[,i, with=FALSE] <- as.numeric(al_wide_2013[,i, with=FALSE])}
for (i in 2:11){al_wide_2013[[i]] <- as.numeric(al_wide_2013[[i]])}
summary(al_wide_2013)
length(al_wide_2013$`Provider ID`)
#Cleanliness
ggplot(al_wide_2013, aes(x = factor(0), y = H_CLEAN_HSP_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Cleanliness (Always Clean)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_CLEAN_HSP_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Cleanliness (Always Clean)", x = "% of Patients")

#Nurse Communication
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_1_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Nurse Communication \n(Always Communicated Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_1_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Nurse Communication \n(Always Communicated Well)", 
         x = "% of Patients")

#Doctor Communication
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_2_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Doctor Communication \n(Always Communicated Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_2_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Doctor Communication \n(Always Communicated Well)", 
         x = "% of Patients")

#Staff Responsiveness
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_3_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Staff Responsiveness \n(Always Received Help)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_3_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Staff Responsiveness \n(Always Received Help)", 
         x = "% of Patients")

#Staff Responsiveness
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_4_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Pain Control \n(Always Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_4_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Pain Control \n(Always Well)", 
         x = "% of Patients")

#Communication about Medicines
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_5_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Communication about Medicines \n(Always Well)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_5_A_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Communication about Medicines \n(Always Well)", 
         x = "% of Patients")

#Discharge Information
ggplot(al_wide_2013, aes(x = factor(0), y = H_COMP_6_Y_P)) +
    geom_boxplot() + xlab("% of Patients") + xlab("") +
    ggtitle("Discharge Information \n(Yes)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_COMP_6_Y_P,data=al_wide_2013, geom="histogram") +
    labs(title = "Discharge Information \n(Yes)", 
         x = "% of Patients")

#Rating
ggplot(al_wide_2013, aes(x = factor(0), y = H_HSP_RATING_9_10)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Patient Rating 9-10") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_HSP_RATING_9_10, data=al_wide_2013, geom="histogram") +
    labs(title = "Patient Rating 9-10", 
         x = "% of Patients")

#Patient Recommendation
ggplot(al_wide_2013, aes(x = factor(0), y = H_RECMND_DY)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Patient Recommendation \n(Definitely Yes)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_RECMND_DY, data=al_wide_2013, geom="histogram") +
    labs(title = "Patient Recommendation \n(Definitely Yes)", 
         x = "% of Patients")

#Quietness
ggplot(al_wide_2013, aes(x = factor(0), y = H_QUIET_HSP_A_P)) +
    geom_boxplot() + ylab("% of Patients") + xlab("") +
    ggtitle("Quietness \n(Always)") +
    scale_x_discrete(breaks = NULL) + coord_flip()
qplot(H_QUIET_HSP_A_P, data=al_wide_2013, geom="histogram") +
    labs(title = "Quietness \n(Always)", 
         x = "% of Patients")
summary(al_wide_2013)

#correlation analysis
al_dt_2013 <- data.table(al_wide_2013)
al_corr_2013 <- al_dt_2013[,2:11, with=FALSE]
class(al_corr_2013)
names(al_corr_2013)
setnames(al_corr_2013, names(al_corr_2013),as.character(c(1:10)))
al_corr_2013
M <- cor(al_corr_2013, use = 'pairwise.complete.obs',method='pearson')
M
corrplot(M, method="circle", type='upper')
corr2013 <- rcorr(as.matrix(al_corr_2013))
corr2013

#imputation-2015
summary(al_wide)
al_wide_imp <- al_wide
for (i in 2:12) {al_wide_imp[[i]][which(is.na(al_wide_imp[[i]]))] <- mean(al_wide_imp[[i]],na.rm=T)}
summary(al_wide_imp)
View(al_wide_imp)
al_wide_imp$year <- 2015

#imputation_2014
summary(al_wide_2014)
al_wide_2014_imp <- al_wide_2014
for (i in 2:12) {al_wide_2014_imp[[i]][which(is.na(al_wide_2014_imp[[i]]))] <- mean(al_wide_2014_imp[[i]],na.rm=T)}
summary(al_wide_2014_imp)
al_wide_2014_imp$year <- 2014

#imputation_2013
summary(al_wide_2013)
length(names(al_wide_2013))
al_wide_2013_imp<-al_wide_2013
for (i in 2:11) {al_wide_2013_imp[[i]][which(is.na(al_wide_2013_imp[[i]]))] <- mean(al_wide_2013_imp[[i]],na.rm=T)}
summary(al_wide_2013_imp)
al_wide_2013_imp$year <- 2013
View(al_wide_2013_imp)
al_wide_2013_imp$H_COMP_7_SA <- NA

#building a full dataset
comb_20152014 <- rbind(al_wide_imp,al_wide_2014_imp)
fulldata <- rbind(comb_20152014, al_wide_2013_imp)
summary(fulldata)
fulldata %>% group_by(year) %>% summarise(n=n())
fulldata_arranged <- setcolorder(fulldata, c("Provider ID","year","H_COMP_1_A_P","H_COMP_2_A_P",
                                             "H_COMP_3_A_P","H_COMP_4_A_P","H_COMP_5_A_P",
                                             "H_COMP_6_Y_P","H_COMP_7_SA","H_CLEAN_HSP_A_P",
                                             "H_QUIET_HSP_A_P","H_RECMND_DY","H_HSP_RATING_9_10"))
fulldata_l <- fulldata %>% gather("measure","percentage",3:13)
summarydata <- fulldata_l %>% group_by(year,measure) %>% summarise(n =n(),min=min(percentage),
                                                                   median = median(percentage),
                                                                   max=max(percentage),mean=mean(percentage))
summarydata$Measure[summarydata$measure == "H_CLEAN_HSP_A_P"] <- "Cleanliness"
summarydata$Measure[summarydata$measure == "H_COMP_1_A_P"] <- "Nurse Communication"
summarydata$Measure[summarydata$measure == "H_COMP_2_A_P"] <- "Physician Communication"
summarydata$Measure[summarydata$measure == "H_COMP_3_A_P"] <- "Staff Reponsiveness"
summarydata$Measure[summarydata$measure == "H_COMP_4_A_P"] <- "Pain Control"
summarydata$Measure[summarydata$measure == "H_COMP_5_A_P"] <- "Communication about Medicine"
summarydata$Measure[summarydata$measure == "H_COMP_6_Y_P"] <- "Discharge Information"
summarydata$Measure[summarydata$measure == "H_COMP_7_SA"] <- "Care Transition"
summarydata$Measure[summarydata$measure == "H_HSP_RATING_9_10"] <- "Rating of 9 or 10"
summarydata$Measure[summarydata$measure == "H_QUIET_HSP_A_P"] <- "Quietness"
summarydata$Measure[summarydata$measure == "H_RECMND_DY"] <- "Recommendation"

write.csv(summarydata,paste0(home,"summarydata.csv"))

View(summarydata)
ggplot(summarydata, aes(x=factor(year),y=mean, group=Measure,col=Measure)) +
    geom_point()+geom_line()

#boxplot for all three years:
report <- subset(fulldata, year == 2014|year == 2015)
summary(report)
report$year <- factor(report$year)

ggplot(report, aes(x = year, y = H_COMP_1_A_P)) +
    geom_boxplot(aes(fill=year)) +xlab("Year")+ ylab("% of patients") + 
    theme(legend.position="none") + 
    ggtitle("Nurse Communication \n(Always Communicated Well)")

ggplot(report, aes(x = factor(year), y = H_COMP_2_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Doctor Communication \n(Always Communicated Well)")

ggplot(report, aes(x = factor(year), y = H_COMP_3_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Staff Responsiveness \n(Always Received Help)")

ggplot(report, aes(x = factor(year), y = H_COMP_4_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Pain Control \n(Always Well)")

ggplot(report, aes(x = factor(year), y = H_COMP_5_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Communication about Medicines \n(Always Well)")

ggplot(report, aes(x = factor(year), y = H_COMP_6_Y_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Discharge Information \n(Yes)")

ggplot(report, aes(x = factor(year), y = H_COMP_7_SA)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Transition Care \n(Strongly Agree)")

ggplot(report, aes(x = factor(year), y = H_CLEAN_HSP_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Cleanliness (Always Clean)")

ggplot(report, aes(x = factor(year), y = H_QUIET_HSP_A_P)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Quietness \n(Always)")

ggplot(report, aes(x = factor(year), y = H_HSP_RATING_9_10)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Patient Rating 9-10")

ggplot(report, aes(x = factor(year), y = H_RECMND_DY)) +
    geom_boxplot(aes(fill=factor(year))) +xlab("Year")+ ylab("% of patients") + theme(legend.position="none") +
    ggtitle("Patient Recommendation \n(Definitely Yes)")

summary(al_wide_imp)
summary(al_wide_2013_imp)
summary(al_wide_2014_imp)
order <- c("Provider ID","year","H_CLEAN_HSP_A_P","H_COMP_1_A_P", "H_COMP_2_A_P",
           "H_COMP_3_A_P", "H_COMP_4_A_P","H_COMP_5_A_P",
           "H_COMP_6_Y_P", "H_COMP_7_SA","H_HSP_RATING_9_10",
           "H_QUIET_HSP_A_P","H_RECMND_DY")
setcolorder(al_wide_imp,order)
setcolorder(al_wide_2014_imp,order)
setcolorder(al_wide_2013_imp,order)
length(al_wide_imp)
#t-test between all variables
a<-t.test(al_wide_imp[[3]],al_wide_imp[[4]])
a$estimate[1]

tloop <- function(data,year) {
    x <- 3
    tresults <- data.frame(x = integer(), y= integer(), p_value= integer(), year=integer())
    while (x < length(data)){
        for (y in (x+1):length(data)) {
            if (sum(is.na(data[[x]]))==0 & sum(is.na(data[[y]]))==0){
                a <- t.test(data[[x]],data[[y]])
                b <- cbind(x,a$estimate[1],y,a$estimate[2],a$p.value,year)
                print(b)
                tresults<-rbind(tresults,b)}
            else if ((!(sum(is.na(data[[x]]))==0))|(!(sum(is.na(data[[y]]))==0))) {next}
        }
    x <- x+1    
    }
    return(tresults)
}

?t.test()

tresults <- tloop(al_wide_imp,"2015")
rownames(tresults) <- 1:nrow(tresults)
View(tresults)
tresults14 <- tloop(al_wide_2014_imp,"2014")
rownames(tresults14) <- 1:nrow(tresults14)
tresults14
tresults13 <- tloop(al_wide_2013_imp,"2013")
rownames(tresults13) <- 1:nrow(tresults13)
tresults13
final_results <- rbind(tresults,tresults14,tresults13)
final_results <- tbl_df(final_results)
final_results <- final_results %>% arrange(x,y,desc(year))
View(final_results)
write.csv(final_results,paste(home,"t_test_results.csv",sep=""))

# adding state information
summary(always2014)
unique(always2014$`HCAHPS Measure ID`)
state2014mul <- HCAHPS2014 %>% select(`Provider ID`,State)
state2014 <- unique(state2014mul)
always_state_2014 <- left_join(always2014,state2014,by="Provider ID")
View(always_state_2014)
summary(always_state_2014)
always_state_2014_l <- always_state_2014 %>% spread(`HCAHPS Measure ID`,`HCAHPS Answer Percent`)
summary(always_state_2014_l)
names(always_state_2014_l)
for (i in 3:13) {always_state_2014_l[[i]][which(is.na(always_state_2014_l[[i]]))] <- mean(always_state_2014_l [[i]],na.rm=T)}
always_state_2014 <- gather(always_state_2014_l,`HCAHPS Measure ID`,`HCAHPS Answer Percent`, H_CLEAN_HSP_A_P, H_COMP_1_A_P,H_COMP_2_A_P, H_COMP_3_A_P, H_COMP_4_A_P,H_COMP_5_A_P, H_COMP_6_Y_P, H_COMP_7_SA, H_HSP_RATING_9_10, H_QUIET_HSP_A_P, H_RECMND_DY)
summary(always_state_2014)
names(always_state_2014)
View(always_state_2014)
summarize_state <- always_state_2014 %>% group_by(State,`HCAHPS Measure ID`) %>%
    summarise(topboxmean = mean(`HCAHPS Answer Percent`))
head(summarize_state)
View(summarize_state)
long_state <- summarize_state %>% spread(`HCAHPS Measure ID`,topboxmean)
View(long_state)
summary(long_state)
distance <- dist(long_state[,2:12], method = "euclidean")
distance
state_cl <- hclust(distance, method = "ward.D")
plot(state_cl)
clustergroups4 <- cutree(state_cl, k=5)
long_state$cluster <- clustergroups4
summary(long_state$cluster)
clean <- tapply(long_state$H_CLEAN_HSP_A_P,clustergroups4, mean)
quiet <- tapply(long_state$H_QUIET_HSP_A_P,clustergroups4, mean)
nurse <- tapply(long_state$H_COMP_1_A_P,clustergroups4, mean)
physcian <- tapply(long_state$H_COMP_2_A_P,clustergroups4, mean)
staff <- tapply(long_state$H_COMP_3_A_P,clustergroups4, mean)
pain <- tapply(long_state$H_COMP_4_A_P,clustergroups4, mean)
meds <- tapply(long_state$H_COMP_5_A_P,clustergroups4, mean)
discharge <-tapply(long_state$H_COMP_6_Y_P, clustergroups4, mean)
transition <- tapply(long_state$H_COMP_7_SA,clustergroups4, mean)
recommend <- tapply(long_state$H_RECMND_DY,clustergroups4, mean)
overall <- tapply(long_state$H_HSP_RATING_9_10, clustergroups4, mean)
table <- rbind(nurse, physcian, staff, pain, meds, discharge, transition, quiet,
               clean, recommend, overall)
write.csv(table, file = "cluster4_new.csv")
names(long_state)
state_cluster <- long_state %>% select(State,cluster)
summary(state_cluster)
write.csv(state_cluster, file = "state_cluster_list.csv")
always_state_2014_l
ID_state <- always_state_2014_l %>% select(`Provider ID`,State)
View(ID_state)
ID_cluster <- left_join(ID_state,state_cluster,by="State")
View(ID_cluster)
#adding states to al_wide_2014_imp
al_wide_2014_imp <- left_join(al_wide_2014_imp, ID_cluster, by="Provider ID")
summary(al_wide_2014_imp)
al_wide_2014_imp$cluster <- factor(al_wide_2014_imp$cluster)

#adding state cluster to 2015 data
StateID2015 <- HCAHPS2015 %>% select(`Provider ID`,State)
StateID2015_unique <- StateID2015[!duplicated(StateID2015),]
View(StateID2015_unique)
State_ID_cluster <- left_join(StateID2015_unique, state_cluster, by="State")
View(State_ID_cluster)
summary(al_wide_imp)
al_wide_imp <- left_join(al_wide_imp, State_ID_cluster, by = "Provider ID")
summary(al_wide_imp)
al_wide_imp$cluster <- factor(al_wide_imp$cluster)


#glm models
model1_glm <- glm(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                      H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                      H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY + cluster,
                  data=al_wide_2014_imp,family=gaussian(link="identity"))
summary(model1_glm)
vif(model1_glm)

model2_glm <- glm(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+
                      H_COMP_4_A_P+H_COMP_6_Y_P+
                      H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                  data=al_wide_2014_imp,family=gaussian(link="identity"))
vif(model2_glm)
confint(model2_glm)
summary(model2_glm)

model3_glm <- glm(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+
                      H_COMP_6_Y_P+
                      H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY,
                  data=al_wide_2014_imp,family=gaussian(link="identity"))
vif(model3_glm)
summary(model3_glm)

#model2 is the best so far

glm_2015.pred <- predict(model2_glm, newdata = al_wide_imp)
glm_2015.sse <- sum((glm_2015.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
glm_2015.rmse <- sqrt(glm_2015.sse/nrow(al_wide_imp))
glm_2015.sse
glm_2015.rmse

#fit a binomial model - with 100 being the total
full_score <- 100
al_wide_2014_imp$opportunity <- full_score - al_wide_2014_imp$H_HSP_RATING_9_10
summary(al_wide_2014_imp$opportunity)

model1_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+H_COMP_2_A_P+
                          H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model1_bin_glm)
vif(model1_bin_glm)

model2_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model2_bin_glm)
vif(model2_bin_glm)

#remove staff
model3_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model3_bin_glm)
vif(model3_bin_glm)

#remove staff and pain
model4_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_5_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
vif(model4_bin_glm)
summary(model4_bin_glm)

#remove staff and meds
model5_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_4_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model5_bin_glm)
vif(model5_bin_glm)

#remove pain 
model6_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_3_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model6_bin_glm)
vif(model6_bin_glm)

#remove pain and meds
model7_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_3_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model7_bin_glm)
vif(model7_bin_glm)


#remove Meds
model8_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial)
summary(model8_bin_glm)
vif(model8_bin_glm)

#remove pain, meds, and staff
model9_bin_glm <- glm(cbind(H_HSP_RATING_9_10, opportunity)~ H_COMP_1_A_P+
                          H_COMP_6_Y_P+
                          H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                      data=al_wide_2014_imp,family = binomial(link="logit"))
summary(model9_bin_glm)
vif(model9_bin_glm)

#model 8 was the best
exp(model8_bin_glm$coefficients)
exp(cbind(OR = coef(model8_bin_glm), confint(model8_bin_glm)))

#validation using 2015 data
al_wide_imp$opportunity <- full_score - al_wide_imp$H_HSP_RATING_9_10
bin_glm_2015.pred <- predict(model8_bin_glm, newdata = al_wide_imp, type = "response")*100
bin_glm_2015.sse <- sum((bin_glm_2015.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
bin_glm_2015.RMSE <- sqrt(bin_glm_2015.sse/nrow(al_wide_imp))
bin_glm_2015.sse
bin_glm_2015.RMSE

View(al_wide_2014_imp)
summary(al_wide_2014_imp)

#CART model
set.seed(1)
model1_tree <- rpart(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                         H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                         H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                     data=al_wide_2014_imp, method = "anova")
prp(model1_tree)
summary(model1_tree)

#finding the cp with the lowest error
model2_tree <- rpart(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                         H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                         H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                     data=al_wide_2014_imp, method = "anova", cp = 10^(-8))
plotcp(model2_tree)
model2_tree$cptable
a <- model2_tree$cptable[which.min(model2_tree$cptable[,"xerror"]),"CP"]
a
model2_tree_cv <- rpart(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                         H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                         H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                     data=al_wide_2014_imp, method = "anova", cp = a)
plotcp(model2_tree_cv)
prp(model2_tree_cv)
sink("myfile", append=FALSE, split=FALSE)
summary(model2_tree_cv)
sink()
pruned <- prune(model2_tree, cp=a)
summary(pruned)
prp(pruned)

#running the 1-SE rule
set.seed(1)
model3_tree <- rpartXse(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                            H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                            H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                        data=al_wide_2014_imp, method = "anova")
summary(model3_tree)
model3_tree$cptable

prp(model3_tree)
model1_tree$variable.importance
summary(model1_tree)
summary(model2_tree_cv)
summary(model3_tree)

#start reading
#SSE and RMSE for tree models
mo1.pred <- predict(model1_tree, newdata = al_wide_imp)
mo1.sse <- sum((mo1.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
mo1.rmse <- sqrt(mo1.sse/nrow(al_wide_imp))

mo2.pred <- predict(model2_tree_cv, newdata = al_wide_imp)
mo2.sse <- sum((mo2.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
mo2.rmse <- sqrt(mo2.sse/nrow(al_wide_imp))

mop.pred <- predict(pruned, newdata = al_wide_imp)
mop.sse <- sum((mop.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
mop.rmse <- sqrt(mop.sse/nrow(al_wide_imp))

mo3.pred <- predict(model3_tree, newdata = al_wide_imp)
mo3.sse <- sum((mo3.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
mo3.rmse <- sqrt(mo3.sse/nrow(al_wide_imp))

mo1.rmse
mo2.rmse
mop.rmse
mo3.rmse


#random forest
summary(al_wide_2014_imp)
names(al_wide_2014_imp)
set.seed(1)
model1_forest <- randomForest(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+H_COMP_2_A_P+
                                  H_COMP_3_A_P+H_COMP_4_A_P+H_COMP_5_A_P+H_COMP_6_Y_P+
                                  H_COMP_7_SA+H_CLEAN_HSP_A_P+H_QUIET_HSP_A_P+H_RECMND_DY+cluster,
                               data=al_wide_2014_imp, importance = TRUE)
summary(model1_forest)
model1_forest$importance
importance(model1_forest)

model2_forest <- randomForest(H_HSP_RATING_9_10 ~ H_COMP_1_A_P+
                                  H_COMP_3_A_P+H_COMP_4_A_P+
                                  H_COMP_7_SA+H_RECMND_DY,
                              data=al_wide_2014_imp, importance = TRUE)

importance(model2_forest)

forest_2015.pred <- predict(model1_forest, al_wide_imp)
forest_2015.sse <- sum((forest_2015.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
forest_2015.rmse <- sqrt(forest_2015.sse/nrow(al_wide_imp))

forest_2.pred <- predict(model2_forest, al_wide_imp)
forest_2.sse <- sum((forest_2.pred - al_wide_imp$H_HSP_RATING_9_10)^2)
forest_2.rmse <- sqrt(forest_2.sse/nrow(al_wide_imp))

forest_2015.rmse
forest_2.rmse

sses <- cbind(glm_2015.sse, bin_glm_2015.sse, mo1.sse, mo2.sse, mo3.sse, forest_2015.sse,
              forest_2.sse)
sses
RMSEs <- cbind(glm_2015.rmse, bin_glm_2015.RMSE, mo1.rmse, mo2.rmse, mo3.rmse, forest_2015.rmse,
               forest_2.rmse)
RMSEs
