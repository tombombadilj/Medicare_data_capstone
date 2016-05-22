#Loading dataset into R
rm(list=ls())
library(dplyr)
library(tidyr)
library(data.table)
install.packages("gmodels")
library(gmodels)
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)


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
View(x)
unique(HCAHPS2015$`HCAHPS Measure ID`)

#Creating a data dictionary for all the HCAHPS items included in the dataset
data_list <- HCAHPS2015 %>% group_by(`HCAHPS Measure ID`,`HCAHPS Question`) %>%
    select(`HCAHPS Measure ID`,`HCAHPS Question`) %>%
    summarise(n=n()) 
View(data_list)
write.csv(data_list,paste(home,"HCAHPS_Questions.csv",sep=""))
View(HCAHPS2015)

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
View(al_wide)
names(al_wide)
summary(al_wide)

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

al_matrix <- as.matrix(al_corr)
rc <- rcorr(al_matrix,type="pearson")
numbermatrix <- as.data.frame(rc$r)
write.csv(numbermatrix,paste(home,"correlation matrix.csv",sep=""))
#all p value less than zero



#cor(al_dt[,2, with=FALSE],al_dt[,3, with=FALSE],use = 'pairwise.complete.obs')
#cor(al_wide$H_CLEAN_HSP_A_P,al_wide$H_COMP_1_A_P, use='pairwise.complete.obs')
#?cor.test


#for (x in c(2:12)){
#   y <- x+1 
#    while (y<12) {print(x)
#    print(y)
#    print(al_dt[,y,with=FALSE])
#   y<-y+1
#   }}

#for (x in c(2:12)) { x<-as.numeric(x)
#    y <- x + 1 
#    y <- as.numeric(y)
#    while (y<12){print (cbind(x,y))
#        cor.test(al_dt[,x,with=FALSE],al_dt[,y,with=FALSE],alternative ='two.sided',method='pearson')
#        y <- y+1
#    }
#}





#length(names(al_wide))
#cor(al_wide[2:12], use="complete.obs", method = 'pearson')
#al_matrix <- as.matrix(al_wide)
#head(al_matrix)
#al_dt<-data.table(al_wide)
#head(al_wide)
#rc <- rcorr(al_matrix[,2:12],type='pearson')
#print(rc, digits = 20)
#?rcorr()
#?sprintf()

#pc <- rcorr(al_wide$H_COMP_1_A_P,al_wide$H_COMP_2_A_P, type="pearson")
#print(pc, digit=19)
#cor(al_wide$H_COMP_1_A_P,al_wide$H_COMP_2_A_P, use="complete.obs")
#cor.test(al_wide$H_COMP_1_A_P,al_wide$H_COMP_2_A_P, alternative ='two.sided',method='pearson')