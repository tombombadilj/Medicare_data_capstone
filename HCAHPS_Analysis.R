rm(list=ls())
library(dplyr)
library(tidyr)
home<-"/Users/Apple/Desktop/Foundation of Data Science/"
path2015 <- "/Users/Apple/Desktop/Foundation of Data Science/Medicare_data/2015/"
HCAHPS2015 <- read.csv(paste(path2015,"HCAHPS - Hospital.csv",sep=""))
HCAHPS2015 <- tbl_df(HCAHPS2015)
dim(HCAHPS2015)
names(HCAHPS2015)
data_list <- HCAHPS2015 %>% group_by(HCAHPS.Measure.ID,HCAHPS.Question) %>%
    select(HCAHPS.Measure.ID,HCAHPS.Question) %>%
    summarise(n=n()) 
View(data_list)
write.csv(data_list,paste(home,"HCAHPS_Questions.csv",sep=""))
View(HCAHPS2015)

HCAHPS2015$Patient.Survey.Star.Rating[HCAHPS2015$Patient.Survey.Star.Rating=="Not Applicable"|
                                          HCAHPS2015$Patient.Survey.Star.Rating=="Not Available"
                                      ]<- NA
unique(HCAHPS2015$Patient.Survey.Star.Rating)

HCAHPS2015$HCAHPS.Answer.Percent[HCAHPS2015$HCAHPS.Answer.Percent=="Not Applicable"|
                                          HCAHPS2015$HCAHPS.Answer.Percent=="Not Available"
                                      ]<- NA
unique(HCAHPS2015$HCAHPS.Answer.Percent)

HCAHPS2015$HCAHPS.Linear.Mean.Value[HCAHPS2015$HCAHPS.Linear.Mean.Value=="Not Applicable"|
                                     HCAHPS2015$HCAHPS.Linear.Mean.Value=="Not Available"
                                 ]<- NA
unique(HCAHPS2015$HCAHPS.Linear.Mean.Value)

unique(HCAHPS2015$HCAHPS.Measure.ID)

graphing <- function(measure) {
    HCAHPS <- HCAHPS2015 %>% filter(HCAHPS.Measure.ID == measure) %>%
        select(Provider.ID,HCAHPS.Measure.ID,Patient.Survey.Star.Rating,HCAHPS.Answer.Percent,
               HCAHPS.Linear.Mean.Value)
    if (grepl(pattern = "_STAR_RATING", measure) == TRUE) {
        HCAHPS1 <- HCAHPS %>% select(Provider.ID,HCAHPS.Measure.ID,Patient.Survey.Star.Rating)
        boxplot(as.numeric(HCAHPS1$Patient.Survey.Star.Rating), horizontal = TRUE, main = measure)
    } else if (grepl(pattern = "_U_P", measure) == TRUE | grepl(pattern = "_A_P", measure) == TRUE | grepl(pattern = "_SN_P", measure) == TRUE){
        HCAHPS1 <- HCAHPS %>% select(Provider.ID,HCAHPS.Measure.ID,HCAHPS.Answer.Percent)
        boxplot(as.numeric(HCAHPS1$HCAHPS.Answer.Percent),horizontal = TRUE, main = measure)
    } else {HCAHPS1 <- HCAHPS %>% select(Provider.ID,HCAHPS.Measure.ID,HCAHPS.Linear.Mean.Value)
    boxplot(as.numeric(HCAHPS1$HCAHPS.Linear.Mean.Value),horizontal = TRUE,main = measure)}
}

graphing("H_CLEAN_STAR_RATING")

