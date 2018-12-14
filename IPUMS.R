#require(maps)
#install.packages("mapdata")
require(dplyr)
#require(mapdata)
#library(ggplot2)
#library(ggrepel)
#library(sp)
#install.packages('sf')
#install.packages('rworldmap',dependencies=TRUE) 
library(rgdal)
install.packages("labelled")
install.packages("devtools")
install.packages('ipumsr')
require("ipumsr")
install.packages("viridis", repos="http://cloud.r-project.org")
require(viridis)

getwd()
setwd("D:/IPUMS-I/Calculos y datos")
#Comand to read the files, IPUMS:
ddi <- read_ipums_ddi("ipumsi_00002.xml")
data <- read_ipums_micro(ddi)

#Descriptive stats
descriptive <- subset.data.frame(data, YEAR==1973 & AGE<=100)
summary(descriptive$AGE)
descriptive <- subset.data.frame(data, YEAR==2005 & ELDCH<=50)
summary(descriptive$ELDCH)
descriptive <- subset.data.frame(data, YEAR==2005 & YNGCH<=50)
summary(descriptive$YNGCH)
descriptive <- subset.data.frame(data, YEAR==2005 & CHBORN<=30 & CHBORN>0 & CHSURV>0)
summary(descriptive$CHBORN)
descriptive <- subset.data.frame(data, YEAR==2005 & CHSURV<=30 & CHBORN>0 & CHSURV>0)
summary(descriptive$CHSURV)
descriptive <- subset.data.frame(data, YEAR==2005 & LASTBYR<=2015)
summary(descriptive$LASTBYR)
descriptive <- subset.data.frame(data, YEAR==2005 & YRSCHOOL<=18)
summary(descriptive$YRSCHOOL)

#1		Rural 2		Urban
descriptive <- subset.data.frame(data, YEAR==2005 & AGE<=100 & URBAN==1)
summary(descriptive$AGE)
descriptive <- subset.data.frame(data, YEAR==2005 & ELDCH<=50 & URBAN==1)
summary(descriptive$ELDCH)
descriptive <- subset.data.frame(data, YEAR==2005 & YNGCH<=50 & URBAN==1)
summary(descriptive$YNGCH)
descriptive <- subset.data.frame(data, YEAR==1973 & CHBORN<=30 & URBAN==1 & CHBORN>0 & CHSURV>0)
summary(descriptive$CHBORN)
descriptive <- subset.data.frame(data, YEAR==1973 & CHSURV<=30 & URBAN==1 & CHBORN>0 & CHSURV>0)
summary(descriptive$CHSURV)
descriptive <- subset.data.frame(data, YEAR==2005 & LASTBYR<=2015 & URBAN==2)
summary(descriptive$LASTBYR)
descriptive <- subset.data.frame(data, YEAR==2005 & YRSCHOOL<=18 & URBAN==2)
summary(descriptive$YRSCHOOL)

#Maps data

# We will get labels from shape file for geographic variables
data <- data %>%
  mutate_at(vars(starts_with("GEO")), zap_labels)

data$CHBORN <- zap_labels(data$CHBORN) #Convert CHBORN as numeric
data$AGE <- zap_labels(data$AGE)
#Select year and variable
born <- subset.data.frame(data, YEAR==1985 & CHBORN<=30)

data_summary <- born %>%
  group_by(GEOLEV2) %>%
  summarise(num_born = sum(CHBORN, na.rm=T))

women <- subset.data.frame(data, YEAR==1985 & SEX==2 & AGE<=45 & AGE>=15) #female==2
data_women <- women %>%
  group_by(GEOLEV2) %>%
  summarise(num_women = (sum(SEX, na.rm=T))/2)

bornperwomen <- merge(data_summary, data_women, "row.names", all.x = FALSE, all.y = FALSE)
bornperwomen$bpm <- c(bornperwomen$num_born/bornperwomen$num_women)
write.csv(bornperwomen, file = "mapa1985.csv")

#CHSURV per woman 

data$CHSURV <- zap_labels(data$CHBORN) #Convert CHBORN as numeric

#Select year and variable
surv <- subset.data.frame(data, YEAR==1973 & CHBORN<=30 & CHBORN>0 & CHSURV<=30)

data_surv <- surv %>%
  group_by(GEOLEV2) %>%
  summarise(num_surv = sum(CHSURV, na.rm=T))

women <- subset.data.frame(data, YEAR==1973 & SEX==2 & AGE<=45 & AGE>=15) #female==2
data_women <- women %>%
  group_by(GEOLEV2) %>%
  summarise(num_women = (sum(SEX, na.rm=T))/2)
survperwomen <- merge(data_surv, data_women, "row.names", all.x = FALSE, all.y = FALSE)
survperwomen$spm <- c(survperwomen$num_surv/survperwomen$num_women)
write.csv(survperwomen, file ="mapa2_1993.csv")


#Age specific graph
women <- subset.data.frame(data, YEAR==2005 & SEX==2 & AGE>=45 & AGE<=50)
womenchildren <- subset.data.frame(women, CHBORN>0)
data_women <- womenchildren %>%
summarise(num_women = (sum(SEX, na.rm=T))/2)

data_child <- womenchildren %>%
summarise(num_chil = (sum(CHBORN, na.rm = T)))

chilperwon <- data_child$num_chil/data_women$num_women

#CHBRN AND CHSURV per cohort and stopping
data$YBRN <- data$YEAR-data$AGE
data$agelstbr <- data$LASTBYR-data$YBRN


for(year in c(1973, 1985, 1993, 2008))
{
  print("===============")
  print(paste("year", year))
  n <- 0
  repeat
  {
    n <- n + 1;
    if(n == 1){
      firstyear <- 1850
      lastyear <- 1900
    } else {
      firstyear <- 1901 + (n - 2) * 10
      lastyear <- firstyear + 9
    }
    minage <- year - lastyear
    maxage <- year - firstyear
    if (minage <= 10)
      break
    
    women <- subset.data.frame(data, YEAR==year & SEX==2 & AGE<=maxage & AGE>=minage)
    womenceli <- subset.data.frame(data, YEAR==year & SEX==2 & AGE<=maxage & AGE>=minage & CHBORN==0) #OBSERVE CELIBACY
    womenchildren <- subset.data.frame(women, CHBORN>0 & CHBORN<90 & CHSURV<90)
    
    #CHBRN
    data_women <- womenchildren %>%
      summarise(num_women = (sum(SEX, na.rm=T))/2)
    data_child <- womenchildren %>%
      summarise(num_chil = (sum(CHBORN, na.rm = T)))
    chilperwon <- data_child$num_chil/data_women$num_women #CHBR PER WOMAN
    
    #CHSURV
    data_women <- womenchildren %>%
      summarise(num_women = (sum(SEX, na.rm=T))/2)
    data_surv <- womenchildren %>%
      summarise(num_chilsurv = (sum(CHSURV, na.rm = T)))
    chilsurvperwon <- data_surv$num_chilsurv/data_women$num_women #CHSURV
    
    #STOPPING, NA FOR year
    womenst <- subset.data.frame(data, YEAR==year & SEX==2 & LASTBYR<9998 & AGE<=maxage & AGE>=minage)
    data_women <- womenst %>%
    summarise(age_lstb = (mean(agelstbr)))
    printf <- function(...) cat(sprintf(...))
    printf(
        "%d, %d, %d, %d\n",
        firstyear,
        maxage,
        nrow(women),
        nrow(womenceli)
    )
    print(paste(firstyear, "-", lastyear))
    print(paste(maxage, "-", minage))
    print(paste("women", nrow(women)))
    print(paste("womenceli", nrow(womenceli)))
    print(paste("womenchildren", nrow(womenchildren)))
    print(paste("ratiocelichildren", round(nrow(womenceli) / nrow(womenchildren)*100), "%"))
    print(paste("chilperwon", chilperwon))
    print(paste("chilsruveperwon", chilsurvperwon))
    print(paste("data_women", data_women$age_lstb))
    print("===")
  }
}