#require(maps)
#install.packages("mapdata")
require(dplyr)
#require(mapdata)
library(ggplot2)
#library(ggrepel)
#library(sp)
#install.packages('sf')
#install.packages('rworldmap',dependencies=TRUE) 
library(rgdal)
#install.packages("labelled")
#install.packages("devtools")
#install.packages('ipumsr')
require("ipumsr")
#install.packages("viridis", repos="http://cloud.r-project.org")
require(viridis)
library(data.table)
#install.packages("tidyverse")
library(tidyverse)
getwd()
setwd("E:/IPUMS-I/Calculos y datos")

# Only read the database file if there is no variable with the name.
# This is far from being 100% foolproof, but I guess it will save time most of the times.
# Some other times it will just make you go crazy
if(!exists("data") && !exists("ddi"))
{
  #Comand to read the files, IPUMS:
  ddi <- read_ipums_ddi("ipumsi_00006.xml")
  data <- read_ipums_micro(ddi)
}
data <- as.data.table(data)


#Descriptive stats
descriptive <- subset.data.frame(data, YEAR==1973 & AGE<=100)
summary(descriptive$AGE)
descriptive <- subset.data.frame(data, YEAR==2005)
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

################################################################
data <- as.data.table(data)
source("asfr.R")



#CHBRN
data_women <- women %>%
  summarise(num_women = (sum(SEX, na.rm=T))/2)
data_child <- womenchildren %>%
  summarise(num_chil = (sum(CHBORN, na.rm = T)))
chilperwon <- data_child$num_chil/data_women$num_women #CHBR PER WOMAN


#CHBRN AND CHSURV per cohort and stopping
data$YBRN <- data$YEAR-data$AGE
data$agelstbr <- data$LASTBYR-data$YBRN

if(0)
for(year in c(1973, 1985, 1993, 2005))
{
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
      lastyear <- firstyear + 4
    }
    minage <- year - lastyear
    maxage <- year - firstyear
    if (minage <= 10)
      break
  }
}
    
    women <- subset.data.frame(data, YEAR==year & SEX==2 & AGE<=maxage & AGE>=minage)
    #womenceli <- subset.data.frame(data, YEAR==year & SEX==2 & AGE<=maxage & AGE>=minage & CHBORN==0) #OBSERVE CELIBACY
    womenchildren <- subset.data.frame(women, CHBORN>0 & CHBORN<90 & CHSURV<90)
    
    #CHBRN
    data_women <- women %>%
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
    womenst <- subset.data.frame(data, YEAR==year & SEX==2 & LASTBYR<2100 & AGE<=maxage & AGE>=minage & LASTBYR != 2000)
    data_women <- womenst %>%
    summarise(age_lstb = (mean(agelstbr)))

    if(1){
      printf <- function(...) cat(sprintf(...))
      printf(
          "%d-%d, %d-%d, %d, %d, %d, %f, %f, %f \n",
          firstyear, lastyear,
          maxage, minage,
          nrow(women),
          nrow(womenceli),
          nrow(womenchildren),
          chilperwon,
          chilsurvperwon,
          data_women$age_lstb
      )
    } else {
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
  


#####################################################################
#Number of twins in samples
#####################################################################
    
    printf <- function(...) cat(sprintf(...))    
#KEEP ONE YEAR. LET'S START WITH SAMPLE=170197301 FOR 1973
data <- as.data.table(data)
data <- data[YEAR==1973]
dataTable <- as.data.table(data)

#LET'S KEEP JUST PEOPLE IDENTIFIED AS CHILDREN RELATE==3
dataTable <- dataTable[RELATE==3]

#LET'S START WITH HOUSEHOLDS WITH JUST MARRIED/COHAB COUPLE WITH CHILDREN HHTYPE==3 (450164 obs)
dataTable <- dataTable[HHTYPE==3]

setkey(dataTable, SAMPLE, SERIAL)
#we need a variable of three columns: sample, serial, and our result.

sampleAndSerial <- dataTable[, SAMPLE, SERIAL] # should maybe use keep.rownames = TRUE?
setkey(sampleAndSerial, SAMPLE, SERIAL)
#Each sample/serial row should be unique
sampleAndSerial <- unique(sampleAndSerial) #111643 households
sampleAndSerial$hasTwins = NA

for (ss in 1:nrow(sampleAndSerial))
{
  sample <- sampleAndSerial[[ss, "SAMPLE"]];
  serial <- sampleAndSerial[[ss, "SERIAL"]];
  # retrieve from data the AGES column for all the rows that have SAMPLE=sample and SERIAL=serial
    # VERY SLOW: ages <- subset.data.frame(data, SAMPLE==sample & SERIAL==serial)["AGE"]
  #VERY FAST:
  if(1){ # allegedly faster than the else, but it's not true
    ages <- dataTable[list(sample, serial)]$AGE
  } else {
    ages <- dataTable[SAMPLE==sample & SERIAL==serial]$AGE
  }
  if(1)
  {#
    if(length(unique(ages)) != length(ages))
    {
      areTwins = TRUE;
    } else {
      areTwins = FALSE;
    }
  } else {#more complicated, possibly slower: 4400 rows per minute
    areTwins = FALSE
    if(nrow(ages) > 1)
    {
      # sort the ages 
      ages %>% arrange(AGE)
      # variable ages contains all the ages of a given household, sorted
      for (n in 2:nrow(ages))
      {
        age <- ages[[n, "AGE"]]
        prevAge <- ages[[n-1, "AGE"]]

        if(age == prevAge)
        {
          areTwins = TRUE
          break
        }
      }
    }
  }
  sampleAndSerial[[ss, "hasTwins"]] <- areTwins
  if(ss%%1000 == 0)
    printf("elapsed: %d\n", ss)
}


###################################################
# Controlling by place of birth
###################################################


newSampleAndSerial = sampleAndSerial;
noTwinCount = 0;
twinCount = 0;
for(ss in 1:nrow(newSampleAndSerial))
{
  printf("%d\n", ss)
  hasTwins <- newSampleAndSerial[[ss, "hasTwins"]];
  if(!hasTwins)
  {
    next;
  }
  serial <- newSampleAndSerial[[ss, "SERIAL"]];
  sample <- newSampleAndSerial[[ss, "SAMPLE"]];
  rows <- dataTable[SAMPLE==sample & SERIAL==serial];
  # sort the ages 
  rows %>% arrange(AGE,desc(AGE))
  # variable ages contains all the ages of a given household, sorted
  areTwins = FALSE;
  for (n in 2:nrow(rows))
  {
    #age <- rows[[n, "AGE"]]
    #prevAge <- rows[[n-1, "AGE"]]
    placebirth <-rows[[n, "BPLCO2"]]
    prevplacebirth <- rows[[n-1, "BPLCO2"]]
    if(placebirth == prevplacebirth)
    {
      areTwins = TRUE;
      break
    }
  }
  if(areTwins) {
    twinCount = twinCount + 1;
  } else {
    noTwinCount = noTwinCount + 1;
    printf("noTwin: %d, twin: %d\n", noTwinCount, twinCount);
  }
  newSampleAndSerial[[ss, "hasTwins"]] = areTwins;
}
familiesWithTwins = newSampleAndSerial[newSampleAndSerial$hasTwins];
ghjvghjghjghj


################################################################################
# I'M CHECKING WHY I HAVE A DIFFERENCE OF 1765 WHEN CONTROLLING BY PLACE OF BIRTH
################################################################################

colnames(sampleAndSerial) <- c("SERIAL", "SAMPLE", "hasTwins_original")
comparison <- merge(sampleAndSerial, familiesWithTwins, all=T)
comparison <- comparison[hasTwins_original==TRUE & is.na(hasTwins)]
#I have 71 families with children with the same age, but different place of birth.

##############################################################################
#   controlling by multiple births>2 children // selecting just twins
############################################################################


#########################################################################
#     CREATE A DATA.TABLE WITH THE FAMILIES AND THE IDENTIFIER
#########################################################################
twins_indi <- data[, list(SAMPLE, SERIAL, FAMSIZE, AGE, AGE2, RELATE, SEX, ELECTRIC, WATSUP, URBAN, YRSCHOOL_MOM, YRSCHOOL_HEAD)] # when you have more than 2 variables use list!
twins_indi <-merge(twins_indi, newSampleAndSerial, by.x="SERIAL", by.y="SERIAL", all=F)
twins_indi$YRSCHOOL_MOM[twins_indi$YRSCHOOL_MOM>90] <- NA
twins_indi$YRSCHOOL_HEAD[twins_indi$YRSCHOOL_HEAD>90] <- NA

######################################################################  
#   Comparing family size with and wo twins

twins_indi_aver <- twins_indi %>%
  group_by(hasTwins) %>%
  summarise(avg_famsize = (mean(FAMSIZE)))

twins_indi_sd <- twins_indi %>%
  group_by(hasTwins) %>%
  summarise(avg_famsize = (sd(FAMSIZE)))

x <- twins_indi[hasTwins==TRUE]
x <- x[, FAMSIZE]
y <- twins_indi[hasTwins==FALSE]
y <- y[,FAMSIZE]
t.test(x,y)

m <- twins_indi[hasTwins==TRUE]
m <- m[, YRSCHOOL_HEAD]
n <- twins_indi[hasTwins==FALSE]
n <- n[,YRSCHOOL_HEAD]
t.test(m,n)
#COMPARING OTHER VARIBALES


dad_edu <- twins_indi %>%
  group_by(hasTwins) %>%
  summarise(avg_mom_edu = (mean(YRSCHOOL_HEAD, na.rm=T)))
  
  twins_indi_sd_d <- twins_indi %>%
  group_by(hasTwins) %>%
  summarise(avg_mom_edu = (sd(YRSCHOOL_HEAD, na.rm=T)))

  

######################################################################
#         AGE HEAPING GRAPH
#####################################################################
year=1973
  data_heaping <- data[YEAR==year & SEX==1] 
age_people_m <- data_heaping %>%
  count(AGE) %>%
  rename(age_pm=n)
age_people_m$AGE <- as.numeric(age_people_m$AGE)
age_people_m <- as.data.frame(age_people_m)
summary(age_people_m)
age_people_m$SEX <- "Masculine"

data_heaping <- data[YEAR==year & SEX==2] 
age_people_f <- data_heaping %>%
  count(AGE) %>%
  rename(age_pf=n)
age_people_f$AGE <- as.numeric(age_people_f$AGE)
age_people_f <- as.data.frame(age_people_f)
summary(age_people_f)
age_people_f$SEX <- "Femenine"


age_people <- rbind(age_people_f, age_people_m)

ggplot(age_people, x=AGE, y=age_p, group = SEX) + geom_point(aes(x=AGE, y=age_p, shape = SEX))+ geom_line(aes(x=AGE, y=age_p,linetype = SEX))+ xlab("Age") + ylab("Number of people") +
  scale_x_continuous(breaks=seq(from=0, to=100, by=5)) +
  theme_bw()  + theme(panel.background = element_blank()) +theme(legend.position="bottom", legend.box = "horizontal",legend.title=element_blank(),
                                                                 panel.border = element_blank(),panel.background = element_blank())
########################################
#   SEX RATIO
######################################
sex_ratio <- merge(age_people_f, age_people_m, by =c("AGE", "AGE"))
sex_ratio$sex_ratio <- sex_ratio$age_pm/sex_ratio$age_pf

ggplot(sex_ratio, x=AGE, y=sex_ratio) + geom_point(aes(x=AGE, y=sex_ratio))+ geom_line(aes(x=AGE, y=sex_ratio))+ xlab("Age") + ylab("Ratio of males to females") +
  scale_x_continuous(breaks=seq(from=0, to=100, by=5)) +
  theme_bw()  + theme(panel.background = element_blank()) 
#########################################################################
# Whipple_Index & ABCC
#######################################################################  

numerator <- twins_indi[AGE==20 | AGE==25 | AGE==30 | AGE==35 | AGE==40 | AGE==45 | AGE==50 | AGE==55 | AGE==60 | AGE==65 | AGE==70 | AGE==75 | AGE==80 | AGE==85 | AGE==90 | AGE==95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
numerator$age_p <- numerator$age_p/numerator$AGE

denominator <- twins_indi[AGE>20 & AGE!=25 & AGE!=30 & AGE!=35 & AGE!=40 & AGE!=45 & AGE!=50 & AGE!=55 & AGE!=60 & AGE!=65 & AGE!=70 & AGE!=75 & AGE!=80 & AGE!=85 & AGE!=90 & AGE!=95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
denominator$age_p <- denominator$age_p/denominator$AGE


Whipple_Index <- ((sum(numerator$age_p))/((1/5)*sum(denominator$age_p)))*100 
ABCC <- (1-((Whipple_Index-100)/400))*100


#female heaping SEX==2
twins_indi_f <- twins_indi[SEX==2]
numerator_f <- twins_indi_f[AGE==20 | AGE==25 | AGE==30 | AGE==35 | AGE==40 | AGE==45 | AGE==50 | AGE==55 | AGE==60 | AGE==65 | AGE==70 | AGE==75 | AGE==80 | AGE==85 | AGE==90 | AGE==95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
numerator_f$age_p <- numerator_f$age_p/numerator_f$AGE

denominator_f <- twins_indi_f[AGE>20 & AGE!=25 & AGE!=30 & AGE!=35 & AGE!=40 & AGE!=45 & AGE!=50 & AGE!=55 & AGE!=60 & AGE!=65 & AGE!=70 & AGE!=75 & AGE!=80 & AGE!=85 & AGE!=90 & AGE!=95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
denominator_f$age_p <- denominator_f$age_p/denominator_f$AGE


Whipple_Index_f <- ((sum(numerator_f$age_p))/((1/5)*sum(denominator_f$age_p)))*100
ABCC_f <- (1-((Whipple_Index_f-100)/400))*100

#Male heaping SEX==1

twins_indi_m <- twins_indi[SEX==1]
numerator_m <- twins_indi_m[AGE==20 | AGE==25 | AGE==30 | AGE==35 | AGE==40 | AGE==45 | AGE==50 | AGE==55 | AGE==60 | AGE==65 | AGE==70 | AGE==75 | AGE==80 | AGE==85 | AGE==90 | AGE==95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
numerator_m$age_p <- numerator_m$age_p/numerator_m$AGE

denominator_m <- twins_indi_m[AGE>20 & AGE!=25 & AGE!=30 & AGE!=35 & AGE!=40 & AGE!=45 & AGE!=50 & AGE!=55 & AGE!=60 & AGE!=65 & AGE!=70 & AGE!=75 & AGE!=80 & AGE!=85 & AGE!=90 & AGE!=95 ] %>%
  group_by(AGE) %>%
  summarise(age_p = (sum(AGE)))
denominator_m$age_p <- denominator_m$age_p/denominator_m$AGE


Whipple_Index_m <- ((sum(numerator_m$age_p))/((1/5)*sum(denominator_m$age_p)))*100
ABCC_m <- (1-((Whipple_Index_m-100)/400))*100



