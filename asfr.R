# we expect the variable data (a data.table) to exist and contain all the observations 
# at the end of the script, the tibble `asfr` will contain the age specific fertility rate (for all the women) by census
printf <- function(...) cat(sprintf(...))

year <- 1973
data_asfr <- data[, list(YEAR, SERIAL, SEX, AGE, RELATE, MOMLOC, STEPMOM, PERNUM, CHBORN, CHSURV)]
data_asfr <- data_asfr[YEAR==year]
data_asfr$motherAgeAtBirth <-0;
# Assuming we are operating on one YEAR at a time
setkey(data_asfr, SERIAL)
data_asfr$personId <- 1:nrow(data_asfr)

#Age specific graph
women <- subset(data_asfr, YEAR==year & SEX==2 & AGE>=15 & AGE<=19)

totWomen <- women %>%
  summarise(num_women = (sum(SEX, na.rm=T))/2) #denominator: total women in age group
totWomen
#the idea here is to be sure that women between the age group are having new borns in that period,that is that the children is between 0 - 4 years. To do so I think I can substract the age og the children from the age of the mother, and counted as a birth just if is between 0 and 4.
#for each serial=ss, I need to substract the age(AGE) of the children (RELATE==3) from the age of the mother (RELATE==2). This will give me a variable of age at birth.

lowestAge = 15;
binSize = 5;
highestAge = 55;
numBins = (highestAge - lowestAge) / binSize;
motherArray = array(0, dim = numBins)
# for each woman select all people from her household
#lastSerial <- max(data_asfr$SERIAL)
#for (serial in seq(1000, lastSerial, by = 1000))
serials = unique(data_asfr$SERIAL)
numSerials = length(serials)

for (ser in 1:numSerials)
{
  serial = serials[ser];
  #household <- data_asfr[SERIAL == serial]
  household <- data_asfr[list(serial)]
  if(nrow(household) == 1)
  {
     next;
  }
  mothersInHouse = array(0, dim = nrow(household))
  # household=data_asfr[SERIAL==104000]
  # for each potential child, retrieve the age and store in the motherAgeAtBirth column the value motherAge-age
  for (p in 1:nrow(household))
  {
    person <- household[p];
    if(person$MOMLOC != 0 & person$STEPMOM == 0)
    {
      # this person is a child
      mother <- subset(household, PERNUM == person$MOMLOC);
      if(nrow(mother) != 1)
      { # the mother is not here, maybe because we are using a subset of the whole database?
        next;
      }
      motherAge = mother$AGE;
      childAge <- household[[p, "AGE"]]
      id <- household[p]$personId
      motherAgeAtBirth <- motherAge - childAge
      data_asfr[id]$motherAgeAtBirth <- motherAgeAtBirth # very sloooow
      #bin <- as.integer((motherAgeAtBirth - lowestAge) / binSize + 1)
      #motherAgeBin <- as.integer((motherAge - lowestAge) / binSize + 1)
      #if(motherAgeBin == bin && bin >= 1 && bin <= numBins){
       # motherArray[bin] = motherArray[bin] + 1;
      #}
    }
  }
  if(ser %% 1000 == 0)
    printf("Elapsed %d, remaining %d, completed: %.2f%%\n", ser, (numSerials - ser), ser/numSerials * 100)
}
sersfokjopohuftrd
data_asfr$YearBirth <- data_asfr$YEAR - data_asfr$AGE

write.table(data_asfr, file="data_asfr_73.csv")
motherArray
#1973
# Numerator: 12696 39870 33027 23754 17500  7517  1972   521

#denominator : 

#1985

#Numerator: 16193 54976 45427 26163 14715  5358  1420   424

# denominator:

#1993

#Numerator: 18820 55068 49173 32539 17231  5951  1489   818
#denominator

#2005

#numerator: 29037 60726 47762 33517 21146  8937  2011   966


asfr_1973 <- tibble(
  age = seq(15, 50, by=5),
  numerator = c(2696, 39870, 33027, 23754, 17500, 7517, 1972, 521),
  denominator = c(122424, 94887, 70229, 56805, 53946, 43568, 35856, 36977),
  asfr = numerator/denominator,
  year= "1973"
)

asfr_1985 <- tibble(
  age = seq(15, 50, by=5),
  numerator = c(16193, 54976, 45427, 26163, 14715,  5358,  1420,   424),
  denominator = c( 159038, 150702, 122574, 93139, 81244, 58596, 52723, 58178 ),
  asfr = numerator/denominator,
  year= "1985"
)

asfr_1993 <- tibble(
  age = seq(15, 50, by=5),
  numerator = c(18820, 55068, 49173, 32539, 17231,  5951, 1489, 818),
  denominator = c(164526, 161059, 152653, 136086, 113659, 85746, 66033, 68512),
  asfr = numerator/denominator,
  year= "1993"
)

asfr_2005 <- tibble(
  age = seq(15, 50, by=5),
  numerator = c(29037, 60726, 47762, 33517, 21146,  8937,  2011,   966),
  denominator = c(186191, 167066, 153211, 139201, 137878, 126329, 107327, 103550),
  asfr = numerator/denominator,
  year= "2005"
)

asfr <- bind_rows(asfr_1973, asfr_1985, asfr_1993, asfr_2005)


library(extrafont)
# Install **TTF** Latin Modern Roman fonts from www.fontsquirrel.com/fonts/latin-modern-roman
# Import the newly installed LModern fonts, change the pattern according to the 
# filename of the lmodern ttf files in your fonts folder
font_import(pattern = "lmodern*")

loadfonts(device = "win")
par(family = "LM Roman 10")

ggplot(asfr, aes(x=age, y=asfr, group = year)) +  geom_point(aes(shape=year)) + geom_line(aes(linetype= year))+ scale_x_continuous(breaks=seq(15, 50, by=5), labels= c("15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55")) + xlab("Age") + ylab("ASFR") + theme_bw()  + theme(legend.position="bottom", legend.box = "horizontal", legend.title=element_blank(),
                            panel.border = element_blank(),panel.background = element_blank())
