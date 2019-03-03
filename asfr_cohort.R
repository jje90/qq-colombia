#Calculating asfr for cohort 1941-1950 and 1951-1960
#data is already data.table
data_asfr_cohort <- data[, list(YEAR, SAMPLE, SERIAL, SEX, AGE, RELATE, MOMLOC, STEPMOM, PERNUM, CHBORN, CHSURV)]
data_asfr_cohort$Yr_birth <- data_asfr_cohort$YEAR - data_asfr_cohort$AGE

setkey(data_asfr_cohort, SAMPLE, SERIAL)
data_asfr_cohort$personId <- 1:nrow(data_asfr_cohort)

#Age specific graph
women <- subset(data_asfr_cohort, SEX==2 & Yr_birth>=1951 & Yr_birth<=1960 & AGE>=13 & AGE<=17)

totWomen <- women %>%
  summarise(num_women = (sum(SEX, na.rm=T))/2) #denominator: total women in age group
totWomen
#the idea here is to be sure that women between the age group are having new borns in that period,that is that the children is between 0 - 4 years. To do so I think I can substract the age og the children from the age of the mother, and counted as a birth just if is between 0 and 4.
#for each serial=ss, I need to substract the age(AGE) of the children (RELATE==3) from the age of the mother (RELATE==2). This will give me a variable of age at birth.

lowestAge = 13;
binSize = 5;
highestAge = 56;
numBins = (highestAge - lowestAge) / binSize;
motherArray = array(0, dim = numBins)
# for each woman select all people from her household
#lastSerial <- max(data_asfr$SERIAL)
#for (serial in seq(1000, lastSerial, by = 1000))
sampleAndSerial <- data_asfr_cohort[, SAMPLE, SERIAL]
sampleAndSerial <- unique(sampleAndSerial)
numSerials = length(sampleAndSerial)

for (ss in 1:numSerials)
{
  sample <- sampleAndSerial[[ss, "SAMPLE"]];
  serial <- sampleAndSerial[[ss, "SERIAL"]];
  household <- data_asfr_cohort[SERIAL == serial & SAMPLE == sample]
  #household <- data_asfr_cohort[serial]
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
      #data_asfr[id]$motherAgeAtBirth <- motherAgeAtBirth # very sloooow
      bin <- as.integer((motherAgeAtBirth - lowestAge) / binSize + 1)
      motherAgeBin <- as.integer((motherAge - lowestAge) / binSize + 1)
      if(motherAgeBin == bin && bin >= 1 && bin <= numBins){
       motherArray[bin] = motherArray[bin] + 1;
      }
    }
  }
  if(ser %% 1000 == 0)
    printf("Elapsed %d, remaining %d, completed: %.2f%%\n", ser, (numSerials - ser), ser/numSerials * 100)
}
sersfokjopohuftrd
