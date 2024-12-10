### Generate encounter histories
### Elizabeth Simpson & David Messmer
### Started: 2023-June-13
### Updated: 2024-Aug-23

# *** Set working directory to the /data folder!

library(tidyverse)
library(lubridate)

### LOAD DATA
# Hen individuals (1 row/hen)
individuals <- read.csv("HenIndvTbl.csv", as.is=TRUE)

# Use the midpoint method to calculate the estimated fate date (midway between LastHeardDate and (Known) FateDate)
hen.dates.EH <- individuals %>% 
  mutate(FateDate = ymd(as.Date(FateDate)),
         EstFateDate = as.Date(as.duration(interval(FateDate, LastHeardDate))/2 + FateDate))

hen.dates.EH <- hen.dates.EH[,c("HenID", "Status", "Age", "Fate", "CaptureDate", "EstFateDate")]

### ENCOUNTER HISTORY NOTES
# 2011 is the [initial] capture cohort, 2020 is the [final] recapture cohort.
# At first capture, a hen must be encountered during the first two months of the the sage-grouse year (Mar. & Apr./ 3 & 4) to be included in her first interval
# Then, she is alive for all subsequent intervals until the interval where her fate is determined (EstFateDate)

# Separate years and months from CaptureDate and EstFateDate
# Calculate the sage-grouse year (bio_yr), i.e., Jan. and Feb. (1 and 2)included in the prior year. 
bioyr.hens <- hen.dates.EH %>%
  mutate(cap_year = year(CaptureDate),
         cap_mo = month(CaptureDate),
         fate_year = year(EstFateDate),
         fate_mo = month(EstFateDate),
         cap_bio_yr = ifelse(cap_mo %in% 1:2, cap_year - 1, cap_year),
         fate_bio_yr = ifelse(fate_mo %in% 1:2, fate_year - 1, fate_year))

bioyr.hens <- bioyr.hens[,c("HenID", "Status", "Age", "cap_bio_yr", "cap_mo", "fate_bio_yr", "fate_mo")]

# Generate EH 'slots' (10 years x 447 hens = 4470)
years <- unique(bioyr.hens$cap_bio_yr)
possible.EHs <- expand.grid(years, bioyr.hens$HenID)
colnames(possible.EHs) <- c("year", "HenID")
EHs <- merge(possible.EHs, bioyr.hens, by.x="HenID", by.y="HenID")

# Determine encounter history for each year
for(i in seq_len(nrow(EHs))){
  if(((EHs$Status[i]=="censored" | EHs$Status[i]=="dropped collar") & EHs$year[i] == EHs$fate_bio_yr[i]) |
     (EHs$year[i] < EHs$cap_bio_yr[i] | EHs$year[i] > EHs$fate_bio_yr[i]) |
     ((EHs$year[i] == EHs$cap_bio_yr[i]) & (!EHs$cap_mo[i]==3 & !EHs$cap_mo[i]==4)) |
     ((EHs$cap_bio_yr[i] == EHs$fate_bio_yr[i]) & (!EHs$cap_mo[i]==3 & !EHs$cap_mo[i]==4))){
    EHs$alive_at_start[i] <- 0
    EHs$alive_at_end[i] <- 0
  } else if((EHs$year[i] > EHs$cap_bio_yr[i] & EHs$year[i] < EHs$fate_bio_yr[i]) | (EHs$year[i] == EHs$cap_bio_yr[i] & (EHs$cap_mo[i]==3 | EHs$cap_mo[i]==4) & EHs$fate_bio_yr[i] > EHs$cap_bio_yr[i])){
    EHs$alive_at_start[i] <- 1
    EHs$alive_at_end[i] <- 0
  } else if((EHs$year[i] == EHs$cap_bio_yr[i] & (EHs$cap_mo[i]==3 | EHs$cap_mo[i]==4) & EHs$fate_bio_yr[i] == EHs$cap_bio_yr[i]) | (EHs$Status[i]=="mortality" & EHs$year[i]==EHs$fate_bio_yr[i])){
    EHs$alive_at_start[i] <- 1
    EHs$alive_at_end[i] <- 1
  }
}

# Long format known fate combinations
EHs.long <- EHs %>%
  mutate(LD= case_when(
      alive_at_start==1 & alive_at_end==0 ~ '10',
      alive_at_start==1 & alive_at_end==1 ~ '11',
      alive_at_start==0 & alive_at_end==0 ~ '00'
  )
)

# Convert to wide
EHs.sub <- EHs.long[,c("HenID", "year", "LD" )]
EHs.wide <- as.data.frame(EHs.sub %>%
  pivot_wider(names_from=year, values_from = LD))
for(i in seq_len(nrow(EHs.wide))){
  EHs.wide$ch[i] <- paste(EHs.wide[i,2:11], collapse = "")
}

# Add in age (yearling vs. adult) classification
EHs.wide.age <- merge(EHs.wide, hen.dates.EH[,c(1,3)], by.x="HenID", by.y="HenID")
EHs.wide.age <- EHs.wide.age[!EHs.wide.age$Age=="",]
EHs.wide.age<- EHs.wide.age %>% 
  mutate(age=as.numeric(ifelse(Age=='juvenile',0,1)))

rmark.EH.age.ID <- EHs.wide.age %>% select(HenID,ch,age) %>%
  mutate(freq=1,
         ch=str_remove_all(ch, ' ')) %>%
  filter(ch!='00000000000000000000') 