# read in screening project data
# change file name to later local data file (don't store redcap data set on github)
source("STOMPTBCommunityWide_R_2019-09-30_1156.r")
require(dplyr)

# households are records, with multiple resident events per household record. Look at number and location of households:
households <- subset(data, redcap_event_name=="household_info_arm_1")
length(households$hhid)
length(unique(households$hhid))
households %>% count(hh_parish.factor, hh_village_1.factor, hh_village_2.factor, hh_village_3.factor, hh_village_other)

# copy the relevant household info to the start of each resident row
# matching by record id
# processor can't handle the slick version, so using a loop:
datacopy <- data
residentrows <- data$redcap_event_name %in% paste0("resident",1:20,"_info_arm_1")
for (i in (1:nrow(data))[residentrows])
{
  household <- which(datacopy$record_id==datacopy[i,"record_id"] & datacopy$redcap_event_name=="household_info_arm_1")
  datacopy[i,is.na(data[i,])] <- datacopy[household,is.na(data[i,])]
}
save(datacopy, file="datacopy_20190930.Rdata")

# load("datacopy_20190930.Rdata")

# exclude pilot data
screendata <- datacopy %>% filter(hh_attempt1=="" | 
                                  as.Date(hh_attempt1, format="%Y-%m-%d") > as.Date("2019-01-31", format="%Y-%m-%d")) %>%
  filter(res_interact_date == "" | 
           as.Date(res_interact_date, format="%Y-%m-%d") > as.Date("2019-01-31", format="%Y-%m-%d"))
households <- subset(screendata, redcap_event_name=="household_info_arm_1")

# identify rows of dataset that correspond to a resident within an event record
screendata$residentnumber <- match(screendata$redcap_event_name, paste0("resident",1:20,"_info_arm_1"))
screendata$residentnumber[screendata$redcap_event_name=="individual_info_arm_2"] <- 0 # for people screened away from home, assign a resident number of zero
screendata$residentnumber[screendata$redcap_event_name=="household_info_arm_1"] <- NA # household rows

