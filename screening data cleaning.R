# read in screening project data
# change file name to later local data file (don't store redcap data set on github)
source("STOMPTBCommunityWide_R_2019-09-30_1156.r")

# households are records, with multiple resident events per household record. Look at number and location of households:
require(dplyr)
households <- subset(data, redcap_event_name=="household_info_arm_1")
length(households$hhid)
length(unique(households$hhid))
households %>% count(hh_parish.factor, hh_village_1.factor, hh_village_2.factor, hh_village_3.factor, hh_village_other)

# identify rows of dataset that correspond to a resident within an event record
levels(data$redcap_event_name.factor)
residents <- subset(data, redcap_event_name %in% paste0("resident",1:20,"_info_arm_1")) # allowing for 20, but current data set goes up only to 12
residents$residentnumber <- match(residents$redcap_event_name, paste0("resident",1:20,"_info_arm_1"))

# and separately identify records of people screened away from home, assign them a resident number of zero
aways <- subset(data, redcap_event_name=="individual_info_arm_2")
aways$residentnumber <- 0

# these rows are for a household, not a person:
households$residentnumber <- NA

# summary:
nrow(aways)
nrow(residents)
nrow(households) # fewer than one enrolled resident per household. Let's drop the empty resident rows:
nrow(data)==nrow(aways) + nrow(residents) + nrow(households) #check

# nhouseholds <- do.call("rbind", lapply(1:max(residents$residentnumber), function(i) {households %>% mutate(residentnumber=i)}))
# table(nhouseholds$residentnumber)
# rb <- rbind(nhouseholds, residents, aways) %>% select(-redcap_event_name, -redcap_event_name.factor, -redcap_data_access_group)

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


## cleaner version, but latop memory can't handle it
# coalesce_by_column <- function(df) {
#   return(dplyr::coalesce(!!! as.list(df)))
# }
# cc <- rb %>% group_by(record_id, residentnumber) %>% summarise_all(coalesce_by_column)
# save(cc, file="cc_20190930.Rdata")
