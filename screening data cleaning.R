# read in screening project data
# change file name to later local data file (don't store redcap data set on github)
source("STOMPTBCommunityWide_R_2019-09-30_1156.r")

# households are records, with multiple resident events per household record. Look at number and location of households:
require(dplyr)
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

# identify rows of dataset that correspond to a resident within an event record
datacopy$residentnumber <- match(datacopy$redcap_event_name, paste0("resident",1:20,"_info_arm_1"))
datacopy$residentnumber[datacopy$redcap_event_name=="individual_info_arm_2"] <- 0 # for people screened away from home, assign a resident number of zero
datacopy$residentnumber[datacopy$redcap_event_name=="household_info_arm_1"] <- NA # household rows

# summary of household resident numbers screened, with 0 for away from homes:
table(datacopy$residentnumber, useNA = "ifany")

# describe all people reported to live in households
sum(datacopy$redcap_event_name=="household_info_arm_1") # number of households
nrow(households) #matches as it should
households %>% count(hh_contact.factor, hh_members_info_complete.factor) # about half of households enumerated but no contact made
households %>% filter(hh_members_info_complete==0) %>% count(hh_contact.factor, hh_neighbors.factor, hh_refusal.factor) # note here that refusal of "yes" means they didn't refuse
  # of those with incomplete info, most weren't home, and for half of those we got some info from neighbors. At about 1400 households (10%), someone was home but wouldn't speak with us
households$hh_res_no[households$hh_neighbors.factor=="No, house is reported to be vacant"] <- 0
households$hh_child[households$hh_neighbors.factor=="No, house is reported to be vacant"] <- 0
households %>% filter(hh_members_info_complete==1 | hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_res_no)), median(hh_res_no, na.rm=T), quantile(hh_res_no, 0.1, na.rm=T), quantile(hh_res_no, 0.9, na.rm=T))
 # missing resident number for <0.1% of households
households %>% filter(hh_members_info_complete==1 | hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_child)), median(hh_child, na.rm=T), quantile(hh_child, 0.1, na.rm=T), quantile(hh_child, 0.9, na.rm=T))
 # most households have no children under age 15
households %>% filter(hh_members_info_complete==1 | hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_child)), median(hh_child + hh_res_no, na.rm=T), quantile(hh_child + hh_res_no, 0.1, na.rm=T), quantile(hh_child + hh_res_no, 0.9, na.rm=T))
 # total median household size is 3 (middle 80% are 1-6 people)

# total number of residents in the households we got a resident estimate from:
sum(households$hh_res_no, na.rm=T)
sum(households$hh_child, na.rm=T)
sum(households$hh_res_no, na.rm=T) + sum(households$hh_child, na.rm=T)
# fraction of households that contributed to this total:
mean(!is.na(households$hh_res_no))
mean(!is.na(households$hh_child))
mean(!is.na(households$hh_res_no) | !is.na(households$hh_child))
# if we assume the households we missed are the same size as those we counted, then the total population of the areas screened is:
(sum(households$hh_res_no, na.rm=T) + sum(households$hh_child, na.rm=T))/mean(!is.na(households$hh_res_no) | !is.na(households$hh_child))
# and the >15y population is:
(sum(households$hh_res_no, na.rm=T) )/mean(!is.na(households$hh_res_no))

# demographics of the reported residents (using household members info):
members <- lapply(X=1:10, FUN=function(x) cbind(households$hhid, households[,startsWith(colnames(households), paste0("res",x,"_"))]))
members[[1]] <- members[[1]][ , !(names(members[[1]]) %in% c("res1_head_yn","res1_head_yn.factor"))]
members <- lapply(members, setNames, colnames(members[[1]]))
members <- do.call(rbind, members)
mean(members$res1_female, na.rm=T) # those we enumerated are just over half female
table(members$res1_female, members$res1_age>30, useNA = "ifany") # breakdown by age (>/< 30) and sex. Need to check whether NAs are e.g. from the 10th resident that didn't exist, or missing data on people who exist
