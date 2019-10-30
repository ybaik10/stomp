# read in screening project data
# change file name to later local data file (don't store redcap data set on github)
source("STOMPTBCommunityWide_R_2019-10-25_1621.r")
require(dplyr)
require(binom)
require(ggplot2)

# copy the relevant household info to the start of each resident row
# matching by record id
# processor can't handle the slick version, so using a loop:
datacopy <- data
residentrows <- data$redcap_event_name %in% paste0("resident",1:20,"_info_arm_1")
for (i in (1:nrow(data))[residentrows])
{
  household <- which(datacopy$record_id==datacopy[i,"record_id"] & datacopy$redcap_event_name=="household_info_arm_1")
  datacopy[i,is.na(data[i,])] <- datacopy[household,is.na(data[i,])]
  if (i%%1000==0) print(i)
}
save(datacopy, file="datacopy_20191025.Rdata")

# load("datacopy_20191025.Rdata"); require(dplyr); require(binom)

# exclude pilot data
screendata <- datacopy %>% filter(hh_attempt1=="" | 
                                  as.Date(hh_attempt1, format="%Y-%m-%d") > as.Date("2019-01-31", format="%Y-%m-%d")) %>%
  filter(res_interact_date == "" | 
           as.Date(res_interact_date, format="%Y-%m-%d") > as.Date("2019-01-31", format="%Y-%m-%d"))

# identify rows of dataset that correspond to a resident within an event record
screendata$residentnumber <- match(screendata$redcap_event_name, paste0("resident",1:20,"_info_arm_1"))
screendata$residentnumber[screendata$redcap_event_name=="individual_info_arm_2"] <- 0 # for people screened away from home, assign a resident number of zero
screendata$residentnumber[screendata$redcap_event_name=="household_info_arm_1"] <- NA # household rows

screendata$screening_id <- as.character(screendata$screening_id)

# copy resN_age and resN_female to the corresponding individual records
screendata$age <- screendata$res_age
ages <- screendata[,paste0("res",1:10,"_age")]; sexes <- screendata[,paste0("res",1:10,"_female")]
screendata$age[is.na(screendata$age)] <- (ages[cbind(1:nrow(screendata), screendata$residentnumber)])[is.na(screendata$age)]
screendata$female <- screendata$res_female
screendata$female[is.na(screendata$female)] <- (sexes[cbind(1:nrow(screendata), screendata$residentnumber)])[is.na(screendata$female)]
table(screendata$female, screendata$residentnumber, useNA = "ifany")

summary(screendata$age[!is.na(screendata$residentnumber)])
screendata[which(screendata$age>100),c("res_age", "res1_age","res2_age")]
screendata[which(screendata$age==3220),c("age")] <- 32
screendata[which(screendata$res1_age==3220),c("res1_age")] <- 32
screendata[which(screendata$age<15),c("res_age", "res1_age","res2_age")]
screendata[which(screendata$age==2),c("age")] <- NA
screendata[which(screendata$res1_age==2),c("res1_age")] <- NA

screendata$res_noconsent[screendata$res_notes %in% c("REFUSED TO PARTICIPATE.", "REFUSED TO BE SCREENED.","SAYS WAS SCREENED AT MULAGO. REFUSED TO PARTICIPATE")]<-1
screendata$res_prior_stomp[screendata$res_notes %in% c("ALREADY SCREENED AS A-97446", "SHE IS SAID TO HAVE BEEN SCREENED THE THE VBE AT TRANSAM FACTORY")]<-1
screendata$res_prior_stomp[screendata$res_check_iris_id=="699964428959201903006606" & screendata$res_prior_stomp_how___1==1] <-1

table(screendata$xpertscreen_category, screendata$xpertscreen_category.factor)
screendata$xpertscreen_category[screendata$xpertscreen_result=="INVALID"] <- 8


screendata$ontbrx <- NA
screendata$ontbrx[screendata$res_nocall_notes=="REPORTS TO BE ALREADY ON TB MEDICATION AT KISUGU HCIII"] <- "Kisugu"
screendata$ontbrx[screendata$res_nocall_notes=="ON TREATMENT"] <- "Unknown"
screendata$ontbrx[screendata$res_nocall_notes=="ALREADY ON TREATMENT DIAGNOSED FROM ALIVE MEDICAL SERVICES THUS PREFERS NOT TO SCREEN"] <- "Alive"
