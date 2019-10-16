source("screening data cleaning.R") # takes a while to run, may want to load saved files instead

# summary of household resident numbers screened, with 0 for away from homes and NA for households:
table(screendata$residentnumber, useNA = "ifany")

# describe househod data collection process
sum(screendata$redcap_event_name=="household_info_arm_1") # number of households enumerated during door to door screening
households %>% count(hh_contact.factor) # about half of households were enumerated but no contact made
mean(households$hh_contact, na.rm=T)
households %>% filter(hh_contact.factor=="No") %>% count(hh_neighbors.factor)
# of those with no contact, we got info from neighbors for half, either about who the residents were (45%) or that the house was vacant (5%)
households %>% count(hh_contact.factor, hh_neighbors.factor, hh_refusal.factor) # note here that refusal of "yes" means they didn't refuse
# At about 10% of all houses visited (20% of those with someone home), someone was home but wouldn't speak with us
households %>% filter(hh_contact.factor=="Yes") %>% summarize(mean(hh_refusal.factor=="Yes", na.rm=T)) # 79% of households with someone home agreed to speak with us
households %>% count(no_attempts) # we've visited very few houses more than once (probaly those where we were doing contact investigation or control selection?)
hist(as.Date(households$hh_attempt1, format="%Y-%m-%d"), breaks = 10)

# describe all people reported to live in households
# first exclude vacant households:
households <- households %>% filter(is.na(hh_neighbors.factor) | !hh_neighbors.factor=="NA, house is reported to be vacant")
# so we can characterize
sum(households$hh_neighbors==1 | households$hh_refusal==1, na.rm=T)
# out of ...
nrow(households) # ... potentially occupied houses. 
sum(households$hh_neighbors==1 | households$hh_refusal==1, na.rm=T)/nrow(households) # that's 63%.
# Of these with data, we're missing a resident number for <0.1% of households, andchild number for <0.2%:
households %>% filter(hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_child)), median(hh_child, na.rm=T), quantile(hh_child, 0.1, na.rm=T), quantile(hh_child, 0.9, na.rm=T))
mean(households$hh_child>1, na.rm=T) # 69% of households have no children under age 15
households %>% filter(hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_res_no)), median(hh_res_no, na.rm=T), quantile(hh_res_no, 0.1, na.rm=T), quantile(hh_res_no, 0.9, na.rm=T))
households %>% filter(hh_refusal==1 | hh_neighbors==1) %>% summarise(mean(is.na(hh_child)), median(hh_child + hh_res_no, na.rm=T), quantile(hh_child + hh_res_no, 0.1, na.rm=T), quantile(hh_child + hh_res_no, 0.9, na.rm=T))
# households hae median 2 (80% 1-4) adults, and median 3 (1-6) total residents. 

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

# number of households providing detailed info about their residents:
sum(!is.na(households$res1_female)) # but it's possible we're missing info for resident 1
sum(!is.na(households$res1_female)| !is.na(households$res2_female) | !is.na(households$res3_female) | 
      !is.na(households$res1_head_yn) | !is.na(households$res1_age))
detailed <- subset(households, !is.na(res1_female)| !is.na(res2_female) | !is.na(res3_female) | 
                                  !is.na(res1_head_yn) | !is.na(res1_age))
nrow(detailed)
summary(detailed$hh_res_no)

# demographics of the reported adult residents (using household members info):
members <- lapply(X=1:10, FUN=function(x) cbind(households$hhid, households[,startsWith(colnames(households), paste0("res",x,"_"))]))
members[[1]] <- members[[1]][ , !(names(members[[1]]) %in% c("res1_head_yn","res1_head_yn.factor"))]
members <- lapply(members, setNames, colnames(members[[1]])) # each list element is a resident number, using 1 names for each so we can bind them together next
members <- do.call(rbind, members) # combine into a single dataframe, length 10*nrow(households)
members <- members %>% filter(!is.na(res1_female)| !is.na(res1_age) | !is.na(res1_status)) # drop the emply rows
nrow(members) 
nrow(members)/nrow(detailed) # adult residents per included household matches above as it should

mean(members$res1_female, na.rm=T) # those we enumerated are just over half female
table(members$res1_female, members$res1_age>30, useNA = "ifany") # breakdown by age (>/< 30) and sex. 

# those we screened at home versus other households residents
# i'm not going to match up the members info with the individual resident info right now, but we could try
screendata %>% filter(residentnumber>0) %>% count(res_cont_yn.factor, res_cont_yn)
seenathome <- screendata %>% filter(residentnumber>0 & res_cont_yn==1)
seenathome %>% count(res_lang.factor, res_ineligible1___1.factor) # 1 excluded for language
seenathome %>% count(res_prior_stomp.factor, res_ineligible2___1.factor, res_prior_stomp_how___1, res_prior_stomp_how___2, res_prior_stomp_how___3, res_prior_stomp_how___4, res_prior_stomp_how___9)
# 9 excluded due to prior participation.
# several hundred more reported prior participation, mostly with no particular role checked and no details typed - maybe because issues with our forms, could look at date ranges for this. of those checked but not verified, >100 screening, 1 case, 1 contact. 
seenathome %>% filter(res_prior_stomp==1 & res_prior_stomp_how___1==0, res_prior_stomp_how___2==0, res_prior_stomp_how___3==0, res_prior_stomp_how___4==0) %>% select(res_prior_stomp_where)
seenathome %>% filter(res_prior_stomp==1 & res_prior_stomp_how___1==1) %>% select(res_prior_stomp_where)
eligible <- seenathome %>% filter(res_ineligible1___1==0 & res_ineligible2___1==0)
nrow(seenathome)
nrow(eligible)
eligible %>% count(res_parishid) # other zone and village fields are for non-hh screening only
eligible %>% count(is.na(res_staff_id), screening_id=="") # there are >400 with no screening ID and/or RA name. Why? 
eligible %>% filter(is.na(res_staff_id) | screening_id=="") %>% select(res_parishid) # many have parish ids of 5 (but not all). Were these pilot data? 
eligible %>% filter(is.na(res_staff_id) | screening_id=="") %>% select(res_interact_date, res_attempt, res_prior_stomp, res_prior_stomp_how___1, res_prior_otherconf, res_parishid, res_check_iris_id) # all are from outside study area (why are they here at all?) or report prior stomp participation, often in community screening
prior <- seenathome %>% filter(res_prior_stomp==1)
### we should look at these (truncated) iris ids and see whether they really match another participant, although we haven't collected them consistently since can't check in real teime (should have gathered them all anyway, if we weren't going to enroll thse people) ###
options("scipen" = 30)
prior$res_check_iris_id
prior %>% select(res_check_iris_id, res_irisid) # very few have their own iris id, so if we find them, they're right that they were screened before
prior$matchid <- prior$matchid2 <- NA
for (i in 1:nrow(prior))
{ n <- which(floor(prior$res_check_iris_id[i]/1e12) == floor(screendata$res_irisid/1e12))
  if (length(n)>0)   
    prior$matchid[i] <- screendata$screening_id[n[1]]
  if (length(n)>1)   
    prior$matchid2[i] <- screendata$screening_id[n[2]]
}
prior %>% filter(res_check_iris_id != "") %>% summarize(mean(!is.na(matchid)))
# found matches in our screening data for 65% of those who reported it and provided an iris scan. would be interesting to look at who these people are who were screened 2 (or more) times. And should make sure their demographics match. 

trueeligible <- eligible %>% filter(res_parishid %in% 1:3 & res_prior_stomp != 1)
# 