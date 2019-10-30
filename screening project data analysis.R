source("screening data cleaning.R") # takes a while to run, may want to load saved files instead

# summary of household resident numbers screened, with 0 for away from homes and NA for households:
table(screendata$residentnumber, useNA = "ifany")

# describe househod data collection process
sum(screendata$redcap_event_name=="household_info_arm_1") # number of households enumerated during door to door screening
households <- subset(screendata, redcap_event_name=="household_info_arm_1")
households %>% count(hh_contact.factor) # about half of households were enumerated but no contact made
mean(households$hh_contact, na.rm=T)
households %>% filter(hh_contact.factor=="No") %>% count(hh_neighbors.factor)
# of those with no contact, we got info from neighbors for half, either about who the residents were (45%) or that the house was vacant (5%)
households %>% count(hh_contact.factor, hh_neighbors.factor, hh_refusal.factor) # note here that refusal of "yes" means they didn't refuse
# At about 10% of all houses visited (20% of those with someone home), someone was home but wouldn't/couldn't speak with us
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
# proportion with resident info data:
(sum(households$hh_res_no, na.rm=T))/((sum(households$hh_res_no, na.rm=T) )/mean(!is.na(households$hh_res_no)))


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
summary(members$res1_age)

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
matched <- merge(x=prior, y=screendata, by.x = "matchid", by.y="screening_id", all = F)
matched2 <- merge(x=prior, y=screendata, by.x = "matchid2", by.y="screening_id", all = F)
cbind(matched$age.x, matched$age.y)
cbind(matched2$age.x, matched2$age.y)
cbind(matched$female.x, matched$female.y)
prior %>% filter(res_check_iris_id != "") %>% summarize(mean(!is.na(matchid)))
# found matches in our screening data for 65% of those who reported it and provided an iris scan. confirmed here that their demographics match. 

# Most who reported prior participation were excluded from rescreening, even if prior participation couldn't be confirmed etc
trueeligible <- eligible %>% filter(res_parishid %in% 1:3 & !(res_prior_stomp == 1))
nrow(trueeligible)
nrow(prior)

# compare groups. dummary variables may be useful
screendata$studyarea <- ifelse(screendata$res_parishid %in% 1:3, 1, 0)
screendata$seenathome <- ifelse(screendata$residentnumber>0 & screendata$studyarea, 
                                ifelse(screendata$res_cont_yn==1, 1, 0), NA)
screendata$ineligible <- ifelse(screendata$seenathome==1, 
                                ifelse(screendata$res_ineligible1___1==1 | screendata$res_ineligible2___1==1, 1, 0), NA)
screendata$prior <- ifelse(screendata$seenathome==1,
                           ifelse(screendata$res_prior_stomp==1, 1, 0), NA)
screendata$noconsent <- ifelse(screendata$seenathome==1,
                           ifelse(screendata$res_noconsent==1, 1, 0), NA)
screendata$refused <- screendata$noconsent & (screendata$ineligible | screendata$prior)%in%c(0,NA)
screendata$excluded <- screendata$ineligible | screendata$prior | screendata$refused
screendata$screened <- ifelse(screendata$seenathome & (!screendata$excluded | is.na(screendata$excluded)), ifelse(screendata$screening_id != "", 1, 0), NA)
table(screendata$excluded, screendata$screened, useNA = 'ifany') # 4 eligible but not assigned a screening id 
screendata$resulted <- ifelse(screendata$screened, ifelse(screendata$xpertscreen_result=="", 0, 1), NA)
screendata %>% filter(seenathome==1) %>% count(ineligible, prior, refused, screened, resulted)
screendata$positive <- ifelse(screendata$resulted, ifelse(screendata$xpertscreen_category==1, 1, 0), NA)
sum(screendata$screened, na.rm=T)
sum(screendata$resulted, na.rm=T)

screendata$screenedaway <- ifelse(screendata$residentnumber==0 & screendata$res_parish %in% 1:3, ifelse(screendata$screening_id != "", 1, 0), NA)
sum(screendata$screenedaway, na.rm=T)

# compare residents found at home, versus all described residents
notseen <- table(members$res1_age) - table(subset(screendata,seenathome==1)$age)
notseen <- as.numeric(rep(names(notseen), times = notseen))
# subset(screendata,seenathome==1)$age[order(subset(screendata,seenathome==1)$age)] == as.numeric(rep(names(seen), times = seen))
par(mfrow=c(1,2)); hist(notseen); hist(subset(screendata,seenathome==1)$age ) 
t.test(notseen,
       subset(screendata,seenathome==1)$age ) # those not home are slightly older
wilcox.test(notseen, 
       subset(screendata,seenathome==1)$age ) 
(femalehome <- array(c(table(subset(screendata,seenathome==1)$female), 
                     table(members$res1_female) - table(subset(screendata,seenathome==1)$female)), 
                      dim=c(2,2)))
fisher.test(femalehome) 

# compare to those screened away from home
# by age first:
# limit aways to those from the study area:
t.test(notseen, subset(screendata, residentnumber==0 & screendata$res_parish %in% 1:3)$res_age) # those screened elsewhere slightly older tha those not at home
t.test(subset(screendata,seenathome==1)$age, subset(screendata, residentnumber==0 & screendata$res_parish %in% 1:3)$res_age) # those screened elsewhere slightly older tha those not at home
# and compare sex:
(femaway <- table(subset(screendata,residentnumber==0 & screendata$res_parish %in% 1:3)$female))
(femtable <- rbind(femalehome, femaway)); rownames(femtable)=c("Seen at home", "Missed at home", "Seen elsewhere")
  colnames(femtable)=c("Male","Female"); 
fisher.test(rbind(femalehome[1,], femaway)) # found at home vs away
fisher.test(rbind(femalehome[2,], femaway) ) # missed at home vs found away

t.test(subset(screendata,seenathome==1)$age, subset(screendata, residentnumber==0 & screendata$res_parish %in% 1:3)$res_age) # those screened elsewhere slightly older tha those not at home
par(mfrow=c(1,3)); hist(subset(screendata,seenathome==1)$age, main="Found at home" ); hist(notseen, main="Missed at home");  
hist(subset(screendata, residentnumber==0 & res_parish %in% 1:3)$res_age, main="Screened elsewhere")
median(subset(screendata,seenathome==1)$age, na.rm=T); median(notseen, na.rm=T); median(subset(screendata, residentnumber==0 & res_parish %in% 1:3)$res_age, na.rm=T)

round(cbind(femtable, prop.table(femtable, margin = 1)),digits = 2)
rbind(quantile(subset(screendata,seenathome==1)$age, c(0.25,0.5,0.75), na.rm=T), quantile(notseen, c(0.25,0.5,0.75), na.rm=T), quantile(subset(screendata, screenedaway==1)$age, c(0.25,0.5,0.75), na.rm=T))
# but note that there a large minority of households where we got no data and don't have any "missed at home" data. 

# among those found at home, screened versus not screened (ineligible or refused, with or without priors included)
# very few 'ineligible', but will look at ineligibles (besides priors) and refusals, versus those screened:
screendata %>% filter(seenathome==1 & prior %in% c(NA,0)) %>% group_by(screened) %>% summarize(median(age, na.rm=T), mean(female, na.rm=T), mean(residentnumber==1&res1_head_yn==1, na.rm=T), n())
# Those screened at home (versus refusal) are a little younger, more female, and less likely to be head of household, but differences are small.


# some were excluded or refused because already on TB treatment (3), on IPT (1), or recently screened (sevearl): 
#  (contrary to protocol, those already on treatment weren't enrolled as cases)
# Other reasons for "refusal" included no guardian present, unable/unwilling to produce sputum, unable to speak
levels(screendata$res_nocall_notes)
screendata %>% filter(refused==1) %>% select(res_nocall_notes)
screendata %>% filter(!is.na(ontbrx)) %>% select(res_check_iris_id, res_prior_stomp_iris, res_irisid) # don't have these. could look for names, need datset version with identifiers.

############# Prevalence of TB ####################
screendata %>% filter(screened==1) %>% count(xpertscreen_category.factor) # consented at home
screendata %>% filter(screenedaway==1) %>% count(xpertscreen_category.factor) # consented away

screendata %>% filter(screenedaway==1) %>% group_by(venue_yn) %>% count(xpertscreen_category.factor) # split by type of away from home screening


screendata %>% filter(screened==1 & as.Date(res_interact_date, format="%Y-%m-%d") < as.Date("2019-09-01", format="%Y-%m-%d")) %>% count(xpertscreen_category.factor) # consented at home
screendata %>% filter(screenedaway==1 & as.Date(res_interact_date, format="%Y-%m-%d") < as.Date("2019-09-01", format="%Y-%m-%d")) %>% count(xpertscreen_category.factor) # consented away

# sputum volume and quantity
screendata %>% filter(screened==1|screenedaway==1) %>% count(xpertscreen_appearance)
par(mfrow=c(1,1)); screendata %>% filter(screened==1|screenedaway==1) %>% select(xpertscreen_volume) %>% unlist(.) %>% hist # looks like a long tail, but there are several in the 20-40 ml range, looks like intentionally entered data
screendata %>% filter(screened==1|screenedaway==1) %>% filter(xpertscreen_volume < 5) %>% select(xpertscreen_volume) %>% unlist(.) %>% hist
screendata$lowvolume <- screendata$xpertscreen_volume <= 0.5
# could do sensiivity analysis that excludes those <1ml, or salivary (about half of all specimens), but probably best to assume they don't have true TB.

# is salivary sputum associated with invalid results? No.
table(screendata$xpertscreen_appearance, screendata$xpertscreen_category.factor) #No it isn't, and many are positive. But there are several invalid/indeterminate with no appearance recorded. all rejected:
screendata %>% filter(xpertscreen_category.factor=="Invalid or indeterminate") %>% select(xpertscreen_volume, xpertscreen_result_comment, xpertscreen_notes)
# low volume associated with TB? Yes, only half as likely to be positive. 
screendata %>% group_by(lowvolume) %>% summarize(mean(xpertscreen_category==1))

screendata %>% filter(screened==1|screenedaway==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)
screendata %>% filter(screened==1|screenedaway==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% group_by(screenedaway) %>% count(xpertscreen_category)

# cases found through screening:
sum(screendata$xpertscreen_category==1, na.rm = T)
# adding those already diagnosed elsewhere, and those diagnosed at health facility: 
sum(screendata$xpertscreen_category==1, na.rm = T) + 56 # to estimate total preavlence, will need to distribute these HF cases over the entire community, not just those found during screening

# community HF notification rate during community phase: 
56/(sum(households$hh_res_no, na.rm=T) + sum(households$hh_child, na.rm=T))/mean(!is.na(households$hh_res_no) | !is.na(households$hh_child))



# estimate prevalence among those screened, with binomial confidence intervals:
prev <- function(count)
{
  rownames(count) <- count$xpertscreen_category
  return(binom.agresti.coull(count["1","n"], sum(count[,'n'])))
}
(screendata %>% filter(screened==1|screenedaway==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)) %>% prev(.)
(screendata %>% filter(screened==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)) %>% prev(.)
(screendata %>% filter(screenedaway==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)) %>% prev(.)

(screendata %>% filter(screenedaway==1 & venue_yn==1) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)) %>% prev(.)
(screendata %>% filter(screenedaway==1 & venue_yn==0) %>% filter(xpertscreen_category %in% c(0,1)) %>% count(xpertscreen_category)) %>% prev(.)

chisq.test(subset(screendata, (screened==1|screenedaway==1)&(xpertscreen_category %in% c(0,1)))$screenedaway, 
           subset(screendata, (screened==1|screenedaway==1)&(xpertscreen_category %in% c(0,1)))$xpertscreen_category )
fisher.test(subset(screendata, (screened==1|screenedaway==1)&(xpertscreen_category %in% c(0,1)))$screenedaway, 
           subset(screendata, (screened==1|screenedaway==1)&(xpertscreen_category %in% c(0,1)))$xpertscreen_category )


screendata %>% group_by(xpertscreen_category==1, screenedaway) %>% summarize(median(res_age, na.rm=T), mean(res_female, na.rm=T))

# characteristics of cases:
# -	Sex and age, compared to overall and screened populations
# compared to screened first because it's easiest:


# -	Variation in prevalence by zone
# -	Income/employment/education/etc (versus com controls?) -- will need to more to the case dataset for this and below.
# -	Migration of cases (versus com controls?) incl time in study area and frequency of short- and long-distance travel

# adjust prevalence for demographics?



# question for team: where are we capturing people who tell us they are on treatment when we screen?

### Need to add cases diagnosed at our health facilities and elsewhere, to an adjusted estimate of community prevalence ###

# can look at risk factors for TB

# can 


##################################################
# Once we bring in case enrollment data:
# Differences between 
# cases, enrolled
# cases, identified not enrolled

# Xpert levels compared
table(screendata$xpertscreen_result)
comlevels <- c(1+47+2,1+7+2,7,11,6)
names(comlevels) <- c("Trace","Very low", "Low", "Medium", "High")
barplot(comlevels)

casedata <- subset(d_original, participanttype==1)
hflevels <- table(casedata$labtbxprslt_qty)[c(5,1:4)]
names(hflevels) <- names(comlevels)

par(mfrow=c(1,2))
cols <- RColorBrewer::brewer.pal(5, "Reds")
barplot(hflevels, main="Health Facility Cases", ylab="N", col=cols)
barplot(comlevels, main="Community cases", col=cols)
mtext("Level of Xpert positivity", side=1, outer=T, line=-2)

