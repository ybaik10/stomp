source("case control contact data cleaning.R")

# summary of CRPs run, for Adithya:
casecontrols <- subset(data, participanttype %in% 1:4)
table(casecontrols$crp_result_quantifiable.factor, casecontrols$participanttype, useNA = "ifany")
table(casecontrols$crp_result_quantifiable.factor, casecontrols$participanttype, as.Date(casecontrols$date_enrolled) < as.Date("2019-09-20"), useNA = "ifany")
# casecontrols %>% filter(as.Date(date_enrolled) > as.Date("2018-05-24") & is.na(crp_result_quantifiable.factor)) %>% select(participanttype, crp_specimen_date, date_enrolled, why_no_crp.factor)
casecontrols %>% arrange(desc(why_no_crp)) %>% filter(as.Date(date_enrolled) < as.Date("2019-09-20") & is.na(crp_result_quantifiable.factor)) %>% select(participanttype, crp_specimen_date, date_enrolled, why_no_crp.factor)

# missing CRPs to look for:
casecontrols %>% filter(is.na(why_no_crp.factor) & as.Date(date_enrolled) < as.Date("2019-09-01") & is.na(crp_result_quantifiable.factor)) %>% 
  select(studyid, participanttype, crp_specimen_date, date_enrolled, crp_result_quantifiable.factor)

