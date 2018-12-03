
all_risk_factors <- read.csv("K:/STIHIV_Data/Jessie/181103_all_risk_factors.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

clean_risk <- function(df) {
  df[df == "."] <- NA
  df[is.na(df)] <- "Unknown"
  df2 <- df %>%
    rename(event = "EventID") %>%
    select(-ends_with("ID")) %>%
    rename(EventID = "event")
  df2
}

all_risk_factors2 <- clean_risk(all_risk_factors)

#merge all risk factors with hep datasets and then never pull it again because it's so big.

#### write a function to make a smaller version of the datasets for merging simplicity

masters_clean <- function(df) {
  df2 <- df %>%
    select(EventID, ProfileID, calendar_year, county, AgeYears, age_binned, Gender) %>%
    filter(calendar_year < 2018)
  df2
}

hbv_all_acute_slim <- masters_clean(hbv_all_acute_binned)

hbv_all_chronic_slim <- masters_clean(hbv_all_chronic_binned)

hcv_all_acute_slim <- masters_clean(hcv_all_acute_2000)

hcv_all_chronic_slim <- masters_clean(hcv_all_chronic_binned)




hbv_all_acute_binned_risk <- left_join(hbv_all_acute_slim, all_risk_factors2, by= "EventID")

hbv_all_chronic_binned_risk <- left_join(hbv_all_chronic_slim, all_risk_factors2, by= "EventID")

hcv_all_acute_binned_risk <- left_join(hcv_all_acute_slim, all_risk_factors2, by= "EventID")

hcv_all_chronic_binned_risk <- left_join(hcv_all_chronic_slim, all_risk_factors2, by= "EventID")



###eliminate columns where the answer is always no or unknown
hbv_all_acute_binned_risk <- hbv_all_acute_binned_risk %>% 
  mutate_all(function(x) ifelse(x %in% c("Unknown", "None", "Don't Know", "No", "0"), 0, x))

hbv_all_acute_binned_risk[hbv_all_acute_binned_risk == "Yes"] <- 1

hbv_all_acute_binned_risk2 <- hbv_all_acute_binned_risk[, colSums(hbv_all_acute_binned_risk != 0, na.rm = TRUE) > 0]

#### hbv acute risk factors ---- 

hbv_acute_risk_13_17 <- hbv_all_acute_binned_risk2 %>%
  filter(calendar_year > 2012 & calendar_year < 2018) %>%
  select(-contains("Patient"), -Parous, -Gravida, -ConsentProvidedDate, -contains("Complete"), -contains("Provider"), 
         -Symptomatic, -AbdoPain, -contains("Symptom"), -Vomiting, -DarkUrine, -ClayColorStool, -Arthralgia, -contains("Jaundice"), 
         -Nausea, -LostAppetite, -Fever, -Diarrhea, -Fatigue, -Died, -contains("Previous"), -contains("Vax"), 
         -TestReasonBornUSA, -contains("ClottingFactor"), -BloodTransfusionYearMonth)

# neg_answers <- c("None", "Unknown", "No", 0, "0")
# 
# hbv_subset <- hbv_acute_risk_13_17 %>%
#   filter_at(vars(7:ncol(hbv_acute_risk_13_17)), all_vars(. %in% c("None", "Unknown", "No", 0, "0", "Don't Know")))
# 


######### hcv risk----------


###### for bar chart with unknowns included

hcv_all_acute_idu <- hcv_all_acute_binned_risk %>%
  ungroup() %>%
  filter(calendar_year > 2012 & calendar_year < 2018) %>%
  select(calendar_year, IDU6wks_6Mos) %>%
 # group_by(calendar_year, IDU6wks_6Mos) %>%
  count(calendar_year, IDU6wks_6Mos) %>%
  spread(IDU6wks_6Mos, n, fill = 0)



###eliminate columns where the answer is always no or unknown
hcv_all_acute_binned_risk[is.na(hcv_all_acute_binned_risk)] <- "Unknown"

#two sep dfs, then merge 

hcv_all_acute_binned_risk_sub1 <- hcv_all_acute_binned_risk %>%
  ungroup() %>%
  select(1:7)

hcv_all_acute_binned_risk_sub2 <- hcv_all_acute_binned_risk %>%
  ungroup() %>%
  select(1, 8:ncol(.)) %>% 
  select(-TestReasonYearBorn, -DiagnosisDate, -JailedOver24Hours)


hcv_all_acute_binned_risk_sub3 <- hcv_all_acute_binned_risk_sub2 %>% 
  mutate_all(function(x) ifelse(x %in% c("Unknown", "None", "Don't Know", "No", "0"), 0, x))

hcv_all_acute_binned_risk_sub3[hcv_all_acute_binned_risk_sub3 == "Yes"] <- 1

hcv_all_acute_binned_risk_sub3 <- hcv_all_acute_binned_risk_sub3[, colSums(hcv_all_acute_binned_risk_sub3 != 0, na.rm = TRUE) > 0]

hcv_all_acute_risk_comb <- left_join(hcv_all_acute_binned_risk_sub3, hcv_all_acute_binned_risk_sub1, by = "EventID")

#### hcv acute risk factors ---- 

hcv_acute_risk_13_17 <- hcv_all_acute_risk_comb %>%
  filter(calendar_year > 2012 & calendar_year < 2018) %>%
  select(-contains("Patient"), -Parous, -Gravida, -ConsentProvidedDate, -contains("Complete"), -contains("Provider"), 
         -Symptomatic, -AbdoPain, -contains("Symptom"), -Vomiting, -DarkUrine, -ClayColorStool, -Arthralgia, -contains("Jaundice"), 
         -Nausea, -LostAppetite, -Fever, -Diarrhea, -Fatigue, -Died, -contains("Previous"), -contains("Vax"), 
         -TestReasonBornUSA, -contains("ClottingFactor"), -BloodTransfusionYearMonth)



risky_func <- function(df) {
  df2 <- df %>%
    mutate(risky_tat = ifelse(Tattoo == 1 & TattooSource != "Commercial Parlor/Shop", 1, 0)) %>%
    mutate(msm_risk = ifelse(Gender == "Male" & (SexPartnersMale == ">5" | 
                                                   SexPartnersMale == "2-5" |
                                                   SexPartnersMale== "One" | 
                                                   SexPrefHomosexual == 1 #nobisexual variable, it was eliminated earlier bc no cases
                                                   ), 1, 0))
  ### any drug use var
  cols_drugs <- c("ContactIDU", "IDU6wks_6Mos", "LTCResident", "StreetDrugs6wks_6Mos", "HalfwayHouse")
  df2[cols_drugs] <- sapply(df2[cols_drugs], as.numeric) 
  df3 <- df2 %>%
    mutate(drug_use_possible = ifelse(ContactIDU + IDU6wks_6Mos + LTCResident +StreetDrugs6wks_6Mos + HalfwayHouse > 0, 1, 0)) %>%
    mutate(IDU = ifelse(ContactIDU + IDU6wks_6Mos > 0, 1, 0)) %>%
    mutate(IDU_msm = ifelse(IDU + msm_risk > 1, 1, 0)) %>%
    mutate(not_idu_msm_tat = ifelse(IDU == 0 & msm_risk == 0 & risky_tat == 0, 1, 0)) %>%
    mutate(risky_tat_idu = ifelse(risky_tat == 1 & IDU == 1, 1, 0)) %>%
    mutate(risky_tat_no_idu = ifelse(risky_tat == 1 & IDU == 0, 1, 0))
  df3
}

hcv_acute_risk_13_17_2 <- risky_func(hcv_acute_risk_13_17)

#break down by year, this is going to have to be manual for now


hcv_acute_risk_17 <- hcv_acute_risk_13_17_2 %>%
  filter(calendar_year == 2017)

hcv_acute_risk_16 <- hcv_acute_risk_13_17_2 %>%
  filter(calendar_year == 2016)

hcv_acute_risk_15 <- hcv_acute_risk_13_17_2 %>%
  filter(calendar_year == 2015)

#look at all other risk factors


