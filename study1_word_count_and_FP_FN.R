#Study 1: look at the role of word count in GPT's ability to code scenarios

library(dplyr)
library(tidyr)
library(readxl)


#===========================================================================
#First, we need a single dataframe that contains both the human and AI codes.
#===========================================================================

#scen <- "Anger 1"

#function to merge the human code and AI code (from different data sources)
combine_scenario_codes <- function(scen){
  human_codes <- read_xlsx("study1_human.xlsx", sheet = scen) %>% 
    janitor::clean_names() %>% 
    select(no:h)
  
  colnames(human_codes)[2] <- "response"
  
  all_gpt_codes <- read_xlsx("study1_WordCount_GPT.xlsx") %>% 
    mutate(no = row_number()) %>% 
    relocate(no)
  scen_abrev <- first_last <- paste0(substr(scen, 1, 1), substr(scen, nchar(scen), nchar(scen)))
  
  
  both <- human_codes %>% 
    left_join(all_gpt_codes %>% select(no, starts_with(scen_abrev)),
              by = c("no" = "no")) %>% 
    rename(id = no,
           gpt_ae = ae,
           gpt_ce = ce,
           gpt_ad = ad,
           gpt_a = a,
           gpt_h = h,
           word_count = contains("word"),
           hum_ae = ends_with("_AE"),
           hum_ce = ends_with("_CE"),
           hum_ad = ends_with("_AD"),
           hum_a = ends_with("_A"),
           hum_h = ends_with("_H")) %>% 
    mutate(scenario = scen) %>% 
    relocate(scenario, id, response, word_count)
  
  return(both)
}

#apply the function for all scenarios and combine them into one dataframe
all_scenarios <- combine_scenario_codes("Anger 1") %>% 
  bind_rows(combine_scenario_codes("Anger 2")) %>% 
  bind_rows(combine_scenario_codes("Anger 3")) %>% 
  bind_rows(combine_scenario_codes("Fear 2")) %>% 
  bind_rows(combine_scenario_codes("Fear 3")) %>% 
  bind_rows(combine_scenario_codes("Sadness 1")) %>% 
  bind_rows(combine_scenario_codes("Sadness 2")) %>% 
  bind_rows(combine_scenario_codes("Sadness 3")) 

#check: do the scenarios have the same counts?
all_scenarios %>% 
  group_by(scenario) %>% 
  summarise(min = min(id),
            max = max(id))

#how many NAs do we have?
sum(is.na(all_scenarios$response)) / nrow(all_scenarios)

#save the df
write.csv(all_scenarios, "study1_all_scenarios_and_codes.csv", row.names = FALSE)

#================================
#Now calculate FP, FN, TP, and TN
#================================

all_scenarios_aug <- all_scenarios %>% 
  mutate(ae = case_when(gpt_ae == 1 & hum_ae == 1 ~ "TP",
                        gpt_ae == 0 & hum_ae == 0 ~ "TN",
                        gpt_ae == 0 & hum_ae == 1 ~ "FN",
                        gpt_ae == 1 & hum_ae == 0 ~ "FP"),
         ce = case_when(gpt_ce == 1 & hum_ce == 1 ~ "TP",
                        gpt_ce == 0 & hum_ce == 0 ~ "TN",
                        gpt_ce == 0 & hum_ce == 1 ~ "FN",
                        gpt_ce == 1 & hum_ce == 0 ~ "FP"),
         ad = case_when(gpt_ad == 1 & hum_ad == 1 ~ "TP",
                        gpt_ad == 0 & hum_ad == 0 ~ "TN",
                        gpt_ad == 0 & hum_ad == 1 ~ "FN",
                        gpt_ad == 1 & hum_ad == 0 ~ "FP"),
         a = case_when(gpt_a == 1 & hum_a == 1 ~ "TP",
                        gpt_a == 0 & hum_a == 0 ~ "TN",
                        gpt_a == 0 & hum_a == 1 ~ "FN",
                        gpt_a == 1 & hum_a == 0 ~ "FP"),
         h = case_when(gpt_h == 1 & hum_h == 1 ~ "TP",
                        gpt_h == 0 & hum_h == 0 ~ "TN",
                        gpt_h == 0 & hum_h == 1 ~ "FN",
                        gpt_h == 1 & hum_h == 0 ~ "FP")) %>% 
  mutate(strat_count = rowSums(across(c(hum_ae, hum_ce, hum_ad, hum_a, hum_h))))

#descriptively look at the frequency of different number of strategies per scenario
all_scenarios_aug %>% 
  filter(!is.na(response)) %>% 
  group_by(scenario, strat_count) %>% 
  summarize(count = n()) %>% 
  print(n = 35)


all_scenarios_aug <- all_scenarios_aug %>% 
  mutate(
    TP_count = rowSums(across(c(ae, ce, ad, a, h)) == "TP"),
    TN_count = rowSums(across(c(ae, ce, ad, a, h)) == "TN"),
    FP_count = rowSums(across(c(ae, ce, ad, a, h)) == "FP"),
    FN_count = rowSums(across(c(ae, ce, ad, a, h)) == "FN"),
    total_count = rowSums(across(c(TP_count, TN_count, FP_count, FN_count)))
  ) %>% 
  mutate(TP_ratio = TP_count/total_count,
         TN_ratio = TN_count/total_count,
         FP_ratio = FP_count/total_count,
         FN_ratio = FN_count/total_count) %>% 
  ungroup()



#this function calculates the correlation between word count and the TP, TN, FP, FN counts:
#scen <- "Anger 1"
calculate_counts_cor_with_wordcount <- function(scen){
  cor_df <- all_scenarios_aug %>%
    filter(!is.na(response)) %>%
    filter(scenario == scen) %>%
    select(contains("count")) %>%
    select(-strat_count, -total_count) %>% 
    cor(., use = "complete.obs", method = "spearman") %>% 
    round(., 2) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    mutate(scenario = scen) %>% 
    relocate(scenario) %>% 
    filter(var == "word_count") %>% 
    select(-c(var, word_count))
  return(cor_df)
}

count_cors_with_word_count <- calculate_counts_cor_with_wordcount("Anger 1") %>% 
  bind_rows(calculate_counts_cor_with_wordcount("Anger 2")) %>% 
  bind_rows(calculate_counts_cor_with_wordcount("Anger 3")) %>%
  bind_rows(calculate_counts_cor_with_wordcount("Fear 2")) %>%
  bind_rows(calculate_counts_cor_with_wordcount("Fear 3")) %>%
  bind_rows(calculate_counts_cor_with_wordcount("Sadness 1")) %>%
  bind_rows(calculate_counts_cor_with_wordcount("Sadness 2")) %>%
  bind_rows(calculate_counts_cor_with_wordcount("Sadness 3")) 
  

#this other function calculates the correlation between word count and the TP, TN, FP, FN ratios:
#scen <- "Anger 1"
calculate_ratios_cor_with_wordcount <- function(scen){
  cor_df <- all_scenarios_aug %>%
    filter(!is.na(response)) %>%
    filter(scenario == scen) %>%
    select(word_count, contains("ratio")) %>%
    cor(., use = "complete.obs", method = "spearman") %>% 
    round(., 2) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    mutate(scenario = scen) %>% 
    relocate(scenario) %>% 
    filter(var == "word_count") %>% 
    select(-c(var, word_count))
  return(cor_df)
}

ratio_cors_with_word_count <- calculate_ratios_cor_with_wordcount("Anger 1") %>% 
  bind_rows(calculate_ratios_cor_with_wordcount("Anger 2")) %>% 
  bind_rows(calculate_ratios_cor_with_wordcount("Anger 3")) %>%
  bind_rows(calculate_ratios_cor_with_wordcount("Fear 2")) %>%
  bind_rows(calculate_ratios_cor_with_wordcount("Fear 3")) %>%
  bind_rows(calculate_ratios_cor_with_wordcount("Sadness 1")) %>%
  bind_rows(calculate_ratios_cor_with_wordcount("Sadness 2")) %>%
  bind_rows(calculate_ratios_cor_with_wordcount("Sadness 3")) 


#try partial correlations next (partialling out the total number of strategies)
