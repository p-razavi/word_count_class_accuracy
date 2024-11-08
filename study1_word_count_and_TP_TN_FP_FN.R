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
           hum_ae = ae,
           hum_ce = ce,
           hum_ad = ad,
           hum_a = a,
           hum_h = h,
           word_count = contains("word"),
           gpt_ae = ends_with("_AE"),
           gpt_ce = ends_with("_CE"),
           gpt_ad = ends_with("_AD"),
           gpt_a = ends_with("_A"),
           gpt_h = ends_with("_H")) %>% 
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
  print(n = 36)


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
    dplyr::select(contains("count")) %>%
    dplyr::select(-strat_count, -total_count) %>% 
    cor(., use = "complete.obs", method = "spearman") %>% 
    round(., 2) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    mutate(scenario = scen) %>% 
    relocate(scenario) %>% 
    filter(var == "word_count") %>% 
    dplyr::select(-c(var, word_count))
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
    dplyr::select(word_count, contains("ratio")) %>%
    cor(., use = "complete.obs", method = "spearman") %>% 
    round(., 2) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("var") %>% 
    mutate(scenario = scen) %>% 
    relocate(scenario) %>% 
    filter(var == "word_count") %>% 
    dplyr::select(-c(var, word_count))
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

#Are the two results the same? Yes!
ratio_cors_with_word_count == count_cors_with_word_count

#==================================================================================================================
#Calculate Partial Correlation bet. Word Count and Each Metric After Partialling out the Total Number of Strategies
#==================================================================================================================

library(ppcor) #note: this function messes with select() from dplyr

calculate_partial_cor_and_pvalues_with_wordcount <- function(scen){
  # Filter the dataframe for the given scenario and drop rows with missing values
  cor_df <- all_scenarios_aug %>%
    filter(!is.na(response)) %>%
    filter(scenario == scen) %>%
    dplyr::select(word_count, TP_count, TN_count, FP_count, FN_count, strat_count) %>%
    tidyr::drop_na()
  
  # Calculate partial correlation estimates and p-values controlling for strat_count
  results <- sapply(c("TP_count", "TN_count", "FP_count", "FN_count"), function(var){
    test_result <- pcor.test(cor_df$word_count, cor_df[[var]], cor_df$strat_count, method = "spearman")
    return(c(estimate = test_result$estimate, p_value = test_result$p.value))
  })
  
  # Convert results into a dataframe with both estimates and p-values
  results_df <- data.frame(
    var = rownames(results),
    estimate = round(results[1, ], 2),
    p_value = round(results[2, ], 3),
    scenario = scen
  ) %>%
    relocate(scenario)
  
  return(results_df)
}


partial_cors_with_pvalues <- calculate_partial_cor_and_pvalues_with_wordcount("Anger 1") %>% 
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Anger 2")) %>% 
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Anger 3")) %>%
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Fear 2")) %>%
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Fear 3")) %>%
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Sadness 1")) %>%
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Sadness 2")) %>%
  bind_rows(calculate_partial_cor_and_pvalues_with_wordcount("Sadness 3")) %>% 
  dplyr::select(-var) %>% 
  tibble::rownames_to_column("index") %>% 
  mutate(index = sub("count.*", "count", index)) %>% 
  mutate(p_value_adjusted = p.adjust(p_value, method = "holm")) %>% 
  mutate(index = stringr::str_replace(index, "_count", ""),
         r_sqrd = janitor::round_half_up(estimate^2, 2),
         sig = if_else(p_value_adjusted < 0.05, "*", " ")
         ) %>% 
  mutate(p_value = if_else(p_value < 0.001, "<0.001", as.character(p_value)),
         p_value_adjusted = if_else(p_value_adjusted < .001, "<0.001",
                                    if_else(p_value_adjusted > .999, "> 0.999", as.character(p_value_adjusted)))) %>% 
  relocate(scenario)

write.csv(partial_cors_with_pvalues, "study1_partial_cors.csv")  
