library(tidyverse)
library(haven)
library(labelled)


raw_df <- read_sav("raw.sav", encoding = "CP936")
raw_df

raw_df %>% sjPlot::view_df()



df <- raw_df %>%
  select(1:45) %>%
  mutate(
    across(c(Q1:Q7), to_factor),
    across(c(Q8:Q45), remove_labels)
  ) %>% 
  rowwise() %>%
  mutate(
    dim_x1 = mean(c_across(Q8:Q10)),
    dim_x2 = mean(c_across(Q11:Q13)),  
    dim_x3 = mean(c_across(Q14:Q16)),  
    dim_x4 = mean(c_across(Q17:Q19)),  
    dim_x5 = mean(c_across(Q20:Q22)),  
    
    dim_m1 = mean(c_across(Q23:Q26)),    
    dim_m2 = mean(c_across(Q27:Q30)),     
    dim_m3 = mean(c_across(Q31:Q34)),  
    
    dim_y1 = mean(c_across(Q35:Q42)),      
    dim_y2 = mean(c_across(Q43:Q45)), 
    
    total_x = mean(c_across(Q8:Q22)),   
    total_m = mean(c_across(Q23:Q34)),   
    total_y = mean(c_across(Q35:Q45)) 
  ) %>% 
  ungroup() %>% 
  rename(
    gender = Q1, age = Q2, education = Q3, experience = Q4,
    position = Q5, scale = Q6, salary = Q7
  ) %>% 
  select(-starts_with("Q"))

df


# 保存整理好的数据
df %>% write_rds("well.rds")
