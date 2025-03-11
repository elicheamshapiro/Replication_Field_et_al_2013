# Load data
df <- read_dta("./data/Grace-Period-Data.dta")

df <- df %>% 
  mutate(sec_loanamount1 = ifelse(sec_loanamount == 4000, 1, 0),
         sec_loanamount2 = ifelse(sec_loanamount == 5000, 1, 0),
         sec_loanamount3 = ifelse(sec_loanamount == 6000, 1, 0),
         sec_loanamount4 = ifelse(sec_loanamount == 7000, 1, 0),
         sec_loanamount5 = ifelse(sec_loanamount == 8000, 1, 0),
         sec_loanamount6 = ifelse(sec_loanamount == 9000, 1, 0),
         sec_loanamount7 = ifelse(sec_loanamount == 10000, 1, 0),
         miss_sec_loanamount1 = ifelse(is.na(sec_loanamount1), 1, 0),
         miss_sec_loanamount2 = ifelse(is.na(sec_loanamount2), 1, 0),
         miss_sec_loanamount3 = ifelse(is.na(sec_loanamount3), 1, 0),
         miss_sec_loanamount4 = ifelse(is.na(sec_loanamount4), 1, 0),
         miss_sec_loanamount5 = ifelse(is.na(sec_loanamount5), 1, 0),
         miss_sec_loanamount6 = ifelse(is.na(sec_loanamount6), 1, 0),
         miss_sec_loanamount7 = ifelse(is.na(sec_loanamount7), 1, 0))

# Define variables
default_vars <- c("Late_Days_364", "Late_Days_476", "not_finished_aug19", "Outstanding_Loan_Amount_Default", "Fifty_Percent_Loan_Paid", "Made_First_11_Pay_On_Time", "Made_First_Pay")
specifications <- "sec_treat + as.factor(Stratification_Dummies)"

#Generate "missing" controls

df <- df %>%
  mutate(miss_Age_C = ifelse(is.na(Age_C), 1, 0),
         miss_Married_C = ifelse(is.na(Married_C), 1, 0),
         miss_Literate_C = ifelse(is.na(Literate_C), 1, 0),
         miss_Muslim_C = ifelse(is.na(Muslim_C), 1, 0),
         miss_Years_Education_C = ifelse(is.na(Years_Education_C), 1, 0),
         miss_shock_any_C = ifelse(is.na(shock_any_C), 1, 0),
         miss_Has_Business_C = ifelse(is.na(Has_Business_C), 1, 0),
         miss_Financial_Control_C = ifelse(is.na(Financial_Control_C), 1, 0),
         miss_homeowner_C = ifelse(is.na(homeowner_C), 1, 0),
         miss_No_Drain_C = ifelse(is.na(No_Drain_C), 1, 0))

# Regressions
models <- list(
# Without controls
  lm(Late_Days_364 ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(Late_Days_476 ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(not_finished_aug19 ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(Outstanding_Loan_Amount_Default ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(Fifty_Percent_Loan_Paid ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(Made_First_11_Pay_On_Time ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),
  lm(Made_First_Pay ~ sec_treat + as.factor(Stratification_Dummies), data = df, clusters = sec_group_name),

# With controls
  lm(Late_Days_364 ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C  + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(Late_Days_476 ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(not_finished_aug19 ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(Outstanding_Loan_Amount_Default ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(Fifty_Percent_Loan_Paid ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(Made_First_11_Pay_On_Time ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name),
  lm(Made_First_Pay ~ sec_treat + as.factor(Stratification_Dummies) + as.factor(sec_loan_officer) + Age_C + Married_C + 
      Literate_C + Muslim_C + HH_Size_C + Years_Education_C + shock_any_C + Has_Business_C + 
      Financial_Control_C + homeowner_C + sec_loanamount1 + sec_loanamount2 + sec_loanamount3 + 
      sec_loanamount5 + sec_loanamount6 + sec_loanamount7 + No_Drain_C + 
       miss_Age_C + miss_Married_C + miss_Literate_C + miss_Muslim_C + 
       miss_Years_Education_C + miss_shock_any_C + miss_Has_Business_C + 
       miss_Financial_Control_C + miss_homeowner_C + miss_No_Drain_C + 
       miss_sec_loanamount1 + miss_sec_loanamount2 + 
       miss_sec_loanamount3 + miss_sec_loanamount5 +
       miss_sec_loanamount6 + miss_sec_loanamount7 + 
       sec_loanamount4 + miss_sec_loanamount4, data = df, clusters = sec_group_name)
)




# Extract coefficient estimates for sec_treat from all models
results <- lapply(models, function(model) {
  tidy(model, conf.int = TRUE) %>% 
    filter(term == "sec_treat")  # Keep only sec_treat
})

# Combine into a single dataframe
results_df <- bind_rows(results, .id = "Model")




results_df <- results_df %>%
  mutate(Estimate_SE = paste0(signif(estimate, 3), " (", signif(std.error, 3), ")")) %>%
  select(Model, Estimate_SE)



###### formatting table
# Extract coefficient estimates for sec_treat from all models
results <- lapply(models, function(model) {
  tidy(model, conf.int = TRUE) %>% 
    filter(term == "sec_treat")  # Keep only sec_treat
})

# Combine into a single dataframe
results_df <- bind_rows(results, .id = "Model")

# Format estimates with standard errors
results_df <- results_df %>%
  mutate(Estimate_SE = paste0(signif(estimate, 3), " (", signif(std.error, 3), ")")) %>%
  select(Model, Estimate_SE)

# Split into two panels: No controls (first 7 models) and With controls (last 7 models)
results_no_controls <- results_df[1:7, ] %>% rename(Estimate_NoControls = Estimate_SE)
results_with_controls <- results_df[8:14, ] %>% rename(Estimate_WithControls = Estimate_SE)

# Manually define variable names as in the table
variable_names <- c("Within 8 weeks", "Within 24 weeks", 
                    "Within 52 weeks", "Amount outstanding", 
                    "Repaid at least 50%", "First half payments on time", 
                    "Made first payment")

# Reshape into wide format with Panel A and Panel B as rows
final_table <- data.frame(
  Panel = c("Panel A: No Controls", "Panel B: With Controls"),
  `Within 8 weeks` = c(results_no_controls$Estimate_NoControls[1], results_with_controls$Estimate_WithControls[1]),
  `Within 24 weeks` = c(results_no_controls$Estimate_NoControls[2], results_with_controls$Estimate_WithControls[2]),
  `Within 52 weeks` = c(results_no_controls$Estimate_NoControls[3], results_with_controls$Estimate_WithControls[3]),
  `Amount outstanding` = c(results_no_controls$Estimate_NoControls[4], results_with_controls$Estimate_WithControls[4]),
  `Repaid at least 50%` = c(results_no_controls$Estimate_NoControls[5], results_with_controls$Estimate_WithControls[5]),
  `First half payments on time` = c(results_no_controls$Estimate_NoControls[6], results_with_controls$Estimate_WithControls[6]),
  `Made first payment` = c(results_no_controls$Estimate_NoControls[7], results_with_controls$Estimate_WithControls[7])
)

# Add observations and control means as separate rows
observations_row <- data.frame(Panel = "Observations", 
                               t(rep(nrow(df), length(variable_names))))
control_means_row <- data.frame(Panel = "Control Mean", 
                                t(signif(sapply(df[, default_vars], mean, na.rm = TRUE), 3)))

# Bind all together
colnames(observations_row) <- colnames(final_table)
colnames(control_means_row) <- colnames(final_table)
final_table <- rbind(final_table, observations_row, control_means_row)

write.csv(final_table, "./output/Table_3.csv", row.names = FALSE)

print(final_table)


