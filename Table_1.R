# Step 1: Create loan amount dummies
df <- df %>% 
  mutate(sec_loanamount1 = ifelse(sec_loanamount == 4000, 1, 0),
         sec_loanamount2 = ifelse(sec_loanamount == 5000, 1, 0),
         sec_loanamount3 = ifelse(sec_loanamount == 6000, 1, 0),
         sec_loanamount4 = ifelse(sec_loanamount == 7000, 1, 0),
         sec_loanamount5 = ifelse(sec_loanamount == 8000, 1, 0),
         sec_loanamount6 = ifelse(sec_loanamount == 9000, 1, 0),
         sec_loanamount7 = ifelse(sec_loanamount == 10000, 1, 0))

# Step 2: Define controls vector
controls <- c("sec_loanamount1", "sec_loanamount2", "sec_loanamount3", 
              "sec_loanamount4", "sec_loanamount5", "sec_loanamount6")

# Step 3: Generate missing value indicators for each sec_loanamount
for (var in controls) {
  df[[paste0("miss_", var)]] <- ifelse(is.na(df[[var]]), 1, 0)
}

# Step 4: Define dependent variables
defaultvars <- c("Business_Expenditures", "Inventory_Raw_Mat", "Equipment", "Other_Bus_Cost",
                 "Non_Business_Exp", "Repairs_Repair_Only", "Utilities_Taxes_Rent",
                 "Human_Capital", "Re_Lent", "Savings", "Food_And_Durable", "New_Business_Ap15")

# Step 5: Run first set of regressions (without additional controls)
results_table <- data.frame(DependentVariable = character(),
                            ControlGroupMean = numeric(),
                            ControlGroupSD = numeric(),
                            OLS_NoControls_Estimate = numeric(),
                            OLS_NoControls_SE = character(),
                            OLS_WithControls_Estimate = numeric(),
                            OLS_WithControls_SE = character(),
                            stringsAsFactors = FALSE)

for (var in defaultvars) {
  # Get Control Group Mean & SD
  control_group <- df %>% filter(sec_treat == 0)
  control_mean <- mean(control_group[[var]], na.rm = TRUE)
  control_sd <- sd(control_group[[var]], na.rm = TRUE)
  
  # OLS No Controls
  formula_no_controls <- as.formula(paste(var, "~ sec_treat + Match3rd_in3rd + factor(Stratification_Dummies) +", paste(controls, collapse = " + ")))
  model_no_controls <- lm(formula_no_controls, data = df)
  coefs_no_controls <- coeftest(model_no_controls, vcov = vcovCL, cluster = ~sec_group_name)
  
  # OLS With Controls
  additional_controls <- c("Age_C", "Married_C", "Muslim_C", "HH_Size_C", "Years_Education_C",
                           "shock_any_C", "Has_Business_C", "Financial_Control_C", "homeowner_C",
                           "sec_loanamount1", "sec_loanamount2", "sec_loanamount3", 
                           "sec_loanamount5", "sec_loanamount6", "sec_loanamount7", "No_Drain_C")
  
  formula_with_controls <- as.formula(paste(var, "~ sec_treat + Match3rd_in3rd + factor(Stratification_Dummies) +", paste(additional_controls, collapse = " + ")))
  model_with_controls <- lm(formula_with_controls, data = df)
  coefs_with_controls <- coeftest(model_with_controls, vcov = vcovCL, cluster = ~sec_group_name)
  
  # Extract sec_treat coefficient & SE
  sec_treat_coef_no_controls <- coefs_no_controls["sec_treat", 1]
  sec_treat_se_no_controls <- paste0("(", round(coefs_no_controls["sec_treat", 2], 2), ")")
  
  sec_treat_coef_with_controls <- coefs_with_controls["sec_treat", 1]
  sec_treat_se_with_controls <- paste0("(", round(coefs_with_controls["sec_treat", 2], 2), ")")
  
  # Store in results table
  results_table <- rbind(results_table, 
                         data.frame(DependentVariable = var,
                                    ControlGroupMean = round(control_mean, 2),
                                    ControlGroupSD = round(control_sd, 2),
                                    OLS_NoControls_Estimate = round(sec_treat_coef_no_controls, 2),
                                    OLS_NoControls_SE = sec_treat_se_no_controls,
                                    OLS_WithControls_Estimate = round(sec_treat_coef_with_controls, 2),
                                    OLS_WithControls_SE = sec_treat_se_with_controls))
}

# Step 6: Format Table to Match Image Format
formatted_results_table <- results_table %>%
  mutate(`Control Group Mean (SD)` = paste0(ControlGroupMean, " (", ControlGroupSD, ")"),
         `OLS (No Controls)` = paste0(OLS_NoControls_Estimate, " ", OLS_NoControls_SE),
         `OLS (With Controls)` = paste0(OLS_WithControls_Estimate, " ", OLS_WithControls_SE)) %>%
  select(`Dependent Variable` = DependentVariable, `Control Group Mean (SD)`, 
         `OLS (No Controls)`, `OLS (With Controls)`)

# Step 7: Save formatted table to CSV
write.csv(formatted_results_table, "./output/Table_1.csv", row.names = FALSE)

# Step 8: Print table in R console
print(formatted_results_table)