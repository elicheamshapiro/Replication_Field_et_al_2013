#Clean Workspace
rm(list = ls())

#Load Data
df <- read_dta("./data/Grace-Period-Data.dta")

# Create empty vectors to store the results for treatment and control groups
Ideal_NPV_Treat <- numeric(22)  
Ideal_NPV_Control <- numeric(22)  

# Loop through interest rates from 10 to 31
for (j in 10:31) {
  
  # Initialize the Ideal NPV variables for each interest rate
  Ideal_NPV_Treat_j <- 0
  Ideal_NPV_Control_j <- 0
  
  # Loop through fortnights from 1 to 22
  for (i in 1:22) {
    
    # Calculate Ideal NPV for treatment group (sec_treat == 1)
    Ideal_NPV_Treat_j <- Ideal_NPV_Treat_j + (8000 * (1 + j /100 ) / 22) / (df$Fortnightly_Interest_Rate^(i + 4))
    
    # Calculate Ideal NPV for control group (sec_treat == 0)
    Ideal_NPV_Control_j <- Ideal_NPV_Control_j + (8000 * (1 + j /100 ) / 22) / (df$Fortnightly_Interest_Rate^i)
  }
  
  # Store the calculated Ideal NPV values for the treatment and control groups
  Ideal_NPV_Treat[j - 9] <- Ideal_NPV_Treat_j  # Store the value, indexing starts from 1
  Ideal_NPV_Control[j - 9] <- Ideal_NPV_Control_j  # Store the value, indexing starts from 1
}


# NPV OF THE AMOUNT THAT THEY SHOULD'VE REPAID

# Initialize the columns for the repayment NPVs
df$Pay_Due_Each_Repayment_NPV_Treat <- 0
df$Pay_Due_Each_Repayment_NPV_Cont <- 0

# Loop through loan amounts from 4000 to 10000, in increments of 1000
for (j in seq(4000, 10000, by = 1000)) {
  
  # Loop through fortnights from 1 to 22
  for (i in 1:22) {
    
    # Update Pay_Due_Each_Repayment_NPV_Treat for treatment group (sec_treat == 1)
    df$Pay_Due_Each_Repayment_NPV_Treat <- ifelse(
      df$sec_loanamount == j & df$sec_treat == 1,
      df$Pay_Due_Each_Repayment_NPV_Treat + (j * 1.10 / 22) / (df$Fortnightly_Interest_Rate^(i + 4)),
      df$Pay_Due_Each_Repayment_NPV_Treat
    )
    
    # Update Pay_Due_Each_Repayment_NPV_Cont for control group (sec_treat == 0)
    df$Pay_Due_Each_Repayment_NPV_Cont <- ifelse(
      df$sec_loanamount == j & df$sec_treat == 0,
      df$Pay_Due_Each_Repayment_NPV_Cont + (j * 1.10 / 22) / (df$Fortnightly_Interest_Rate^i),
      df$Pay_Due_Each_Repayment_NPV_Cont
    )
  }
}

# Create the final Pay_Due_Each_Repayment_NPV column
df$Pay_Due_Each_Repayment_NPV <- df$Pay_Due_Each_Repayment_NPV_Treat
df$Pay_Due_Each_Repayment_NPV[df$Pay_Due_Each_Repayment_NPV == 0] <- df$Pay_Due_Each_Repayment_NPV_Cont[df$Pay_Due_Each_Repayment_NPV == 0]


########################## WITH SELECTION ##############################

#REPAYMENT RATE PER PERSON
df$Repayment_Rate <- df$Received_Amount_NPV/df$Pay_Due_Each_Repayment_NPV

df <- df %>% filter(!is.na(Interest_Rate))

treatment_group <- df %>% filter(Prefer_NoGrace == 0, sec_treat == 1)
control_group <- df %>% filter(Prefer_NoGrace == 0, sec_treat == 0)

mean_Repayment_Rate_PT_GT <- numeric(22)
mean_Repayment_Rate_PT_GC <- numeric(22)



for (j in 10:31) {
  # The Stata code uses 1.10, 1.11, ..., 1.31 for the interest rate thresholds
  interest_rate_threshold <- 1 + j / 100  # This will give values from 1.10 to 1.31
  
  treatment_group_filtered <- treatment_group %>% filter(Interest_Rate >= interest_rate_threshold)
  if(nrow(treatment_group_filtered) > 0) {
    mean_Repayment_Rate_PT_GT[j - 9] <- mean(treatment_group_filtered$Repayment_Rate, na.rm = TRUE)
  } else {
    mean_Repayment_Rate_PT_GT[j - 9] <- NA
  }
  
  control_group_filtered <- control_group %>% filter(Interest_Rate >= interest_rate_threshold)
  if(nrow(control_group_filtered) > 0) {
    mean_Repayment_Rate_PT_GC[j - 9] <- mean(control_group_filtered$Repayment_Rate, na.rm = TRUE)
  } else {
    mean_Repayment_Rate_PT_GC[j - 9] <- NA
  }
}

# Print out the results
mean_Repayment_Rate_PT_GT
mean_Repayment_Rate_PT_GC

# THE NUMBER OF PEOPLE AT EACH INTEREST RATE

# Remove rows with NA in Interest_Rate and Prefer_NoGrace
df <- df %>% filter(!is.na(Interest_Rate), !is.na(Prefer_NoGrace))

# Create N column where 1 represents each observation
df$N <- 1

# Calculate the total number of observations
Total <- nrow(df)

mean_Prefer_Grace <- numeric(22)

for (j in 10:31) {
  # Calculate the interest rate threshold (1.10, 1.11, ..., 1.31)
  interest_rate_threshold <- 1 + j / 100
  
  # Filter the rows for each interest rate threshold
  group_filtered <- df %>% filter(Interest_Rate >= interest_rate_threshold)
  
  # Calculate the number of people with that interest rate threshold
  N_at_threshold <- nrow(group_filtered)
  
  # Calculate the mean for the given interest rate threshold
  mean_Prefer_Grace[j - 9] <- N_at_threshold / Total
}

mean_Prefer_Grace

B <- matrix(NA, nrow = 22, ncol = 1)

for (j in 10:31) {
# Calculate Profit Per Person for Treatment and Control groups
Profit_Per_Person_PT_GT_j <- mean_Repayment_Rate_PT_GT[j - 9] * Ideal_NPV_Treat[j - 9] - 8000
Profit_Per_Person_PT_GC_j <- mean_Repayment_Rate_PT_GC[j - 9] * Ideal_NPV_Control[1] - 8000

# Calculate the Difference in Profits
Difference_Profits_j <- mean_Prefer_Grace[j - 9] * (Profit_Per_Person_PT_GT_j - Profit_Per_Person_PT_GC_j)

# Store the Difference of Profits in matrix B (adjusting index for 1-based indexing)
B[j - 9, 1] <- Difference_Profits_j
}

# Display the matrix B with the calculated difference profits
print(B)

# Create new columns for Number, Difference_Profits, and Interest
df <- df %>% arrange(slid)  # Sorting by slid
df$Number <- seq_len(nrow(df))  # Creating a sequence number column
df$Difference_Profits <- NA
df$Interest <- NA

# Loop through interest rates 10 to 31 and assign values
for (j in 10:31) {
  n <- j - 9  # Adjusting index for 1-based indexing
  df$Interest[df$Number == n] <- j  # Assigning interest rate value
  df$Difference_Profits[df$Interest == j] <- B[n, 1]  # Assigning Difference_Profits from matrix B
}

# Label variable (commented out since R doesn't have Stata-style labels)
# df <- df %>% rename(`Selection and No Moral Hazard` = Difference_Profits)

# Plot the Difference in Profits vs. Interest
library(ggplot2)

ggplot(df, aes(x = Interest, y = Difference_Profits)) +
  geom_line() +
  labs(title = "Selection and No Moral Hazard", x = "Interest Rate", y = "Difference in Profits") +
  theme_minimal()
################################### NO SELECTION ##################################

# Compute average repayment rate conditional on contract received
mean_Repayment_Rate_T_10 <- mean(df$Repayment_Rate[df$sec_treat == 1], na.rm = TRUE)
mean_Repayment_Rate_C_10 <- mean(df$Repayment_Rate[df$sec_treat == 0], na.rm = TRUE)

# Initialize matrix A for difference in profits without selection
A <- matrix(NA, nrow = 22, ncol = 1)

# Loop through interest rates from 10 to 31
for (j in 10:31) {
  n <- j - 9  # Adjusting index for 1-based indexing
  
  # Calculate Profit Per Person for Treatment and Control groups (without selection)
  Profit_Per_Person_T_j <- mean_Repayment_Rate_T_10 * Ideal_NPV_Treat[n] - 8000
  Profit_Per_Person_C_j <- mean_Repayment_Rate_C_10 * Ideal_NPV_Control[1] - 8000
  Difference_Profits_NS_j <- Profit_Per_Person_T_j - Profit_Per_Person_C_j
  
  # Store in matrix A
  A[n, 1] <- Difference_Profits_NS_j
}

# Add Difference in Profits NO SELECTION to dataframe
df$Difference_Profits_NS <- NA
df$Interest_NS <- NA

# Loop through interest rates to assign values
for (j in 10:31) {
  n <- j - 9
  df$Interest_NS[df$Number == n] <- j
  df$Difference_Profits_NS[df$Interest_NS == j] <- A[n, 1]
}

# Plot both Difference in Profits with and without selection
ggplot(df, aes(x = Interest)) +
  geom_line(aes(y = Difference_Profits, color = "Selection")) +
  geom_line(aes(y = Difference_Profits_NS, color = "No Selection")) +
  labs(title = "Difference in Profits: Selection vs. No Selection", 
       x = "Interest Rate", 
       y = "Difference in Profits") +
  scale_color_manual(values = c("Selection" = "blue", "No Selection" = "red")) +
  theme_minimal()

################################### NO SELECTION BUT INCLUDING PREFERENCE##################################

# Compute average repayment rate conditional on contract received for treatment and control groups
mean_Repayment_Rate_T_10 <- mean(df$Repayment_Rate[df$sec_treat == 1], na.rm = TRUE)
mean_Repayment_Rate_C_10 <- mean(df$Repayment_Rate[df$sec_treat == 0], na.rm = TRUE)

# Initialize matrix A for Difference in Profits No Selection with Preferences
A <- matrix(NA, nrow = 22, ncol = 1)

# Loop through interest rates from 10 to 31
for (j in 10:31) {
  n <- j - 9  # Adjusting index for 1-based indexing
  
  # Compute Profit Per Person for Treatment and Control groups
  Profit_Per_Person_T_j <- mean_Repayment_Rate_T_10 * Ideal_NPV_Treat[n] - 8000
  Profit_Per_Person_C_j <- mean_Repayment_Rate_C_10 * Ideal_NPV_Control[1] - 8000
  
  # Compute Difference in Profits No Selection with Preferences
  Difference_Profits_NSP_j <- mean_Prefer_Grace[n] * (Profit_Per_Person_T_j - Profit_Per_Person_C_j)
  
  # Store in matrix A
  A[n, 1] <- Difference_Profits_NSP_j
}

# Add Difference in Profits No Selection with Preferences to dataframe
df$Difference_Profits_NSP <- NA

# Loop through interest rates to assign values
for (j in 10:31) {
  n <- j - 9
  df$Difference_Profits_NSP[df$Interest_NS == j] <- A[n, 1]
}

# Plot all three cases: Selection, No Selection, and No Selection with Preferences
ggplot(df, aes(x = Interest)) +
  geom_line(aes(y = Difference_Profits, color = "Selection")) +
  geom_line(aes(y = Difference_Profits_NS, color = "No Selection")) +
  geom_line(aes(y = Difference_Profits_NSP, color = "No Selection with Preferences")) +
  labs(title = "Difference in Profits: Selection vs. No Selection vs. No Selection with Preferences", 
       x = "Interest Rate", 
       y = "Difference in Profits") +
  scale_color_manual(values = c("Selection" = "blue", "No Selection" = "red", "No Selection with Preferences" = "green")) +
  theme_minimal()

################################### WITH MORAL HAZARD AND SELECTION ##################################

# Define constant k
k <- 0.46

# Generate repayment rates with moral hazard adjustments
df$Repayment_Rate_10 <- df$Received_Amount_NPV / df$Pay_Due_Each_Repayment_NPV
df$Repayment_Rate_11 <- (df$Repayment_Rate_10 * (100 - k)) / 100

# Loop to generate Repayment_Rate_12 to Repayment_Rate_31
for (i in 12:31) {
  m <- i - 1
  df[[paste0("Repayment_Rate_", i)]] <- (df[[paste0("Repayment_Rate_", m)]] * (100 - k)) / 100
}

# Initialize matrices and vectors
mean_Repayment_Rate_PT_GT_MH <- numeric(22)
mean_Repayment_Rate_PT_GC_MH <- numeric(22)
B <- matrix(NA, nrow = 22, ncol = 1)

# Loop through interest rates to compute mean repayment rates with moral hazard
for (j in 10:31) {
  n <- j - 9  # Adjust index for 1-based indexing
  
  # Compute mean repayment rate for treatment group (with moral hazard)
  treatment_group_filtered <- df %>% filter(Prefer_NoGrace == 0, sec_treat == 1, Interest_Rate >= (1 + j / 100), !is.na(Interest_Rate))
  if (nrow(treatment_group_filtered) > 0) {
    mean_Repayment_Rate_PT_GT_MH[n] <- mean(treatment_group_filtered[[paste0("Repayment_Rate_", j)]], na.rm = TRUE)
  } else {
    mean_Repayment_Rate_PT_GT_MH[n] <- NA
  }
  
  # Compute mean repayment rate for control group (without moral hazard)
  control_group_filtered <- df %>% filter(Prefer_NoGrace == 0, sec_treat == 0, Interest_Rate >= (1 + j / 100), !is.na(Interest_Rate))
  if (nrow(control_group_filtered) > 0) {
    mean_Repayment_Rate_PT_GC_MH[n] <- mean(control_group_filtered$Repayment_Rate, na.rm = TRUE)
  } else {
    mean_Repayment_Rate_PT_GC_MH[n] <- NA
  }
  
  # Compute profit per person with moral hazard
  Profit_Per_Person_PT_GT_MH_j <- mean_Repayment_Rate_PT_GT_MH[n] * Ideal_NPV_Treat[n] - 8000
  Profit_Per_Person_PT_GC_MH_j <- mean_Repayment_Rate_PT_GC_MH[n] * Ideal_NPV_Control[1] - 8000
  
  # Compute Difference in Profits with Moral Hazard
  Difference_Profits_MH_j <- mean_Prefer_Grace[n] * (Profit_Per_Person_PT_GT_MH_j - Profit_Per_Person_PT_GC_MH_j)
  
  # Store in matrix B
  B[n, 1] <- Difference_Profits_MH_j
}

# Add Difference in Profits with Moral Hazard to dataframe
df$Difference_Profits_MH <- NA

# Loop through interest rates to assign values
for (j in 10:31) {
  n <- j - 9
  df$Difference_Profits_MH[df$Interest == j] <- B[n, 1]
}

# Update Interest values with APR equivalents
APRInt <- c(0.175, 0.195, 0.21, 0.23, 0.25, 0.268, 0.287, 0.306, 0.325, 0.345, 0.364, 0.384, 
            0.404, 0.424, 0.444, 0.465, 0.485, 0.506, 0.527, 0.547, 0.568, 0.59)

for (j in 10:31) {
  n <- j - 9
  df$Interest[df$Interest == j] <- APRInt[n]
}

df$Interest <- df$Interest * 100  # Convert to percentage

# Update variable labels (R doesn't have direct labels, so we use renaming for clarity)
df <- df %>% rename(
  "No Selection and No Moral Hazard but with Preferences" = Difference_Profits_NSP,
  "Selection and Moral Hazard" = Difference_Profits_MH,
  "Selection and No Moral Hazard" = Difference_Profits
)

# Plot the three cases with line styles similar to Stata
ggplot(df, aes(x = Interest)) +
  geom_path(aes(y = `No Selection and No Moral Hazard but with Preferences`, color = "No Selection & No Moral Hazard (w/ Preferences)")) +
  geom_path(aes(y = `Selection and Moral Hazard`, color = "Selection & Moral Hazard"), linetype = "dashed") +
  geom_path(aes(y = `Selection and No Moral Hazard`, color = "Selection & No Moral Hazard"), linetype = "dotdash") +
  labs(title = "Difference in Profits: Selection, Moral Hazard & Preferences", 
       x = "Percent APR", 
       y = "Difference in Profits per Client",
       color = "Legend") +  # Legend title
  scale_color_manual(values = c("No Selection & No Moral Hazard (w/ Preferences)" = "darkblue", 
                                "Selection & Moral Hazard" = "darkred", 
                                "Selection & No Moral Hazard" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom",          # Move legend below the plot
        legend.title = element_text(size = 10),  # Smaller legend title
        legend.text = element_text(size = 8),    # Smaller legend text
        legend.key.size = unit(0.4, "cm"),       # Reduce legend key size
        legend.spacing.y = unit(0.2, "cm")) +    # Reduce vertical spacing in legend
  guides(color = guide_legend(ncol = 1)) # Force legend into a single column

ggsave("./output/Figure_4.pdf", width = 10, height = 6)

