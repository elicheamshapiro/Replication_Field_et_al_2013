# Create Profits_Graph
df <- df %>% mutate(Profits_Graph = ifelse(Profit > 12000 & !is.na(Profit), 12000, Profit))

# Initialize new dataframe
profit_data <- data.frame(Profit_Value = seq(500, 12000, by = 500),
                          Profits_Treat = NA)

m <- 1
profit_values <- seq(500, 12000, by = 500)

for (i in profit_values) {
  j <- i - 500
  
  temp_df <- df %>% mutate(Profits = ifelse(Profits_Graph <= i & Profits_Graph >= j & sec_treat == 1, 1, NA))
  temp_df$Profits[!is.na(temp_df$Profits_Graph) & is.na(temp_df$Profits)] <- 0
  temp_df$Profits[temp_df$sec_treat == 0] <- NA
  
  profit_data$Profits_Treat[m] <- mean(temp_df$Profits, na.rm = TRUE)
  
  m <- m + 1
}

# Initialize new dataframe
profit_data <- data.frame(Profit_Value = seq(500, 12000, by = 500),
                          Profits_Treat = NA,
                          Profits_Control = NA)

m <- 1
profit_values <- seq(500, 12000, by = 500)

# Compute treatment group profits
for (i in profit_values) {
  j <- i - 500
  
  temp_df <- df %>% mutate(Profits = ifelse(Profits_Graph <= i & Profits_Graph >= j & sec_treat == 1, 1, NA))
  temp_df$Profits[!is.na(temp_df$Profits_Graph) & is.na(temp_df$Profits)] <- 0
  temp_df$Profits[temp_df$sec_treat == 0] <- NA
  
  profit_data$Profits_Treat[m] <- mean(temp_df$Profits, na.rm = TRUE)
  
  m <- m + 1
}

# Compute control group profits
m <- 1
for (i in profit_values) {
  j <- i - 500
  
  temp_df <- df %>% mutate(Profits = ifelse(Profits_Graph <= i & Profits_Graph >= j & sec_treat == 0, 1, NA))
  temp_df$Profits[!is.na(temp_df$Profits_Graph) & is.na(temp_df$Profits)] <- 0
  temp_df$Profits[temp_df$sec_treat == 1] <- NA
  
  profit_data$Profits_Control[m] <- mean(temp_df$Profits, na.rm = TRUE)
  
  m <- m + 1
}

# Reshape data for ggplot
profit_data_long <- profit_data %>%
  pivot_longer(cols = c(Profits_Treat, Profits_Control),
               names_to = "Group", values_to = "Density")

# Convert labels
profit_data_long$Group <- factor(profit_data_long$Group, 
                                 levels = c("Profits_Control", "Profits_Treat"),
                                 labels = c("Regular", "Grace period"))

# Plot the side-by-side bar chart
ggplot(profit_data_long, aes(x = factor(Profit_Value), y = Density, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "black")) +
  labs(title = "Distribution of profits for grace and nongrace period clients",
       x = "Long-run profits", y = "Density", fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./output/Figure_2.pdf", width = 10, height = 6)