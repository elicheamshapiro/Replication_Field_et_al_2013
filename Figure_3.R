# Sorting the dataframe
df <- df %>% arrange(sec_treat, Distance_First_Last_Paid)

# Creating 'Order' variable
df <- df %>% group_by(sec_treat) %>% mutate(Order = row_number())

# Creating 'max_Number_People' using group-wise max function
df <- df %>% group_by(sec_treat, Distance_First_Last_Paid) %>%
  mutate(max_Number_People = max(Order)) %>%
  ungroup()

# Creating Percent_Repaid_Delay and Percent_Repaid_NoDelay
df <- df %>%
  mutate(
    Percent_Repaid_Delay = ifelse(sec_treat == 1, (425 - max_Number_People) / 425, NA),
    Percent_Repaid_NoDelay = ifelse(sec_treat == 0, (420 - max_Number_People) / 420, NA)
  )

# Setting observations manually
df <- df %>% add_row(Distance_First_Last_Paid = 1, Percent_Repaid_Delay = 1, sec_treat = 1)
df <- df %>% add_row(Distance_First_Last_Paid = 550, Percent_Repaid_Delay = 0.01176471, sec_treat = 1)
df <- df %>% add_row(Distance_First_Last_Paid = 1, Percent_Repaid_NoDelay = 1, sec_treat = 0)

# Sorting the dataframe again
df <- df %>% arrange(sec_treat, Distance_First_Last_Paid)

# Plot with points and dotted path, plus solid red vertical lines
ggplot(df, aes(x = Distance_First_Last_Paid)) +
  geom_point(aes(y = Percent_Repaid_Delay, color = "Grace Period")) + 
  geom_path(aes(y = Percent_Repaid_Delay, color = "Grace Period"), linetype = "solid") +
  geom_point(aes(y = Percent_Repaid_NoDelay, color = "Regular")) + 
  geom_path(aes(y = Percent_Repaid_NoDelay, color = "Regular"), linetype = "solid") +
  geom_vline(xintercept = c(308, 364), color = "red", linetype = "solid", size = 1) +  # Solid red vertical lines
  scale_color_manual(values = c("Grace Period" = "blue", "Regular" = "red")) +
  labs(
    title = "Fraction of Clients Who Have Not Repaid in Full\nRelative to the Date of First Installment",
    x = "Days from First Meeting to Finished Repaying",
    y = "",
    color = "Treatment Group"
  ) +
  scale_x_continuous(breaks = seq(0, 600, by = 50), minor_breaks = seq(0, 600, by = 25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_minimal()
ggsave("./output/Figure_3.pdf", width = 10, height = 6)