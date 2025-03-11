means <- df %>%
  summarise(
    `Inventory and raw materials*` = mean(Inventory_Raw_Mat/10, na.rm = TRUE),
    `Equipment*` = mean(Equipment, na.rm = TRUE)/10,
    `Operating costs` = mean(Other_Bus_Cost, na.rm = TRUE),
    `Home repairs` = mean(Repairs_Repair_Only, na.rm = TRUE),
    `Human capital` = mean(Human_Capital, na.rm = TRUE),
    `Money for relending` = mean(Re_Lent, na.rm = TRUE),
    `Savings` = mean(Savings, na.rm = TRUE),
    `Food and durable consumption` = mean(Food_And_Durable, na.rm = TRUE),
    `Utilities, taxes, and rent` = mean(Utilities_Taxes_Rent, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Mean")

desired_order <- c("Inventory and raw materials*", 
                   "Equipment*", 
                   "Operating costs", 
                   "Home repairs", 
                   "Human capital", 
                   "Money for relending", 
                   "Savings", 
                   "Food and durable consumption", 
                   "Utilities, taxes, and rent")

# Add category type for color differentiation
means <- means %>%
  mutate(Type = ifelse(Category %in% c("Inventory and raw materials*", "Equipment*", "Operating costs"), 
                       "Business use", "Nonbusiness use"), Category = factor(Category, levels = desired_order))

# Create bar chart
ggplot(means, aes(x = Category, y = Mean, fill = Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Business use" = "lightgray", "Nonbusiness use" = "black")) +
  theme_minimal() +
  labs(title = "Loan Use Categories",
       x = "Loan use category\n(*Scaled by a factor of 10)",
       y = "Loan expenditure",
       fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(color = "black", fill = "white"))

ggsave("./output/Figure_1.pdf", width = 10, height = 6)
