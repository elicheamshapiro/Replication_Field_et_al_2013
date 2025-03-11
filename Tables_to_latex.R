#Table 1 to latex
table_1 <- read_csv("./output/Table_1.csv")
latex_table1 <- xtable(table_1)
sink("./output/latex_table_1.tex")
print(latex_table1, include.rownames = FALSE, booktabs = TRUE)
sink()

# Table 3 to latex
table_3 <- read_csv("./output/Table_3.csv")
latex_table3 <- xtable(table_3)
sink("./output/latex_table_3.tex")
print(latex_table3, include.rownames = FALSE, booktabs = TRUE)
sink()