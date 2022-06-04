df1 <- df %>% group_by(Year, Age)
df2 <- df1 %>% summarise(
  Population.Count = sum(Population.Count)
)