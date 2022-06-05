df1 <- df %>% group_by(Year)
df1[is.na(df1)] <- 0
df2 <- df1 %>% summarise(
  Total = sum(Population.Count, na.rm=FALSE)
)