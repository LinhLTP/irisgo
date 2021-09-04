setwd("/Users/admin/Documents/Linh-R Studio/Irisgo")

library(tidyverse)
library("readxl")
library("writexl")
library(tidyr)
library(dplyr)

data <- read_excel("/Users/admin/Documents/Linh-R Studio/Irisgo/IRISGO_data.xlsx")

# Long to wide
data_wide <- spread(data, "Measure Names","Measure Values")

# User ID 
# df <- data_wide %>% group_by(`User ID`) %>% summarise(Usage = sum(`Total Usage Time (minute)`), Learn = sum(`Total Learning Time (minute)`), Entertainment = sum(`Total Entertaining Time (minute)`), Discovery = sum(`Total Discovering Time (minute)`), Child = n()) %>% ungroup()

dfx <- data_wide %>% group_by(`User ID`) %>% summarise(Usage = sum(`Total Usage Time (minute)`,na.rm = T), Learn = sum(`Total Learning Time (minute)`,na.rm = T), Entertainment = sum(`Total Entertaining Time (minute)`,na.rm = T), Discovery = sum(`Total Discovering Time (minute)`,na.rm = T), Child = n()) # na.rm = thay the na = 0 


df3 <- data_wide %>% distinct(`User ID`, .keep_all = TRUE)
df2 <- data_wide[!duplicated(data_wide$`User ID`), ]
df4 <- df2 %>% select(`User ID`,`Day of User Joined Date`,`Number of Active Days`)

df5 <- merge(dfx,df4)
df51 <-  df5[complete.cases(df5),] # remove null / na valaue 

# Cluster 
df52 <- df51 %>% filter(Usage != "0")
df6 <- df52 %>% select(`User ID`, `Usage`, `Number of Active Days`)

df7 <-  df6[complete.cases(df6),]

df7 %>% mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) %>% select(-`User ID`) -> final_df_scaled

set.seed(29)

wss <- sapply(1:10, 
              function(k){kmeans(final_df_scaled %>% sample_frac(0.2), 
                                 k, nstart = 30)$tot.withinss})

u <- data.frame(k = 1:10, WSS = wss)

u %>% 
  ggplot(aes(k, WSS)) + 
  geom_line() + 
  geom_point() + 
  geom_point(data = u %>% filter(k == 3), color = "red", size = 3) + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) + 
  labs(title = "Figure 7: The Optimal Number of Clusters, Elbow Method", x = "Number of Clusters (K)") + 
  theme(panel.grid.minor = element_blank())

# Phân cụm với k = 3: 
set.seed(123)
km.res <- kmeans(final_df_scaled, 3, nstart = 30)

# Sử dụng kết quả phân cụm: 
df52 %>% 
  mutate(Group = km.res$cluster) %>% 
  mutate(Group = paste("Group", Group)) -> final_df_clustered

# # reorder column
final_df_clustered <- final_df_clustered[c(1,7,6,8,2,3,4,5,9)] # reorder columns
write_xlsx(final_df_clustered,"/Users/admin/Documents/Linh-R Studio/Irisgo/final_df_clustered.xlsx")

# reorder column
df5 <- df5[c(1,7,6,8,2,3,4,5)]
write_xlsx(df5,"/Users/admin/Documents/Linh-R Studio/Irisgo/df5.xlsx")

