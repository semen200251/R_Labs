#task 3

load(file='C:/Proteomics/data/trades.RData')
data <- select(bind_rows(trades), -geo)
data <- data[str_detect(data$indic_et, "Imports in|Exports in"), ]

dat_wide <- pivot_wider(data, names_from = indic_et, values_from = values)
colnames(dat_wide)

gr_sum <- aggregate(dat_wide[[4]], list(dat_wide$time), sum)
names(gr_sum) <- c("Year", "Sum.Exp")

min_exp_ind <- which(gr_sum == min(gr_sum[["Sum.Exp"]]), arr.ind = TRUE)
same_without_min <- gr_sum[-min_exp_ind, ]

ggplot(gr_sum, aes(x = Year, y = Sum.Exp)) + geom_line() + geom_point() +
  geom_text(data = same_without_min, aes(x = Year, y = Sum.Exp, label = Sum.Exp),
            vjust = 0, hjust=1, nudge_x = 100, nudge_y = 33000, size = 3.5) +
  geom_point(data = gr_sum, aes(x = gr_sum[min_exp_ind[1,1], min_exp_ind[1,2] - 1],
                                   y = gr_sum[min_exp_ind[1,1], min_exp_ind[1,2]]),
             size = 8, shape = "*", color = "red") +
  geom_text(data = gr_sum[min_exp_ind[1,1], ], aes(x = Year, y = Sum.Exp, label = Sum.Exp),
            color = "red", size = 3.5, vjust = 0, hjust = 1, nudge_x = 100, nudge_y = -33000, check_overlap = TRUE) +
  labs(title = "Total export changes", x = "Year", y = "Value, million of ECU/EURO")

#task 12

data1 <- read.table(file = "C:/Proteomics/data/data1.txt")
data2 <- read.csv('C:/Proteomics/data/data2.csv', sep = ',')

head(data1)
colnames(data1) <- data1[1, ]
data1 <- data1[-1, ] 
head(data1)

data2 <- as.data.frame(t(data2))
head(data2)
colnames(data2) <- data2[1, ]
data2 <- data2[-1, ] 
head(data2)

fix_data2 <- function(data){
  for (name in colnames(data)) {
    data[[name]] <- gsub(" ", "", data[[name]])
  }
  return(data)
}

data2 <- fix_data2(data2)

df <- cbind(data1,data2)
df_without_NA <- na.omit(df) 
rownames(df_without_NA) <- 1:nrow(df_without_NA)
df_without_NA[ , -1] <- as.data.frame(lapply(df_without_NA[ , -1], as.numeric))

df_without_NA <- select(df_without_NA, Sample, GenmBMatur, everything())

dat_long1 <- pivot_longer(df_without_NA, GenmBMatur:Height, names_to = "Feature", values_to = "Values")

ggplot(dat_long1, aes(x = Values)) +
  geom_histogram(fill = "grey20", col = "white", binwidth = 5) +
  facet_wrap("Feature") +
  labs(title = "Sample histogram", x = "Feature values", y = "Count of values")

ggplot(dat_long1, aes(x = Values)) + geom_density(alpha = 0.2, fill = "red") +
  facet_wrap("Feature") +
  labs(title = "Sample density", x = "Feature values", y = "Density")

dat_long2 <- pivot_longer(df_without_NA, Protein:Oil, names_to = "Feature", values_to = "Values")

ggplot(dat_long2, aes(x = Values)) +
  geom_histogram(fill = "grey20", col = "white", binwidth=1) +
  facet_wrap("Feature") +
  labs(title = "Sample histogram", x = "Feature values", y = "Count of values")

ggplot(dat_long2, aes(x = Values)) + geom_density(alpha = 0.2, fill = "red") +
  facet_wrap("Feature") +
  labs(title = "Sample density", x = "Feature values", y = "Density")

df_without_NA$GrowthType <- as.factor(df_without_NA$GrowthType)

ggplot(df_without_NA, aes(x = MaturType, fill = GrowthType)) +
  geom_histogram(col = "white", binwidth = 1) + 
  labs(title = "Distribution GrowthType for each MaturType", x = "MaturType values", y = "Count of MaturType values")

ggplot(df_without_NA, aes(x = Oil)) +
  geom_histogram(aes(y = ..density..), alpha = 0.9, fill = "grey20", col = "white", binwidth = 0.5) +
  geom_density(alpha = 0.2, fill = "red") +
  labs(title = "Oil sample histogram, density", x = "Oil values", y = "Density values")

ggplot(df_without_NA, aes(y = Oil)) + geom_boxplot(fill='#de3163', color="black") +
  labs(title = "Oil sample boxplot", y = "Oil values")
