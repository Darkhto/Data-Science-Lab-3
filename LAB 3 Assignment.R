

#Checking Churn_Train for missing data
Churn_Train
summary(Churn_Train)
summary(is.na(Churn_Train))


#visualize the plot
ggplot(Churn_Train, aes(Churn_Train$Total.Charges)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Variable distribution") +
  theme_classic() +
  theme(plot.title = element_text(size = 30))




# simple value imputation (Replace value of null value with 0, mean and median)
value_imputed <- data.frame(
  original = Churn_Train$Total.Charges,
  imputed_zero = replace(Churn_Train$Total.Charges,
                         is.na(Churn_Train$Total.Charges), 0),
  
  imputed_mean = replace(Churn_Train$Total.Charges,
                         is.na(Churn_Train$Total.Charges), mean(Churn_Train$Total.Charges, na.rm = TRUE)),
  
  imputed_median = replace(Churn_Train$Total.Charges,
                           is.na(Churn_Train$Total.Charges), median(Churn_Train$Total.Charges, na.rm =TRUE))
)
value_imputed


#Histogram after imputation
h1 <- ggplot(value_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(value_imputed, aes(x = imputed_zero)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Zero-imputed distribution") +
  theme_classic()
h3 <- ggplot(value_imputed, aes(x = imputed_mean)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("Mean-imputed distribution") +
  theme_classic()
h4 <- ggplot(value_imputed, aes(x = imputed_median)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("Median-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)




#Impute Missing Values in R with MICE
library(mice)

churn_numeric <- Churn_Train %>%
  select(Tenure, Monthly.Charges, Total.Charges)
md.pattern(Churn_numeric)

mice_imputed <- data.frame(original = Churn_Train$Total.Charges, 
                           imputed_pmm = complete(mice(churn_numeric, method = "pmm"))$Total.Charges,
                           imputed_cart = complete(mice(churn_numeric, method = "cart"))$Total.Charges,
                           imputed_lasso = complete(mice(churn_numeric, method = "lasso.norm"))$Total.Charges)
mice_imputed


#Histogram after imputation
h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()

h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position =
                   "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()

h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position =
                   "identity") +
  ggtitle("LASSO-imputed distribution") +
  theme_classic()
plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)





#Activity 3 imputation with R missforest PackTotal.Charges
library(missForest)

missForest_imputed <- data.frame(
  original = churn_numeric$Total.Charges,
  imputed_missForest = missForest(churn_numeric)$ximp$Total.Charges
)

missForest_imputed


#Histogram after imputation
h1 <- ggplot(missForest_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position =
                   "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h2 <- ggplot(missForest_imputed, aes(x = imputed_missForest)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position =
                   "identity") +
  ggtitle("Miss Forest-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, nrow = 2, ncol = 2)


#Activity 4 Normalize data with scaling methods

log_scale = log(as.data.frame(Churn_Train$Total.Charges))

library(caret)
process <- preProcess(as.data.frame(Churn_Train$Total.Charges),
                      method=c("range"))
norm_scale <- predict(process, as.data.frame(Churn_Train$Total.Charges))

scale_data <- as.data.frame(scale(Churn_Train$Total.Charges))

scale_data


#Activity 5 Feature encoding

#Label Encoding
gender_encode <- ifelse(Churn_Train$Gender == "Female",1,0)
table(gender_encode)


#One-Hot Encoding
new_dat =
  data.frame(Churn_Train$Total.Charges,Churn_Train$Gender,Churn_Train$Tech.Support)
summary(new_dat)

library(caret)
dmy <- dummyVars(" ~ .", data = new_dat, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = new_dat))
glimpse(dat_transformed)


#Encoding Continuous (or Numeric) Variables
summary(new_dat$Churn_Train.Total.Charges)
bins <- c(-Inf, 399.3, 3786.6, Inf)
bin_names <- c("Low", "Mid50", "High")
new_dat$new_TotalCharges <- cut(new_dat$Churn_Train.Total.Charges, breaks = bins, labels = bin_names)

summary(new_dat$Churn_Train.Total.Charges)
summary(new_dat$new_TotalCharges)



