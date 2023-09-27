#read file in R
getwd()
data = read.csv("Churn_Train.csv",header=TRUE)
data

#view data
view(data)
data$Total.Charges
#check missing values
is.na(data$Total.Charges)

#impute missing values with mean values
mean_data <- mean(data$Total.Charges, na.rm = TRUE)
data$Total.Charges[is.na(data$Total.Charges)] <- mean_data

#calculate descriptive statistics using describe()
describe(data)
describe(data, CustomerID, Monthly.Charges, Total.Charges) #select columns by name
describe(data, Monthly.Charges:Total.Charges) #select all columns between monthlyCharges and totalCharge
describe(data, (Monthly.Charges:Total.Charges)) #select all column except monthlyCharges until totalCharge

#test normality
normality(data)
normality(data, Monthly.Charges, Total.Charges) #select columns by name
normality(data, Monthly.Charges:Total.Charges) #select all columns between monthlyCharges and totalCharge
normality(data, (Monthly.Charges:Total.Charges)) #select all column except monthlyCharges until totalCharge

#plot normality
plot_normality(data,Monthly.Charges,Total.Charges)

#calculate correlation
correlate(data)
correlate(data, Monthly.Charges,Total.Charges) #select columns by name
correlate(data, Monthly.Charges:Total.Charges) #select all columns between monthlyCharges and totalCharge
correlate(data, (Monthly.Charges:Total.Charges)) #select all column except monthlyCharges until totalCharge

#plot correlate
data %>%
  data() %>%
  plot()

correlate(data,Monthly.Charges,Total.Charges) %>%
  plot()

#EDA used on target variable
categ <- target_by(data,Contract)

#EDA when target variable is categorical, predictor is numerical
cat_num <- relate(categ,Monthly.Charges)
cat_num
summary(cat_num)
plot(cat_num)

#EDA when target variable is categorical, predictor is categorical
cat_cat <- relate(categ,Internet.Service)
cat_cat
summary(cat_cat)
plot(cat_cat)

#EDA when target variable is numerical, predictor is numerical
num <- target_by(data,Monthly.Charges)
num_num <- relate(num,Total.Charges)
num_num
summary(num_num)
plot(num_num)

#EDA when target variable is numerical, predictor is categorical
num_cat <- relate(num,Senior.Citizen)
num_cat
summary(num_cat)
plot(num_cat)
