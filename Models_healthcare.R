rm(list = ls())
#Importing data in R
setwd("C:/Users/golla/Music/MdTerm/Shashank/Statistical-Data-Mining-Coursework-master")
df<-read.csv("Health_Data_Final_updated.csv")

#Checking the structure of the data
str(df)

#Converting the variables in factors
df$Country<-as.factor(df$Country)
df$Year<-as.factor(df$Year)

#Removing unwanted variables
df$key<-NULL
install.packages('rmarkdown')
df$...1<-NULL

#Plotting Histogram for target variables: Life expectancy & Health expense for year 2015
df1<-df[df$Year==2015,]
hist(df1$Life_Expectancy_at_Birth,main='Life Expectancy 2015')
hist(log(df1$Life_Expectancy_at_Birth),main='log of Life Expectancy 2015')
hist(df1$ExpenditurePerCapita,main = 'Expenditure per capita 2015')
hist(log(df1$ExpenditurePerCapita),main = 'log of Expenditure per capita 2015')

#Correlation plot for both the models
cor_df1<-subset(df,select=c(ExpenditurePerCapita,Diagnostic_Exams,Hospitals,PercPopulationabove65,Private_Insurance,Public_Insurance,hospital_employment,tot_equipment))
#installing package pheatmap
install.packages('pheatmap')
library(pheatmap)
#Correlation plot for variables in health expense
m<-cor(cor_df1)
pheatmap(m, display_numbers = T, color = colorRampPalette(c('white','red'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15,main='Correlation plot for Healthcare Expenditure')
#Correlation plot for variables in life expectancy
cor_df2<-subset(df,select=c(Life_Expectancy_at_Birth,Hospitals,ExpenditurePerCapita	,Mean_Schooling_Years,	hospital_employment,tot_equipment,medical_grads,nurse_grads,death_by_cancer,death_by_circular,death_by_accident,death_by_respirat))
m1<-cor(cor_df2)
pheatmap(m1, display_numbers = T, color = colorRampPalette(c('white','red'))(100), cluster_rows = F, cluster_cols = F, fontsize_number = 15,main='Correlation plot for Life Expectancy')

#Models for Health Expenses
hex<-subset(df,select=c(Country,Year,Diagnostic_Exams,ExpenditurePerCapita	,Hospitals,	Life_Expectancy_at_Birth,	PercPopulationabove65,Physicians,	Private_Insurance,	Public_Insurance	,	Mean_Schooling_Years,	NationalIncome,	hospital_employment,tot_equipment,medical_grads	,nurse_grads))
#OLS model for Health expense
#installing package plm
install.packages('plm')
library(plm)
d<-plm.data(hex,index=c("Country","Year"))
pooling_HE<-plm(log(ExpenditurePerCapita)~Diagnostic_Exams+Hospitals+	PercPopulationabove65+	Private_Insurance+	Public_Insurance	+	hospital_employment+tot_equipment	,data=d,model = "pooling")
summary(pooling_HE)

#Residual plot for Health expense
plot(d$Country,pooling_HE$residuals,las = 2,main='Pooling model residual plot by country')
plot(d$Year,pooling_HE$residuals,las = 2,main='Pooling model residual plot by year')

plmtest(pooling_HE)

#plm fixed model for Health expense
pooling_HE_Fixed<-plm(log(ExpenditurePerCapita)~Diagnostic_Exams+Hospitals+	PercPopulationabove65+	Private_Insurance+	Public_Insurance	+	hospital_employment+tot_equipment,data=d,model = "within")
summary(pooling_HE_Fixed)

#Residual plot
plot(d$Country,pooling_HE_Fixed$residuals,las = 2,main='Fixed model residual plot by country')
plot(d$Year,pooling_HE_Fixed$residuals,las = 2,main='Fixed model residual plot by year')


#plm random model for Health expense
pooling_HE_Random<-plm(log(ExpenditurePerCapita)~Diagnostic_Exams+Hospitals+	PercPopulationabove65+	Private_Insurance+	Public_Insurance	+	hospital_employment+tot_equipment,data=d,model = "random")
summary(pooling_HE_Random)

#Residual plot
plot(d$Country,pooling_HE_Random$residuals,las = 2,main='Random model residual plot by country')
plot(d$Year,pooling_HE_Random$residuals,las = 2,main='Random model residual plot by year')


#Stargazer to compare all the models
library(stargazer)
stargazer(pooling_HE,pooling_HE_Fixed,pooling_HE_Random,type="text")

#comparing fixed and random models
phtest(pooling_HE_Fixed,pooling_HE_Random)

#Life Expectancy Models
#dataset of life expectancy
Lex<-subset(df,select=c(Country,Year,ExpenditurePerCapita,ExpenditurePerCapita	,Hospitals,	Life_Expectancy_at_Birth,Physicians	,	Mean_Schooling_Years,	NationalIncome,	hospital_employment,tot_equipment,medical_grads	,nurse_grads,death_by_cancer,	death_by_circular,death_by_accident,death_by_respirat))
dle<-plm.data(Lex,index=c("Country","Year"))

#OLS model for Life expectancy
pooling_LE<-plm(Life_Expectancy_at_Birth~Hospitals+ExpenditurePerCapita	+	Mean_Schooling_Years+	hospital_employment+tot_equipment+medical_grads	+nurse_grads+death_by_cancer+death_by_circular+death_by_accident+death_by_respirat,data=dle,model = "pooling")
summary(pooling_LE)

#Residual plot
plot(dle$Country,pooling_LE$residuals,las = 2,main='Pooling model residual plot by country')
plot(dle$Year,pooling_LE$residuals,las = 2,main='Pooling model residual plot by year')

#to check if panel model
plmtest(pooling_LE)

#fixed effect model for Life expectancy
pooling_LE_Fixed<-plm(Life_Expectancy_at_Birth~Hospitals+ExpenditurePerCapita	+	Mean_Schooling_Years+	hospital_employment+tot_equipment+medical_grads	+nurse_grads+death_by_cancer+death_by_circular+death_by_accident+death_by_respirat,data=dle,model = "within")
summary(pooling_LE_Fixed)
fixef(pooling_LE_Fixed)

#Residual plot
plot(dle$Country,pooling_LE_Fixed$residuals,las = 2,main='Fixed model residual plot by country')
plot(dle$Year,pooling_LE_Fixed$residuals,las = 2,main='Fixed model residual plot by year')


#Random effect model for Life expectancy
pooling_LE_Random<-plm(Life_Expectancy_at_Birth~Hospitals+ExpenditurePerCapita	+	Mean_Schooling_Years+	hospital_employment+tot_equipment+medical_grads	+nurse_grads+death_by_cancer+death_by_circular+death_by_accident+death_by_respirat,data=dle,model = "random")
summary(pooling_LE_Random)

#Residual plot
plot(dle$Country,pooling_LE_Random$residuals,las = 2,main='Random model residual plot by country')
plot(dle$Year,pooling_LE_Random$residuals,las = 2,main='Random model residual plot by year')


#stargazer to compare life expectancy models
stargazer(pooling_LE,pooling_LE_Fixed,pooling_LE_Random,type ='text')

#comparing fixed and random model of Life expectancy
phtest(pooling_LE_Fixed,pooling_LE_Random)






