#Libraries
library(tidyverse)

######Fancy Linear Regression Plot Function###
ggplotRegression = function(linearModel) {
  ggplot(linearModel$model, aes_string(x = names(linearModel$model)[2], y = names(linearModel$model)[1])) + 
    geom_point(size = 2) + 
    stat_smooth(method = 'lm', col = "blue") +  
    theme_bw() + 
    labs(title = bquote(R^2== ~.(signif(summary(linearModel)$adj.r.squared, 5))~"&"~"p-value"==~.(signif(summary(linearModel)$coef[2,4], 10)))) +
    theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
          axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
          axis.text.y = element_text(size  = 24), 
          axis.title=element_text(size=24),
          legend.title=element_text(size=24), 
          legend.text=element_text(size=18), 
          legend.position = "bottom") 
  
}

#Make data frames
#read in NSF data and count awards per school per year
NSFData = read_csv("~/SACNAS/all_nsf_hon_and_reg_clean.csv") %>%
  group_by(bs_school, award_type) %>%
  count() %>%
  ungroup() %>%
  mutate(year = gsub('\\D+','', award_type)) %>% #make a column with just year
  as.data.frame()

#count number of hon/awarded applications per year
perYear = NSFData %>%
  group_by(year) %>%
  count() 

#make counts proportions
countApps = NSFData %>% 
  group_by(award_type, year) %>% 
  count() %>%
  mutate(propAwards = n/perYear$n[match(year, perYear$year)])

####Visualizing our new data frame
ggplot(NSFData, aes(x=award_type, y=n)) + 
  geom_bar(stat = "identity") +
  labs(x = "Award Type", y = "Number of Awards", title = "NSF Awards and Honorable Mentions (2015-2017)") +
  theme_bw() +
  theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
        axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
        axis.text.y = element_text(size  = 24), 
        axis.title=element_text(size=24),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=18), 
        legend.position = "bottom") 

#Plot the proportional data
ggplot(data = countApps, aes(x=award_type, y=propAwards)) +
  geom_bar(stat = "identity") +
  labs(x = "Award Type", y = "Proportion of Awards", title = "NSF Awards and Honorable Mentions (2015-2017)") +
  theme_bw() +
  theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
        axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
        axis.text.y = element_text(size  = 24), 
        axis.title=element_text(size=24),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=18), 
        legend.position = "bottom") 

#Plot the proportional data with color by year
library(RColorBrewer) #library to get color palettes
ggplot(data = countApps, aes(x=award_type, y=propAwards, fill=year)) +
  geom_bar(stat = "identity") +
  labs(x = "Award Type", y = "Proportion of Awards", title = "NSF Awards and Honorable Mentions (2015-2017)") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
        axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
        axis.text.y = element_text(size  = 24), 
        axis.title=element_text(size=24),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=18), 
        legend.position = "right")

####Running t-test

#select columns of interest
honMen = NSFData %>% 
  filter(award_type== "2016_hon") %>% 
  select(n) %>% 
  unlist()
honAward = NSFData %>% 
  filter(award_type== "2016_reg") %>% 
  select(n) %>% 
  unlist()

#run a t-test
t.test(x = honAward, y = honMen)

####Visualizing our data and making predictions

#reshape our data frame to turn award types into columns
reshapeNSFData = NSFData %>% 
  select(-c(year)) %>% 
  pivot_wider(names_from = award_type, values_from = n, values_fill = list(n=0)) %>% 
  as.data.frame() 

#Without the regression line
ggplot(reshapeNSFData, aes(x=reshapeNSFData$`2016_reg`, y=reshapeNSFData$`2017_reg`)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "Count of Awardees in 2016", y = "Count of Awardees in 2017", title = "Correlation with Number of NSF Awards (2016 & 2017)") +
  theme_bw() +
  theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
        axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
        axis.text.y = element_text(size  = 24), 
        axis.title=element_text(size=24),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=18), 
        legend.position = "bottom") 

#With the regression line
ggplot(reshapeNSFData, aes(x=reshapeNSFData$`2016_reg`, y=reshapeNSFData$`2017_reg`)) + 
  geom_point() + 
  theme_bw() + 
  geom_smooth(method="lm") +
  labs(x = "Count of Awardees in 2016", y = "Count of Awardees in 2017", title = "Correlation with Number of NSF Awards (2016 & 2017)") +
  theme_bw() +
  theme(plot.title=element_text(size =18, face = "bold", hjust=0.5), 
        axis.text.x = element_text(size  = 24, vjust=1, hjust=0.5), 
        axis.text.y = element_text(size  = 24), 
        axis.title=element_text(size=24),
        legend.title=element_text(size=24), 
        legend.text=element_text(size=18), 
        legend.position = "bottom") 

#linear regression with 2016 as predictor and 2017 outcome/response
model2017 = lm(data = reshapeNSFData, formula =  reshapeNSFData$`2017_reg` ~ reshapeNSFData$`2016_reg`)

#Use summary to check whether correlation is significant
summary(model2017)

#Regression with R2 and p-value
ggplotRegression(model2017)  +
  labs(x = "Count of Awardees in 2016", y = "Count of Awardees in 2017")
