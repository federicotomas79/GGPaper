## ATECH-D-25-02171
# Owner: Federico Tomasetto 

library(tidyverse)

#check the command here()

setwd("")

gg1 <- read.csv('...gg.fulldata.csv')      
glimpse(gg1)

#Calculate different gg risk levels
hist(gg1$Mean.m.2)
length(gg1$Mean.m.2) #1070

length(which(gg1$Mean.m.2 <100)) #307
length(which(gg1$Mean.m.2 <150)) #423
length(which(gg1$Mean.m.2 <200)) #543

# Mutate values and assign risk levels
gg1 <- gg1 %>%
   mutate(risk_level = case_when(
        gg1$Mean.m.2 <= 150 ~ "Low Risk",
       TRUE ~ "High Risk"
     )
   )
 
glimpse(gg1)

#Convert into risk levels into factors
gg1 <- mutate_at(gg1, vars(risk_level), as.factor)
gg1 <- mutate_at(gg1, vars(year), as.factor)
gg1 <- mutate_at(gg1, vars(Ryegrass.cultivar), as.factor)

#MSAVI = modified soil-adjusted vegetation index (index less sensitive to chlorophyll effects, more responsive to green LAI variations and more resistant to soil and atmosphere effects)
#NGRDI = Normalized Green–Red Difference Index indicates the colour of a pixel (i.e., greenish or reddish)
#ReNDVI = Red-edge Normalized Vegetation Index approaches 1 when the crops are dense and the RENDVI approaches 0 when the crops are thin

#Calculate and add difference in sampling days
gg.sample.days <- as.Date(as.character(gg1$gg_sample_Date), format="%d/%m/%Y")
unique(gg.sample.days)
rs.sample.days <- as.Date(as.character(gg1$rs_sample_Date), format="%d/%m/%Y")
unique(rs.sample.days)

diff.days.sample <- difftime(rs.sample.days, gg.sample.days, units = "days")
unique(diff.days.sample)
gg1$diff.days.sample <- as.numeric(diff.days.sample)
glimpse(gg1)

#Graphs for difference days between rs and gg sample
#x11()
# Getting rid of the gap in 2013

gg1 %>% filter(year != 2013) %>%
ggplot(aes(x = factor(year), y = diff.days.sample, fill=diff.days.sample)) +
  geom_point(stat = 'identity', color="grey", show.legend = FALSE) +
  theme_bw() +
  labs(y = "Difference in days (RS images)", x = "Sampling year")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="point", color="red", show.legend = FALSE) +
  scale_y_continuous(breaks = (c(0, -50, -100, -150, -200)), labels = c("Day GG sampled", -50, -100, -150, -200)) 
  

#Visualize grass grub densities by months
#range(gg1$Mean.m.2) #0-688 
ggplot(gg1, aes(x = gg.sample.days, y = Mean.m.2)) +
  geom_point(stat = 'identity', color="grey") +
  theme_bw() +
  scale_x_date(date_labels=("%d-%b-%Y"), 
               breaks = unique(gg.sample.days)) +
  labs(title = "Grass grub per year", y = "Value", x = "Date") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="pointrange", color="red")

#With log transformation 
ggplot(gg1, aes(x = gg.sample.days, y = log(Mean.m.2))) +
  geom_point(stat = 'identity', color="grey") +
  theme_bw() +
  scale_x_date(date_labels=("%d-%b-%Y"), 
               breaks = unique(gg.sample.days)) +
  labs(title = "Grass grub per year", y = "Log (Value)", x = "Date") +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="pointrange", color="red")

#Visualize grass grub densities by cultivar per year
#x11()
ggplot(na.omit(gg1), aes(x=factor(year), y = Mean.m.2, fill = Ryegrass.cultivar)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(fill = "Ryegrass cultivar", x = "Year", y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  geom_jitter(width=0.1, alpha=0.1) +
  facet_wrap(~Sowing.rate..kg.ha., labeller = labeller(Sowing.rate..kg.ha. = c("6" = "Sowing rate = 6 kg/ha", "30" = "Sowing rate = 30 kg/ha")))

#A two-way ANOVA to assess the main effects of each factor and their interaction (https://www.scribbr.com/statistics/two-way-anova/#:~:text=The%20two%2Dway%20ANOVA%20will,the%20dependent%20variable%20(average%20crop)
model1 <- aov(Mean.m.2 ~ Ryegrass.cultivar + Sowing.rate..kg.ha., data = gg1)
model2 <- aov(Mean.m.2 ~ Ryegrass.cultivar * Sowing.rate..kg.ha., data = gg1)
model3 <- aov(Mean.m.2 ~ Ryegrass.cultivar * year + Sowing.rate..kg.ha. * year, data = gg1)
model4 <- aov(Mean.m.2 ~ Ryegrass.cultivar * year * Sowing.rate..kg.ha., data = gg1) #three-way anova
summary(model4)

library(AICcmodavg)
model.set <- list(model1, model2, model3, model4)
model.names <- c("two.way", "interaction", "blocking", "three-way")
aictab(model.set, modnames = model.names)

summary(model4)

#Visualize grass grub infestation levels
table(gg1$risk_level) #High Risk  Low Risk 
                      #647        427

ggplot(na.omit(gg1), aes(x=factor(risk_level), y=Mean.m.2, color=factor(risk_level))) + 
  geom_violin(trim=FALSE) +
  scale_x_discrete(limits = rev, labels=c("Low", "High")) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  #stat_summary(fun=mean, geom="point", size=2) +
  guides(colour = "none") +    
  geom_jitter(width=0.1, alpha=0.1) +
  labs(x = "Risk levels", y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  geom_boxplot(width=0.1)

ggplot(na.omit(gg1), aes(x=factor(risk_level), y=Mean.m.2, color=factor(risk_level))) + 
  geom_violin(trim=FALSE) +
  scale_x_discrete(limits = rev, labels=c("Low", "High")) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  #stat_summary(fun=mean, geom="point", size=2) +
  guides(colour = "none") +    
  geom_jitter(width=0.1, alpha=0.1) +
  labs(x = "Risk levels", y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  geom_boxplot(width=0.1) +
  facet_wrap(~year)

#Group data by days since grazing
table(gg1$days_snc_lst_graze)
groups4 = cut(gg1$days_snc_lst_graze, breaks = c(0, 4, 17, 30, 194), labels = FALSE, include.lowest = TRUE)
print(groups4)
table(groups4)
gg1$dslg_groups <- groups4

#Create data table for exploration
library(DT)
gg1.datatable <- data.frame(gg1[,c(8:9,12:13,29)])
datatable(gg1.datatable, selection="multiple")

#visualize all variables in the dataset
library(reshape2)

melt.gg1 <- melt(gg1[, c(16:25)])
melt.gg1$year <- gg1$year

ggplot(data = melt.gg1, aes(x = value, group=as.factor(year), color=as.factor(year))) +
  stat_density(geom = "path", position = "identity", linewidth=1.5) + 
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size = 13))+
  facet_wrap(~variable, scales = "free") + 
  labs(color='Years') +
  ggtitle("Vegetation Indices")

#Create correlations of all of the numerical variables
#First way
library(GGally)

gg1.data <- data.frame(gg1[,c(9,16:25)]) 
#x11()
ggpairs(gg1.data, title="Correlogram - Vegetation Indexes from Planet Lab vs. grass grub densities", upper = list(continuous = wrap("cor", size = 3)))

#Second way (https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html)
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly nameshttp://127.0.0.1:45347/graphics/plot_zoom_png?width=1200&height=900
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(scale(data.frame(gg1[,c(16:25)])), 20)

#Temporal and Spatial Regression analysis
model.ts <- lm(Mean.m.2 ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI + Lat + Long, data = gg1, na.action = na.exclude)
summary(model.ts)
plot(model.ts)

#To analyse autocorrelation in R (https://rpubs.com/markpayne/164550)
par(mfrow=c(1,1))
plot(residuals(model.ts))

plot(residuals(model.ts),type="b")
abline(h=0,lty=3)
acf(residuals(model.ts), na.action = na.pass)

#Create a spatial dataframe
library(spdep)

gg1.sliced <- slice(gg1, 1:(n()-200))

gg1.xy <- cbind(gg1.sliced[,c(30:31)])
str(gg1.xy)

gg1.nb <- dnearneigh(gg1.xy, d1 = 0, d2 = 50)
str(gg1.nb, list.len=5, give.attr = F)

gg1.lw <- nb2listw(gg1.nb, style = "W")
str(gg1.lw$weights, list.len=5, give.attr = F) 

moran.test(gg1.sliced$Mean.m.2, listw = gg1.lw, randomisation = FALSE)

#Moran with permutation (using a Monte Carlo test)
gg.bperm <- moran.mc(gg1.sliced$Mean.m.2, listw = gg1.lw, nsim = 999)
gg.bperm

gg.cor3 <- sp.correlogram(neighbours = gg1.nb, var = gg1.sliced$Mean.m.2, order = 3)
print(gg.cor3)

#Measure of influence (https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html)
library(olsrr)

#Check the R book 
influence.measures(model.ts)
influence.measures(model.ts)$is.inf
lm.influence(model.ts)$hat > 0.1

sort(lm.influence(model.ts)$hat, decreasing = TRUE)

#Using Random Forest (https://uc-r.github.io/random_forests)
gg2<-gg1[complete.cases(gg1),]
glimpse(gg2)
table(gg2$risk_level)

#To graph risk levels over the years to consider it as a categorical variable
ggplot(gg2, aes(x=factor(risk_level), y=Mean.m.2, color=factor(risk_level))) + 
#  geom_violin(trim=FALSE) +
  scale_x_discrete(limits = rev, labels=c("Low", "High")) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  #stat_summary(fun=mean, geom="point", size=2) +
  guides(colour = "none") +    
  geom_jitter(width=0.1, alpha=0.1) +
  labs(x = "Risk levels", y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  geom_boxplot(width=0.1) +
  facet_wrap(~year)

library(caret) # For easy train/test split

set.seed(123) # For reproducibility
index.rl <- createDataPartition(gg2$risk_level, p = 0.7, list = FALSE) # 70% train

train_data <- gg2[index.rl, ]
table(train_data$risk_level) #to check that there even values for the response variables

test_data <- gg2[-index.rl, ]
table(test_data$risk_level)

library(randomForest)

#Tuning Random Forest
set.seed(123)
train_data_rf <- train_data[,c(5, 12, 16:25, 30:31, 34)]
features <- setdiff(names(train_data_rf), "risk_level")

m2 <- tuneRF(
  x          = train_data_rf[features],
  y          = train_data_rf$risk_level,
  ntreeTry   = 1000,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

#Run Random Forest (Excluding Ryegrass cultivars and years)
model_rf <- randomForest(risk_level ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI + Lat + Long + Ryegrass.cultivar, data = train_data, 
                         ntree=1000, proximity = TRUE, mtry=10)
print(model_rf)

plot(model_rf)

predicted_classes_rf <- predict(model_rf, newdata = test_data)
confusionMatrix(predicted_classes_rf, test_data$risk_level)   

confusionMatrix(predicted_classes_rf, test_data$risk_level, positive="Low Risk")

# To present confusion matrix
# ConfusionTableR (https://cran.r-project.org/web/packages/ConfusionTableR/vignettes/ConfusionTableR.html)

round(importance(model_rf), 1) # For Random Forest
varImpPlot(model_rf)

#Using the caret Random Forest method (without ryegrass cultivar)
rf_model <- caret::train(risk_level ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI + Lat + Long,
                         data = train_data,
                         method = "rf",
                         metric = "Accuracy",
                         preProcess = c("center", "scale"),
                         trControl = trainControl(method = "cv"))

rf_model

ggplot <- ggplot2::ggplot(varImp(rf_model, scale = FALSE))

ggplot + theme_bw()


rf_class <- predict(rf_model, newdata = test_data, type = "raw") 
predictions <- cbind(data.frame(train_preds=rf_class, 
                                test_data$risk_level))

#To print out a confusion matrix
library(ConfusionTableR)

cm <- caret::confusionMatrix(predictions$train_preds, predictions$test_data.risk_level)
print(cm)

x11()
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('Risk Levels Confusion Matrix', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='darkorange4')
  text(195, 435, 'High Levels', cex=1.4)
  rect(250, 430, 340, 370, col='green4')
  text(295, 435, 'Low Levels', cex=1.4)
  text(125, 370, 'Predicted', cex=1.5, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.6, font=2)
  rect(150, 305, 240, 365, col='green4')
  rect(250, 305, 340, 365, col='darkorange4')
  text(140, 400, 'High Levels', cex=1.4, srt=90)
  text(140, 335, 'Low Levels', cex=1.4, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='black')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='black')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "Details", xaxt='n', yaxt='n', cex.main = 1.6)
  text(10, 85, names(cm$byClass[1]), cex=1.4, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 2), cex=1.3)
  text(30, 85, names(cm$byClass[2]), cex=1.4, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 2), cex=1.3)
  text(50, 85, names(cm$byClass[5]), cex=1.4, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 2), cex=1.3)
  text(70, 85, names(cm$byClass[6]), cex=1.4, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 2), cex=1.3)
  text(90, 85, names(cm$byClass[7]), cex=1.4, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 2), cex=1.3)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.4, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 2), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.4, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 2), cex=1.4)
}  

draw_confusion_matrix(cm)
 