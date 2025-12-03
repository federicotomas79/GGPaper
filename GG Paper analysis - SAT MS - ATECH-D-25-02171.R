## ATECH-D-25-02171
# Owner: Federico Tomasetto 

library(tidyverse)
library(spdep)
library(caret)
library(randomForest)
library(AICcmodavg)
library(olsrr)
library(DT)
library(reshape2)
library(GGally)
library(ConfusionTableR)

# Load and prepare data in one pipeline
gg1 <- read_csv('...gg.fulldata.csv') %>%
  mutate(
    # Convert to factors
    risk_level = factor(if_else(Mean.m.2 <= 150, "Low Risk", "High Risk")),
    year = factor(year),
    Ryegrass.cultivar = factor(Ryegrass.cultivar),
    
    # Date conversions and calculations
    gg_sample_Date = dmy(gg_sample_Date),
    rs_sample_Date = dmy(rs_sample_Date),
    diff.days.sample = as.numeric(difftime(rs_sample_Date, gg_sample_Date, units = "days")),
    
    # Group days since grazing
    dslg_groups = cut(days_snc_lst_graze, 
                      breaks = c(0, 4, 17, 30, 194), 
                      labels = FALSE, 
                      include.lowest = TRUE)
  )

# Density exploration
hist(gg1$Mean.m.2)

# Summary statistics (more efficient than multiple length() calls)
gg1 %>%
  summarise(
    total = n(),
    below_100 = sum(Mean.m.2 < 100),
    below_150 = sum(Mean.m.2 < 150),
    below_200 = sum(Mean.m.2 < 200)
  )

# Graph 1: Difference days between RS and GG sample
gg1 %>% 
  filter(year != 2013) %>%
  ggplot(aes(x = year, y = diff.days.sample)) +
  geom_point(color = "grey") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "point", color = "red") +
  scale_y_continuous(breaks = c(0, -50, -100, -150, -200), 
                     labels = c("Day GG sampled", -50, -100, -150, -200)) +
  labs(y = "Difference in days (RS images)", x = "Sampling year") +
  theme_bw()

# Graph 2: Grass grub densities by date
ggplot(gg1, aes(x = gg_sample_Date, y = Mean.m.2)) +
  geom_point(color = "grey") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", color = "red") +
  scale_x_date(date_labels = "%d-%b-%Y", breaks = unique(gg1$gg_sample_Date)) +
  labs(title = "Grass grub per year", y = "Value", x = "Date") +
  theme_bw()

# Graph 3: Log-transformed densities
ggplot(gg1, aes(x = gg_sample_Date, y = log(Mean.m.2))) +
  geom_point(color = "grey") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", color = "red") +
  scale_x_date(date_labels = "%d-%b-%Y", breaks = unique(gg1$gg_sample_Date)) +
  labs(title = "Grass grub per year", y = "Log (Value)", x = "Date") +
  theme_bw()

# Graph 4: Densities by cultivar per year
gg1 %>%
  drop_na() %>%
  ggplot(aes(x = year, y = Mean.m.2, fill = Ryegrass.cultivar)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.1) +
  facet_wrap(~Sowing.rate..kg.ha., 
             labeller = labeller(Sowing.rate..kg.ha. = 
                                   c("6" = "Sowing rate = 6 kg/ha", 
                                     "30" = "Sowing rate = 30 kg/ha"))) +
  labs(fill = "Ryegrass cultivar", x = "Year", 
       y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  theme_bw()

# ANOVA models
model1 <- aov(Mean.m.2 ~ Ryegrass.cultivar + Sowing.rate..kg.ha., data = gg1)
model2 <- aov(Mean.m.2 ~ Ryegrass.cultivar * Sowing.rate..kg.ha., data = gg1)
model3 <- aov(Mean.m.2 ~ Ryegrass.cultivar * year + Sowing.rate..kg.ha. * year, data = gg1)
model4 <- aov(Mean.m.2 ~ Ryegrass.cultivar * year * Sowing.rate..kg.ha., data = gg1)

model.set <- list(model1, model2, model3, model4)
model.names <- c("two.way", "interaction", "blocking", "three-way")
aictab(model.set, modnames = model.names)
summary(model4)

# Graph 5: Risk level distributions
table(gg1$risk_level)

gg1 %>%
  drop_na() %>%
  ggplot(aes(x = fct_rev(risk_level), y = Mean.m.2, color = risk_level)) + 
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.1) +
  geom_boxplot(width = 0.1) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Risk levels", 
       y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  guides(colour = "none") +
  theme_bw()

# Graph 6: Risk levels by year
gg1 %>%
  drop_na() %>%
  ggplot(aes(x = fct_rev(risk_level), y = Mean.m.2, color = risk_level)) + 
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.1) +
  geom_boxplot(width = 0.1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~year) +
  labs(x = "Risk levels", 
       y = bquote(italic(C.)~italic(giveni) ~ "larvae per" ~ m^2)) +
  guides(colour = "none") +
  theme_bw()

# Data table for exploration
gg1_datatable <- gg1 %>% select(8:9, 12:13, 29)
datatable(gg1_datatable, selection = "multiple")

# Graph 7: Vegetation indices by year
gg1 %>%
  select(year, 16:25) %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, color = factor(year), group = factor(year))) +
  stat_density(geom = "path", position = "identity", linewidth = 1.5) + 
  facet_wrap(~variable, scales = "free") +
  labs(color = 'Years', title = "Vegetation Indices") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        text = element_text(size = 13))

# Correlogram
gg1_data <- gg1 %>% select(9, 16:25)
ggpairs(gg1_data, 
        title = "Correlogram - Vegetation Indexes from Planet Lab vs. grass grub densities",
        upper = list(continuous = wrap("cor", size = 3)))

# Most highly correlated function
mosthighlycorrelated <- function(mydataframe, numtoreport) {
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  
  as.data.frame(as.table(cormatrix)) %>%
    set_names("First.Variable", "Second.Variable", "Correlation") %>%
    arrange(desc(abs(Correlation))) %>%
    head(numtoreport)
}

mosthighlycorrelated(scale(gg1 %>% select(16:25)), 20)

# Temporal and Spatial Regression
model.ts <- lm(Mean.m.2 ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + 
                 Red + RedEdge + reNDVI + Lat + Long, 
               data = gg1, na.action = na.exclude)
summary(model.ts)
plot(model.ts)

# Autocorrelation analysis
par(mfrow = c(1, 1))
plot(residuals(model.ts), type = "b")
abline(h = 0, lty = 3)
acf(residuals(model.ts), na.action = na.pass)

# Spatial analysis
gg1_sliced <- gg1 %>% slice(1:(n() - 200))
gg1_xy <- gg1_sliced %>% select(Lat, Long) %>% as.matrix()

gg1_nb <- dnearneigh(gg1_xy, d1 = 0, d2 = 50)
gg1_lw <- nb2listw(gg1_nb, style = "W")

moran.test(gg1_sliced$Mean.m.2, listw = gg1_lw, randomisation = FALSE)

# Moran Monte Carlo test
gg_bperm <- moran.mc(gg1_sliced$Mean.m.2, listw = gg1_lw, nsim = 999)
gg_bperm

gg_cor3 <- sp.correlogram(neighbours = gg1_nb, var = gg1_sliced$Mean.m.2, order = 3)
print(gg_cor3)

# Influence measures
influence.measures(model.ts)

# Random Forest Analysis
gg2 <- gg1 %>% drop_na()
table(gg2$risk_level)

# Train/test split
set.seed(123)
index_rl <- createDataPartition(gg2$risk_level, p = 0.7, list = FALSE)
train_data <- gg2[index_rl, ]
test_data <- gg2[-index_rl, ]

table(train_data$risk_level)
table(test_data$risk_level)

# Tune Random Forest
set.seed(123)
train_data_rf <- train_data %>% select(5, 12, 16:25, 30:31, 34)
features <- setdiff(names(train_data_rf), "risk_level")

m2 <- tuneRF(
  x = train_data_rf[features],
  y = train_data_rf$risk_level,
  ntreeTry = 1000,
  mtryStart = 5,
  stepFactor = 1.5,
  improve = 0.01,
  trace = FALSE
)

# Random Forest model
model_rf <- randomForest(
  risk_level ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + 
    Red + RedEdge + reNDVI + Lat + Long + Ryegrass.cultivar,
  data = train_data,
  ntree = 1000,
  proximity = TRUE,
  mtry = 10
)
print(model_rf)
plot(model_rf)

# Predictions and confusion matrix
predicted_classes_rf <- predict(model_rf, newdata = test_data)
confusionMatrix(predicted_classes_rf, test_data$risk_level)
confusionMatrix(predicted_classes_rf, test_data$risk_level, positive = "High Risk")

# Variable importance
round(importance(model_rf), 1)
varImpPlot(model_rf)

# Caret Random Forest
rf_model <- train(
  risk_level ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + 
    Red + RedEdge + reNDVI + Lat + Long,
  data = train_data,
  method = "rf",
  metric = "Accuracy",
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv")
)

rf_model

ggplot(varImp(rf_model, scale = FALSE)) + theme_bw()

# Final predictions
rf_class <- predict(rf_model, newdata = test_data, type = "raw")
predictions <- data.frame(
  train_preds = rf_class,
  actual = test_data$risk_level
)

cm <- confusionMatrix(predictions$train_preds, predictions$actual)
print(cm)

# Confusion matrix visualization function
draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1, 1, 2)))
  par(mar = c(2, 2, 2, 2))
  plot(c(100, 345), c(300, 450), type = "n", xlab = "", ylab = "", 
       xaxt = 'n', yaxt = 'n')
  title('Risk Levels Confusion Matrix', cex.main = 2)
  
  # Create matrix
  rect(150, 430, 240, 370, col = 'darkorange4')
  text(195, 435, 'High Levels', cex = 1.4)
  rect(250, 430, 340, 370, col = 'green4')
  text(295, 435, 'Low Levels', cex = 1.4)
  text(125, 370, 'Predicted', cex = 1.5, srt = 90, font = 2)
  text(245, 450, 'Actual', cex = 1.6, font = 2)
  rect(150, 305, 240, 365, col = 'green4')
  rect(250, 305, 340, 365, col = 'darkorange4')
  text(140, 400, 'High Levels', cex = 1.4, srt = 90)
  text(140, 335, 'Low Levels', cex = 1.4, srt = 90)
  
  # Add results
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex = 1.6, font = 2, col = 'black')
  text(195, 335, res[2], cex = 1.6, font = 2, col = 'black')
  text(295, 400, res[3], cex = 1.6, font = 2, col = 'black')
  text(295, 335, res[4], cex = 1.6, font = 2, col = 'black')