## GG Paper
# Federico Tomasetto 
# 2024/11/26

rm(list = ls(all.names = TRUE)) # will clear all objects, including hidden objects
gc()

library(tidyverse)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper/gg.fulldata.csv')      
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

# library(raster)
# gg1br <- brick(gg1[,c()])

plot(gg1$NDVI)

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
model3 <- aov(Mean.m.2 ~ Ryegrass.cultivar * year + Sowing.rate..kg.ha., data = gg1)

library(AICcmodavg)
model.set <- list(model1, model2, model3)
model.names <- c("two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)

summary(model3)
TukeyHSD(model)

#Visualize grass grub infestation levels
#gg1$gg_risk_label[gg1$gg_risk_label=="0"] <- "Low"
#gg1$gg_risk_label[gg1$gg_risk_label=="1"] <- "High"

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

#Principal Component Analysis (check Spatial data analysis in ecology and agriculture using R)
std.gg <-data.frame(gg1[,c(16:25)])
gg.pca <- stats::princomp(~ . , data=std.gg, cor=TRUE, scores=TRUE) 
summary(gg.pca)
screeplot(gg.pca, type="lines")
#get_eigenvalue(gg.pca)

#3D Visualization
library(rgl)

get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

plot3d(gg.pca$scores[,1:3], col=get_colors(gg1$dslg_groups), type="s", radius = 0.15, box=FALSE)
dev.off()

#First PCA plot idea (a bit confusing)
gg.pca <- prcomp(~ . , data=std.gg, scale. = TRUE) 
plot(gg.pca$x[,1],gg.pca$x[,2])
text(gg.pca$x[,1],gg.pca$x[,2], gg1$Ryegrass.cultivar, cex=0.7, pos=4, col="red")


#Second plotting PCA method
library(ggrepel)
ggplot(gg1, aes(gg.pca$x[,2], gg.pca$x[,1], label = Ryegrass.cultivar)) +
  geom_text_repel() +
  geom_point(color = 'red') +
  theme_classic(base_size = 16)

#Using a Principal Component Analysis data visualization
#library("devtools")
#install_github("kassambara/factoextra")
library("factoextra")

fviz_pca_ind(gg.pca, geom="point",  habillage=gg1$Ryegrass.cultivar)
fviz_pca_ind(gg.pca, geom="point",  habillage=gg1$gg_sample_Date)

gg1$cultdate <- interaction(gg1$Ryegrass.cultivar, gg1$gg_sample_Date)
fviz_pca_ind(gg.pca, geom="point", habillage=gg1$cultdate)

#look at the days since grazing in a PCA

#Using Partial Least Squares (PLS)
library("guidedPLS")
plsda_out1 <- PLSSVD(X=as.matrix(gg1[,c(16:25)]), Y=as.matrix(gg1$year), deflation = TRUE)
plot(plsda_out1$scoreX, col=sort(factor(gg1$year)), main="PLSDA Vegetation Indeces", pch=16)
legend(x = "top", legend = sort(unique(gg1$year)), 
       fill = 1:sort(unique(gg1$year)), horiz=TRUE, bty = "n", cex = 0.9)

#PLS with low levels of infestation
gg1low <- gg1 %>% filter(risk_level=="Low")
plsda_out2 <- PLSSVD(X=as.matrix(gg1low[,c(16:25)]), Y=as.matrix(gg1low$year), deflation = TRUE)
plot(plsda_out2$scoreX, col=sort(factor(gg1low$year)), main="PLSDA VIs - low level", pch=16)
legend(x = "top", legend = sort(unique(gg1low$year)), 
       fill = 1:sort(unique(gg1low$year)), horiz=TRUE, bty = "n", cex = 0.9)

#PLS with high levels of infestation
gg1high <- gg1 %>% filter(risk_level=="High")
plsda_out3 <- PLSSVD(X=as.matrix(gg1high[,c(16:25)]), Y=as.matrix(gg1high$year), deflation = TRUE)
plot(plsda_out3$scoreX, col=sort(factor(gg1high$year)), main="PLSDA VIs - high level", pch=16)
legend(x = "top", legend = sort(unique(gg1high$year)), 
       fill = 1:sort(unique(gg1high$year)), horiz=TRUE, bty = "n", cex = 0.9)

#collapse groups of days since last grazing

#library(devtools)
#install_github("mixOmicsTeam/mixOmics") #https://mixomics-users.discourse.group/t/customize-plotindiv-plot-using-plsda-with-mixomics/932
library(mixOmics)
library(pls)
vi_data <- gg1[,c(16:25)]
class_label <- gg1[,c(31)]

plsda_out2 <- plsda(vi_data, class_label, max.iter = 10000, ncomp=3)
plotIndiv(plsda_out2, ind.names = TRUE, ellipse = TRUE, legend = TRUE, title="PLSDA Vegetation Indeces for grazing groups")
(vip_scores <- vip(plsda_out2))
important_vis <- vi_data[vip_scores>1,]

#Visualisation of this importance #1
plotLoadings(plsda_out2, contrib = 'max', method = 'median', comp = 1)

#Visualisation of this importance #2
library(RColorBrewer)
coul <- brewer.pal(10, "Paired") 

barplot(vip_scores,
        beside = TRUE, col = coul,
        ylim = c(0, 2.7), 
        legend = TRUE, 
        args.legend = list(bty = "n", x = "top", ncol = 3),
        main = "Variable Importance in the PLSDA", font.main = 4)

#PLSDA with two groups of infestation levels
table(gg1$gg_risk_label) 

gg1low <- gg1 %>% filter(gg_risk_label=="Low")
gg1high <- gg1 %>% filter(gg_risk_label=="High")

plsda_out3 <- plsda(X=gg1low[,c(16:25)], Y=gg1low$dslg_groups, max.iter = 10000, ncomp=3)
plotIndiv(plsda_out3, ind.names = TRUE, ellipse = TRUE, legend = TRUE, title="PLSDA Vegetation Indeces - Low infestation")

plsda_out4 <- plsda(X=gg1high[,c(16:25)], Y=gg1high$dslg_groups, max.iter = 10000, ncomp=3)
plotIndiv(plsda_out4, ind.names = TRUE, ellipse = TRUE, legend = TRUE, title="PLSDA Vegetation Indeces - High infestation")


#CART approach analysis
library(rpart)
gg.rp1 <- rpart(Mean.m.2 ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI + Lat + Long, data = gg1)
x11()
plot(gg.rp1)
text(gg.rp1)
summary(gg.rp1)

#Temporal and Spatial Regression analysis
model.ts <- lm(Mean.m.2 ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI + Lat + Long, data = gg1, na.action = na.exclude)
summary(model.ts)
plot(model.ts)

#To analyse autocorrelatin in R (https://rpubs.com/markpayne/164550)
par(mfrow=c(1,1))
plot(residuals(model.ts))

plot(residuals(model.ts),type="b")
abline(h=0,lty=3)
acf(residuals(model.ts), na.action = na.pass)

#Spatial GLM (https://pages.cms.hu-berlin.de/EOL/gcg_quantitative-methods/Lab15_SpatialRegression.html)
#gg1$residuals <- residuals(model.ts)
#gg1$fitted <- fitted(model.ts)

#Map plots sampled 
library(sf)

ggplot(gg1, aes(Lat, Long)) + 
  geom_point(size = .25, show.legend = FALSE) +
  coord_quickmap()

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

#from here.....

# legend("topleft",                    # Add legend to plot
#        legend = c(unique(gg1$year)),
#        col = 1:4,
#        pch = 16,
#        horiz = TRUE)


# fviz_pca_biplot(res.pca,
#                 labelsize=3,
#                 addEllipses = T,
#                 repel=F,
#                 # Individuals
#                 geom.ind = "point",
#                 geom.var = c("point", "text"),
#                 fill.ind = data$Species,
#                 pointshape = 21 ,
#                 pointsize = 2,
#                 alpha.ind=0.4,
#                 # Variables
#                 alpha.var =1, 
#                 legend.title = list(fill = "Species"))+

#p_title <- 'Vegetation Indeces from Planet Lab' 

#p1 <- ggplot() +
#      theme_bw() +
#      geom_point(data = gg1, aes(x = Blue, y = Mean.m.2), pch = 21, size = 3, alpha = .7, colour = 'grey50', fill = 'chartreuse4') +
#      geom_smooth(data = gg1, aes(x = Blue, y = Mean.m.2), method = 'lm') +
#      labs(title = p_title)+
#      facet_wrap(~year, ncol = 2)