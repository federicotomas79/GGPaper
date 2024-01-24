## GG Paper
# Federico Tomasetto 
# 2022/10/4

#rm(list = ls())

library(tidyverse)

setwd("C:/Users/TOMASETTOF/OneDrive - AgResearch/Documents/GitHub/GGPaper")

gg1 <- read.csv('C:/Users/TOMASETTOF/OneDrive - AgResearch/ggj_paper/materials/rs_gg_mdl_data_2022_01_07_plus_2012_FT.csv')      
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

#Graphs for difference days between rs and gg sample
#x11()
ggplot(gg1, aes(x = year, y = diff.days.sample, fill=diff.days.sample)) +
  geom_point(stat = 'identity', color="grey", show.legend = FALSE) +
  theme_bw() +
  labs(title = "Difference in days between gg sampling and rs data", y = "Difference in days (RS images)", x = "Sampling year")+
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="point", color="red", show.legend = FALSE) +
  scale_y_continuous(breaks = (c(0, -50, -100, -150, -200)), labels = c("Day GG sampled", -50, -100, -150, -200))

#Visualize grass grub densities by months
#range(gg1$Mean.m.2) #0-688 
ggplot(gg1, aes(x = gg.sample.days, y = Mean.m.2)) +
  geom_point(stat = 'identity', color="grey") +
  theme_bw() +
  scale_x_date(date_labels=("%d-%b-%Y"), 
               breaks = unique(gg.sample.days)) +
  labs(title = "Grass grub per year", y = "Log (Value)", x = "Date") +
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
ggplot(gg1, aes(x=factor(year), y = Mean.m.2, fill = Ryegrass.cultivar)) + 
  geom_boxplot() + 
  theme_bw() +
  labs(fill = "Ryegrass cultivar", y= expression("Larvae per"~ m^2)) +
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "Grass grub by cultivar per year", x = "Year") +
  facet_wrap(~Sowing.rate..kg.ha., labeller = labeller(Sowing.rate..kg.ha. = c("6" = "Sowing rate = 6 kg/ha", "30" = "Sowing rate = 30 kg/ha")))

#Visualize grass grub infestation levels
gg1$gg_risk_label[gg1$gg_risk_label=="0"] <- "Low"
gg1$gg_risk_label[gg1$gg_risk_label=="1"] <- "High"

ggplot(gg1, aes(x=factor(gg_risk_label), y=Mean.m.2, color=factor(gg_risk_label))) + 
  geom_violin(trim=FALSE) +
  scale_x_discrete(limits = rev) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  #stat_summary(fun=mean, geom="point", size=2) +
  guides(colour = "none") +    
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "Grass grub by infestation levels", x = "Risk levels", y= expression("Larvae per"~ m^2)) +
  geom_boxplot(width=0.1)

ggplot(gg1, aes(x=factor(gg_risk_label), y=Mean.m.2, color=factor(gg_risk_label))) + 
  geom_violin(trim=FALSE) +
  scale_x_discrete(limits = rev) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  #stat_summary(fun=mean, geom="point", size=2) +
  guides(colour = "none") +    
  geom_jitter(width=0.1, alpha=0.1) +
  labs(title = "Grass grub by infestation levels", x = "Risk levels", y= expression("Larvae per"~ m^2)) +
  geom_boxplot(width=0.1) +
  facet_wrap(~year)

#Create data table for exploration
library(DT)
gg1.datatable <- data.frame(gg1[,c(8:9,12:13,29)])
datatable(gg1.datatable, selection="multiple")

#visualize all variables in the dataset
library(reshape2)

melt.gg1 <- melt(gg1[, c(9,16:25,29)])
melt.gg1$year <- gg1$year

ggplot(data = melt.gg1, aes(x = value, group=year, colour=year)) +
  stat_density(geom = "path", position = "identity") + 
  theme_bw() +
  scale_color_gradientn(colours = rainbow(5))+
  facet_wrap(~variable, scales = "free") 

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

mosthighlycorrelated(scale(data.frame(gg1[,c(16:25)])), 10)

#Principal Component Analysis (check Spatial data analysis in ecology and agriculture using R)
std.gg <-data.frame(gg1[,c(16:25)])
gg.pca <- prcomp(~ . , data=std.gg, scale. = TRUE) 
summary(gg.pca)
screeplot(gg.pca, type="lines")

#First PCA plot idea (a bit confusing)
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

#Using Partial Least Squares (PLS)
library("guidedPLS")
plsda_out1 <- PLSSVD(X=as.matrix(gg1[,c(16:25)]), Y=as.matrix(gg1$year), deflation = TRUE)
plot(plsda_out1$scoreX, col=factor(gg1$year), main="PLS-DA Vegetation Indeces", pch=16)
legend(x = "top", legend = sort(unique(gg1$year)), fill = 1:sort(unique(gg1$year)), horiz=TRUE, bty = "n", cex = 0.9)

#1library(devtools)
#install_github("mixOmicsTeam/mixOmics") #https://mixomics-users.discourse.group/t/customize-plotindiv-plot-using-plsda-with-mixomics/932
library(mixOmics)
plsda_out2 <- plsda(X=gg1[,c(16:25)], Y=gg1$year, max.iter = 10000, ncomp=2)
plotIndiv(plsda_out2, ind.names = TRUE, ellipse = TRUE, legend = TRUE, title="PLS-DA Vegetation Indeces")

#CART approach analysis
library(rpart)

gg.rp1 <- rpart(Mean.m.2 ~ Blue + GLI + Green + IR + MSAVI + NDVI + NGRDI + Red + RedEdge + reNDVI, data = gg1)
#x11()
plot(gg.rp1)
text(gg.rp1)
summary(gg.rp1)

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