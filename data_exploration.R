# --------------------------------------------
# Script Name: data_exploration.R
# Purpose1: This scribes how to remove the sites with missing data of the  Doubs
#           dataset, and detect whether environmental factors are collinearity.
# Purpose2: Analysis on the relationships between fishes and environment
#           factors and visualize such relationships.


# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-04-15

# --------------------------------------------
# Load the package
library(ade4)
library(vegan)
library(ape)
library(dplyr)
# Load the required dataset
data("doubs")
doubs
str(doubs)
str(env)

fish <- doubs$fish
env <- doubs$env
spa <- doubs$xy

# --------------------------------------------
# remove the sites with missing data 
# --------------------------------------------
# 1.Check for missing values in the entire data frame
anyNA(doubs)

any_missing_in_data <- any(is.na(doubs))
cat("Does the data frame have missing data?", any_missing_in_data)
#No NA in the "doubs"
#If the data has misssing value, we can filter it by the following
doubs_no_na <- doubs %>%
  filter_all(all_vars(!is.na(.)))
#OR another method to filter
doubs_clean <- na.omit(doubs)

# 2.Know the species richness of each site
# 2.1 Check the sturcture for data frame of species firstly
colSums(doubs.fish)
summary(doubs.fish)
rowSums(fish) #Note:there are no species in site 8 

# 2.2 Next Illustrate the species richness of each site along the river

sit.pres <- apply(fish > 0, 1, sum)
sort(sit.pres)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(doubs.spe), cex=.8, col="red")
plot(spa, asp=1, main="Map of Species Richness", pch=21, col="white",
     bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)",
     ylab="y coordinate (km)")
lines(spa, col="light blue")
#Only the No.8 sample has no fish along the river

# 3.remove the No.8 site to clean the missing data 
fish <- fish[-8,]
env <- env[-8,]
spa <- spa[-8,]
View(fish)
knitr::kable(spe[1:9,1:5])

# --------------------------------------------
# Delete the collinear environmental factors by R mode
# --------------------------------------------
#R-mode (analysis of relationships among variables)
# Pearson r linear correlation
env.pearson<-cor(env) 
round(env.pearson, 2) #Rounds the coefficients to 2 decimal points 

# Kendall tau rank correlation
env.kendall<-cor(env, method="kendall") 
round(env.kendall, 2)

#Method2
library(stats)
op <- par(mfrow=c(1,1), pty="s")
pairs(doubs.env, panel=panel.smooth,
      diag.panel=panel.hist,
      main="Biplots with histograms and smooth surves")
par(op)

# Method3
# Reference:https://blog.csdn.net/weixin_39886469/article/details/111017010  
#           http://sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
library(Hmisc)
corr_matrix <- rcorr(as.matrix(env))
corr_matrix

print(class(corr_matrix))
print(corr_matrix)

library(corrplot)
corrplot(corr_matrix$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

corr <- cor(env) # 计算相关系数矩阵
corrplot(corr, method = "color") # 使用颜色绘制相关系数图

install.packages("PerformanceAnalytics", dependencies = TRUE)
library(PerformanceAnalytics)
chart.Correlation(env, histogram=T, pch=19)
#Note: there is a high collinearity between dfs and other environmental factors
# --------------------------------------------
# Analysis on the relationships between fishes and environment
# --------------------------------------------
# Selection of unconstrained ordination base on CDA analysis
dca <- decorana(fish)
print(dca)
# 3.0 < DCA1==3.855 < 4.0, RDA and CCA are both OK

# Method1: Using RDA
# 1.Standardize and transform the data
# 1.1 The Hellinger transformation of the coummunity species to correct for the double zero problem
fish.hel <- decostand(fish, "hellinger")
# Calculate distance matrix
fish.bchel<-vegdist(fish.hel, method="bray", binary=FALSE) 

# 1.2 The Z-score standardization of the environmental factor for normal distribution
env.z <- decostand(env, "standardize")
apply(env.z, 2, mean) # means = 0
apply(env.z, 2, sd) # standard deviations = 1
# OR same standardization with scale() 
env.z1 <- as.data.frame(scale(env))

# 1.3 Remove the environmental factor of the 'dfs',showing collinearity with other factors
env.z2 <- subset(env.z, select = -which(names(env.z) == "dfs"))

# 2. Run the RDA and select environmental variables
dim(fish.hel)
dim(env.z2)
rda_fish <- rda(fish.hel ~ ., data = env.z2)
summary(rda_fish)
rda_fish$call
#rda(formula = fish.hel ~ alt + slo + flo + pH + har + pho + nit + amm + oxy + bdo, data = env.z2)

# Forward selection of environmental variables
fwd.sel <- ordiR2step(rda(doubs.fish.hel ~ 1, data = doubs.env.z),
                      scope = formula(doubs.fish.rda), direction = "forward", R2scope = TRUE,
                      pstep = 1000, trace = FALSE)
fwd.sel$call
# rda(formula = doubs.fish.hel ~ alt + oxy + bdo, data = doubs.env.z)

# Run the new model between species and the selected environmental factors
rda_signif_fish <- rda(fish.hel ~ alt + oxy + bdo, data = env.z2)
# !变量待修改Check R^2 retreived from the rda result
R2 <- RsquareAdj(spe.rda)$r.squared # unadjusted R^2 
R2 
R2adj <- RsquareAdj(spe.rda)$adj.r.squared # adjusted R^2
R2adj 

# Check and test model significance.
anova.cca(rda_signif_fish, step = 1000)

# selected environmental variables significantly explain 54.0% (p = 0.001)

# Plot the RDA results
# plot RDA
# Scaling 1
ordiplot(doubs.fish.rda.signif, scaling = 1, main = "doubs RDA - Scaling 1")
# Scaling 2
ordiplot(doubs.fish.rda.signif, scaling = 2, main = "doubs RDA - Scaling 2")


# Triplot: sites, response variables and explanatory variables
# Scaling 1
plot(spe.rda, scaling=1, main="scaling 1 - wa scores")
spe.sc <- scores(spe.rda, choices=1:2, scaling=1, display="sp")
arrows(0,0,spe.sc[,1], spe.sc[,2], length=0, lty=1, col='red')

# Scaling 2
plot(spe.rda, main="scaling 2 - wa scores")
spe2.sc <- scores(spe.rda, choices=1:2, display="sp")  
arrows(0,0,spe2.sc[,1], spe2.sc[,2], length=0, lty=1, col='red')




# 假设env.z2是你的环境数据矩阵
pca_results <- prcomp(env.z2, scale = TRUE) # 对环境数据进行PCA分析

# 查看PCA结果
summary(pca_results)

# 绘制PCA结果的散点图或双标图来可视化变量之间的关系
biplot(pca_results, scale = 0, cex = 0.6)

# 你可以使用PCA的主成分作为新的预测变量进行RDA分析
# 这里假设你保留了前几个主成分，这些主成分解释了环境数据的大部分变异
rda_results <- rda(fish.hel ~ PC1 + PC2, data = as.data.frame(pca_results$x))


