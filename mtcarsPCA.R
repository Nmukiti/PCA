install.packages("devtools")
library(devtools)
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(mtcars.pca, labels=rownames(mtcars))

# Extract variable loadings
loadings <- mtcars.pca$rotation[, 1]

# Create a data frame with variable names and loadings
variables <- data.frame(Variable = colnames(mtcars)[1], Loadings = loadings)

# Order variables by absolute loadings in descending order
variables <- variables[order(abs(variables$Loadings), decreasing = TRUE), ]

# View the variables contributing to PC1
print(variables)

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3),
                    "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe",
                                                                                        3))
ggbiplot(mtcars.pca,ellipse=TRUE, labels=rownames(mtcars),
         groups=mtcars.country)

ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4), labels=rownames(mtcars),
         groups=mtcars.country)


spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)
mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcars.country, "Jupiter")
mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)
ggbiplot(mtcarsplus.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle =
           FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"),
         groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet",
                                               "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")


s.sc <- scale(t(spacecar[c(1:7,10,11)]), center= mtcars.pca$center)
s.pred <- s.sc %*% mtcars.pca$rotation
mtcars.plusproj.pca <- mtcars.pca
mtcars.plusproj.pca$x <- rbind(mtcars.plusproj.pca$x, s.pred)
ggbiplot(mtcars.plusproj.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE,
         circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"),
         groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet",
                                               "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample projected")+
  theme_minimal()+
  theme(legend.position = "bottom")