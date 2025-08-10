install.packages("ggpubr")
library(ggpubr)
library(rstatix)
library(ggplot2)
install.packages("MASS") 
install.packages("reshape2") 
install.packages("reshape") 

library(MASS) 
library(reshape2) 
library(reshape) 

set.seed(1234)
wdata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(wdata, 4)

ggdensity(wdata, x = "weight",
          add = "mean", rug = TRUE,
          color = "sex",
          palette = c("#00AFBB", "#E7B800"))

data("ToothGrowth")
df <- ToothGrowth
head(df, 4)

p <- ggboxplot(df, x = "dose", y = "len",
               color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "dose")
p

my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)
#####
load(file="NNS.RData")
load(file="Annotation.RData")
my_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3"))
melt_data <- melt(z_score_cluster, id = c("clusters")) 

base <- ggplot(melt_data, aes(clusters, value, color=clusters)) + 
  geom_boxplot(outlier.shape = NA) + 
  ylim(-3,4) +
  xlab("Clusters") + 
  ylab("zscore")
my_comparisons <- list( c("1", "2"), c("2", "3"), c("1", "3"))

base + facet_wrap(~variable, ncol = 4)+ stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 3)
# can't add p-value to each plot

#clusters - SUPP (OG VC)
#value - len
#?- group (grp1 grp2)
#variable - dose

# process colnames
colnames(melt_data) = c("clusters", "var", "value")
stat.test <- melt_data %>%
  group_by(var) %>% # dose
  t_test(value ~ clusters) %>% # len ~ supp
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()
stat.test 

bxp <- ggboxplot(
  melt_data, x = "clusters", y = "value", fill = "clusters", 
  facet.by = "var", ylim = c(-3, 6), outlier.shape = NA
)
stat.test <- stat.test %>% add_xy_position(x = "clusters") # x = "supp"
bxp + stat_pvalue_manual(stat.test, y.position = c(3,4,5))

# plot with p-value
bxp <- ggboxplot(
  melt_data, x = "clusters", y = "value", fill = "clusters", 
  facet.by = "var", ylim = c(-3, 6), outlier.shape = NA
)
stat.test <- stat.test %>% add_xy_position(x = "clusters") # x = "supp"
bxp + stat_pvalue_manual(
  stat.test, y.position = c(3,4,5),
  label = "{p.adj}" #{p.adj.signif}
) 

########### PMA vs feature
patientNum <- lapply(total$col1, function(x) unlist(str_split(x, "_"))[[1]])
total$patientNum <- patientNum
z_score <- z_score_cluster
z_score$patientNum <- total$patientNum
z_score$PMA <- total$PMA
melt_data <- melt(z_score, id = c("PMA","patientNum", "clusters"))
colnames(melt_data) = c("PMA", "patientNum", "clusters", "var", "value")
melt_data$value <- as.numeric(melt_data$value)
bxp <- ggscatter(
  melt_data, x = "PMA", y = "value",  
  facet.by = "var",
  color = "patientNum",
  ylab=FALSE,
  ylim=c(-3,3),
  nrow = 4,
  size = NA,
  add = "loess", conf.int = TRUE
)
bxp
## Get new label

p <- ggboxplot(z_score_cluster, x = "variable", y = "value",
               color = "clusters", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "clusters")
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = )
p

#####
cities <- c(rep("Delhi", 3), rep("Mumbai", 3), 
            rep("Chennai", 3), rep("Bengaluru", 3)) 

humidity <- rep(c("High", "Medium", "Low"), 4) 

temperature <- abs(rnorm(12, 25, 10)) 

dataframe <- data.frame(cities, humidity, 
                        temperature) 

# calling the dataframe 
dataframe 

# plotting graph 
ggplot(dataframe, aes(fill = humidity, 
                      y = temperature, x = cities))+ 
  geom_bar(position = "fill", stat = "identity")+ 
  ggtitle("Weather Data of 4 Cities !")+ 
  theme(plot.title = element_text(hjust = 0.5))


