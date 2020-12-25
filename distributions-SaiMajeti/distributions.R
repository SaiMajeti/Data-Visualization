library(dslabs)
library(ggplot2)
library(dplyr)
library(tidyverse)

#histogram 1 - binwidth = 10
ggplot(movielens, aes(year)) + 
  geom_histogram(binwidth = 10, fill = "darkslategray1", col = "deepskyblue2", na.rm = TRUE) +
  xlab("Year") +
  ggtitle(label = "Histogram 1 - Movielens",
          subtitle = "Number of movies made in a 10 year window from 1970 - 2020") +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900,2020, 10))


#histogram 2 binwidth = 5
ggplot(movielens, aes(year)) + 
  geom_histogram(binwidth = 5, fill = "darkslategray1", col = "deepskyblue2", na.rm = TRUE) +
  xlab("Year") +
  ggtitle(label = "Histogram 2 - Movielens",
          subtitle = "Number of movies made in a 5 year window from 1970 - 2020") +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900,2020, 10))

#ecdf
ggplot(movielens, aes(year)) + stat_ecdf(geom = "step", na.rm = TRUE, col = "deepskyblue2") +
  scale_x_continuous(limits = c(1900, 2020), breaks = seq(1900,2020, 10)) + 
  ylab("") + 
  ggtitle(label = "eCDF - movielens" , subtitle = "Number of movies made")


#boxplot
ggplot(movielens, aes(y = year, x = "")) + 
  geom_boxplot(na.rm = TRUE, col = "deepskyblue2", fill = "darkslategray1") + 
  coord_flip() +
  xlab("") +
  theme(axis.ticks.y = element_blank()) + 
  scale_y_continuous(limits = c(1900, 2020), breaks = seq(1900,2020, 10)) + 
  ggtitle(label = "Boxplot - movielens", subtitle = "Number of movies made")

# histogram - 1; binwidth 1
ggplot(stars, aes(magnitude)) + 
  geom_histogram(na.rm = TRUE, binwidth = 1, fill = "bisque", col = "coral1") + 
  scale_x_continuous(limits = c(-15, 20), breaks = seq(-15,20,5))+ 
  ggtitle(label = "Histogram 1 - Stars",
          subtitle = "Magnitude of stars")


#histogram - 2, binwidth 5
ggplot(stars, aes(magnitude)) + 
  geom_histogram(na.rm = TRUE, binwidth = 5, fill = "bisque", col = "coral1") +
  scale_x_continuous(limits = c(-15, 20), breaks = seq(-15, 20, 5))+
  ggtitle(label= "Histogram 2 - Stars",
          subtitle = "Magnitude of stars")

#ecdf
ggplot(stars, aes(magnitude)) + 
  stat_ecdf(geom = "step", na.rm = TRUE, col = "coral1") + 
  scale_x_continuous(limits = c(-15, 20), breaks = seq(-15, 20, 5))+
  ggtitle(label = "eCDF - Stars", subtitle = "Magnitude of stars")+
  ylab("")

#boxplot
ggplot(stars, aes(y = magnitude, x = "")) + 
  geom_boxplot(na.rm = TRUE, col = "coral1", fill = "bisque") +
  coord_flip() + 
  xlab("") + 
  theme(axis.ticks.y = element_blank()) +
  scale_y_continuous(limits = c(-15, 20), breaks = seq(-15, 20, 5))+
  ggtitle(label="Boxplot - Stars", subtitle = "Magnitude of stars")

#histogram - binws 50
ggplot(mapping = aes(outlier_example)) + 
  geom_histogram(na.rm = TRUE, fill = "maroon", bins = 50) + 
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  theme_classic()+
  ggtitle(label="Histogram 1 - Outlier_example", subtitle = "Effect of having extreme outliers")

#histogram - bins 100
ggplot(mapping = aes(outlier_example)) +
  geom_histogram(na.rm = TRUE, fill = "maroon", bins = 100) +
  scale_x_continuous(breaks = seq(0, 200, 10)) + 
  theme_classic() +
  ggtitle(label="Histogram 2 - Outlier_example", subtitle = "Effect of having extreme outliers")

#ecdf
ggplot(mapping = aes(outlier_example)) +
  stat_ecdf(geom = "step", na.rm = TRUE, col = "maroon")+
  scale_x_continuous(breaks = seq(0, 200, 10)) +
  ggtitle(label="eCDF - Outlier_example", subtitle = "Effect of having extreme outliers")+
  ylab("")

#boxplot
ggplot(mapping = aes(y = outlier_example, x = "")) + 
  geom_boxplot(na.rm = TRUE, fill = "honeydew2", col = "maroon") + coord_flip() +
  xlab("") + 
  theme_classic() +
  scale_y_continuous(limits =c(0, 200), breaks = seq(0, 200, 10)) +
  ggtitle(label="Boxplot - Outlier_example", subtitle = "Effect of having extreme outliers")+
  theme(axis.ticks.y = element_blank())

#histogram
#histogram - binwidth 0.5
ggplot(mapping = aes(outlier_example)) + 
  geom_histogram(na.rm = TRUE, fill = "thistle1", col = "maroon", binwidth = 0.5) + 
  scale_x_continuous(limits = c(5, 8), breaks = seq(5, 8, 0.2)) +
  ggtitle(label="Histogram 1 - Outlier_example", subtitle = "Plot of Outlier_example without the extreme outlier")

#histogram - binwidth 0.1
ggplot(mapping = aes(outlier_example)) +
  geom_histogram(na.rm = TRUE, col = "maroon", fill = "thistle1", binwidth = 0.1) +
  scale_x_continuous(limits = c(5, 8), breaks = seq(5, 8, 0.2)) + 
  ggtitle(label="Histogram 2 - Outlier_example", subtitle = "Plot of Outlier_example without the extreme outlier")

#ecdf
ggplot(mapping = aes(outlier_example)) +
  stat_ecdf(geom = "step", na.rm = TRUE, col = "maroon") +
  scale_x_continuous(limits = c(5, 8), breaks = seq(5, 8, 0.2)) +
  ggtitle(label="eCDF - Outlier_example", subtitle = "Plot of Outlier_example without the extreme outlier")+ ylab("")


#boxplot
ggplot(mapping = aes(y = outlier_example, x = "")) + 
  geom_boxplot(na.rm = TRUE, fill = "thistle1", col = "maroon") + coord_flip() +
  xlab("") + 
  scale_y_continuous(limits = c(5, 8), breaks = seq(5, 8, 0.2)) +
  ggtitle(label="Boxplot - Outlier_example", subtitle = "Plot of Outlier_example without the extreme outlier")+
  theme(axis.ticks.y = element_blank())