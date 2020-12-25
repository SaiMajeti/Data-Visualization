library(tidyverse)
library(ggplot2)
library(dplyr)
library(XLConnect) 
library(directlabels)
library(extrafont)
require(ggthemes)
library(reshape2)
library(lubridate)
library(ggpubr)


#read sheet from file
sheet1 <- readWorksheetFromFile("WSJ.xlsx", sheet = 1, endCol = 5)

#change name of column 1 to 'Year'
colnames(sheet1)[colnames(sheet1) == "Col1"] <- "Year"

#White line
p <- ggplot(sheet1, aes(x = Year))

p <- p + 
  geom_line(aes(y = White), color = "#495E8A", size = 1.05, linetype = "solid")+
  geom_label(aes(y = White[9], x = Year[9] + 0.5, label = "White"), color = "#495E8A", vjust = -0.2, fill = "white", label.size = NA)

#Hispanic line
p <- p + 
  geom_line(aes(y = Hispanic), color  = "#CA6D20", size = 1.05)+
  geom_label(aes(y = Hispanic[9], x = Year[9] + 0.5, label = "Hispanic"), color = "#CA6D20", vjust = 2.8, label.padding = unit(0.07, "lines"), label.size = NA)

#Asian line
p <- p + 
  geom_line(aes(y = Asian), color = "#5B8C35", size = 1.05)+
  geom_label(aes(y = Asian[9], x = Year[9] + 0.3, label = "Asian"), color = "#5B8C35", vjust = 2.5, fill = "white", label.padding = unit(0.1, "lines"), label.size = NA)

#Black line
p <- p + 
  geom_line(aes(y = Black), color = "#DB0404", size = 1.05)+
  geom_label(aes(y = Black[9], x = Year[9] + 0.3, label = "Black"), color = "#DB0404", vjust = -0.8, fill = "white", label.padding = unit(0.08, "lines"), label.size = NA)

#Setting axis breaks
p <- p + theme_hc()+
  scale_x_continuous(name="", breaks = seq(2004, 2014, 1), expand = c(0,0.3))+
  scale_y_continuous(name="", limits = c(0,20), breaks = seq(0, 20, 2))

#changing axis text size and removing axis ticks
p <- p + 
  theme(axis.text.x = element_text(size = rel(1.3), family = "Calibri", color = "black"), axis.text.y = element_text(size = rel(1.3), family = "Calibri", color = "black"), axis.ticks =element_blank(), plot.margin = margin(0, 0, 0, 0, "cm"))


#Adding plot title and subtitle
p <- p + 
  labs(title = "Out of Work", subtitle = "Percent of families with at least one member unemployed")+ 
  theme(plot.title = element_text(family = "Calibri", size = 18, face = "bold", vjust = 0, hjust = -0.04), plot.subtitle = element_text(family = "Calibri", size = 13, hjust = -0.09))


#final plot
p

sheet1 <- readWorksheetFromFile("WSJ.xlsx", sheet = 1, endCol = 5)

#change name of column 1 to 'Year'
colnames(sheet1)[colnames(sheet1) == "Col1"] <- "Year"

new1 <- melt(sheet1[, c ('Year','White','Hispanic','Asian','Black')], value.name = "value", id.vars = 1 )

names(new1) <-  c("Year", "Class", "Value")

g <- ggplot(new1, aes(x = Year, y = Value)) +
  geom_line(aes(color = Class), size = 1, show.legend = FALSE)

g <- g + scale_color_manual(values = c("White" = "#495E8A", "Hispanic" = "#CA6D20", "Asian" = "#5B8C35" , "Black" = "#DB0404"))

g <- g+ labs(title = "Out of Work", subtitle = "Percent of families with at least one member unemployed")+ 
  theme(plot.title = element_text(family = "Calibri", face = "bold", size = 18, vjust = 0, hjust = -0.04), plot.subtitle = element_text(family = "Calibri", size = 13, hjust = -0.09))

g <- g + theme(axis.text.x = element_text(size  = rel(1.3), family = "Calibri", color = "black"), axis.text.y = element_text(size = rel(1.3), family = "Calibri", color = "black"), axis.ticks =element_blank(), plot.margin = margin(0, 0, 0, 0, "cm"))

g <- g + theme_hc() +
  scale_x_continuous(name="", breaks = seq(2004, 2014, 1), expand = c(0,0.3))+
  scale_y_continuous(name="", limits = c(0,20), breaks = seq(0, 20, 2))

g 

#read worksheet
Sheet2 <- readWorksheetFromFile("OECD_Skills.xlsx", sheet = 1, endCol = 7, startRow = 13, endRow = 44, header = TRUE)

#rename the columns
names(Sheet2) <- c("Coun", "Country", "Highskilled", "Mediumskilled", "Lowskilled", "Total", "Total business sector employment sustained by foreign demand")

#preserve the dataframe order
Sheet2$Country <- factor(Sheet2$Country, levels = rev(Sheet2$Country))

#Base plot
B <- ggplot(Sheet2, aes(x = Country,))+
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 3), expand = c(0,0))+ theme_excel_new()+
  theme(axis.ticks = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(), plot.margin = margin(t = 0.5, unit = "cm"))

B <- B + coord_flip()

#Lowskilled  
B1 <- B + 
  geom_bar(aes(y = Lowskilled), fill = "#006BB8", width = 0.5, inherit.aes = TRUE, stat = "identity" )+
  labs(title = "Lowskilled")

B1 <- B1 + theme(plot.title = element_text(color = "#006BB8", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm")), axis.line.y = element_line(linetype = "solid"), axis.text.y = element_text(size = rel(1.5), color = "#6E6E6E", margin = margin(r = 0.5, unit = "cm") ))

#Mediumskilled
B2 <- B + geom_bar(aes(y = Mediumskilled), fill = "#7FA8D9", width = 0.5, inherit.aes = TRUE, stat = "identity")+
  labs(title = "Mediumskilled")

B2 <- B2 + theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#7FA8D9", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm")))

#Highskilled
B3 <- B + geom_bar(aes(y = Highskilled), fill = "#00AACC", width = 0.5, stat = "identity")+
  labs(title = "Highskilled")

B3 <- B3 + theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#00AACC", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm")))

#Total trained workforce
B4 <- B + geom_bar(aes(y = Total), fill = "#264478", width = 0.5, stat = "identity")+
  labs(title = "Total Trained Workforce")

B4 <- B4 + theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#264478", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm")))

#final plot
final <- ggarrange(B1, B2, B3, B4, ncol = 4)


final

Sheet2 <- readWorksheetFromFile("OECD_Skills.xlsx", sheet = 1, endCol = 7, startRow = 13, endRow = 44, header = TRUE)

names(Sheet2) <- c("Coun", "Country", "Highskilled", "Mediumskilled", "Lowskilled", "Total", "Total business sector employment sustained by foreign demand")

#lock the dataframe order
Sheet2$Country <- factor(Sheet2$Country, levels = rev(Sheet2$Country))

B <- ggplot(Sheet2, aes(x = Country,))+ theme_excel_new()+
  theme(axis.ticks = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(), plot.margin = margin(t = 0.5, unit = "cm"))

B <- B + coord_flip()


B1 <- B + geom_bar(aes(y = Lowskilled), fill = "#006BB8", width = 0.5, inherit.aes = TRUE, stat = "identity" )+
  labs(title = "Lowskilled")

B1 <- B1 +
  theme(plot.title = element_text(color = "#006BB8", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, l= 1, unit = "cm")), axis.line.y = element_line(linetype = "solid"), axis.text.y = element_text(size = rel(1.5), color = "#6E6E6E", margin = margin(r = 0.5, unit = "cm"))) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 2), expand = c(0,0))

B2 <- B + geom_bar(aes(y = Mediumskilled), fill = "#7FA8D9", width = 0.5, inherit.aes = TRUE, stat = "identity")+
  labs(title = "Mediumskilled")

B2 <- B2 +
  theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#7FA8D9", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm"))) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 2), expand = c(0,0))

B3 <- B + geom_bar(aes(y = Highskilled), fill = "#00AACC", width = 0.5, stat = "identity")+
  labs(title = "Highskilled")

B3 <- B3 + 
  theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#00AACC", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm"))) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 2), expand = c(0,0))

B4 <- B + geom_bar(aes(y = Total), fill = "#264478", width = 0.5, stat = "identity")+
  labs(title = "Total Trained Workforce")

B4 <- B4 + 
  theme(axis.text.y = element_blank(), axis.line.y = element_line(linetype="solid"), plot.title = element_text(color = "#264478", hjust = 0, size = rel(2.5), margin = margin(b= 0.5, unit = "cm"))) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, 3), expand = c(0,0))

final <- ggarrange(B1, B2, B3, B4, ncol = 4)

final

