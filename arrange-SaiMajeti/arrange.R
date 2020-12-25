
nest_men_women <- melt(mydata[,c('Date','Women','Men')], value.name = "value",id.vars = 1)

nest_men_women$Date <- as.character.Date(as.Date.factor((nest_men_women$Date), format = "%m/%d/%y"))

view(nest_men_women)


ggplot(nest_men_women, aes(x = Date, y = value)) +
  geom_bar(aes(fill = variable), position = position_dodge(width = 1.0), stat = "identity", width = 0.8)+
  
  labs(title = "Q1 - What is the breakdown of Women vs. Men that were provided shelter each night?", 
       x= "Date", y = "Number of Women/Men that were provided shelter each night",
       caption = "Break down of women vs. men who were provided shelter each night.
       
    Legend gives the color encoding details of the measure values (Pink - women, Blue - men)
       
    Labels give the count of women/men for each night. Using this, on a selected date, the number of men vs women who were provided shelter each night can be identified.

  **Visual Encoding:**

  Marks - Bar

  Channel - Color for break down between Women and Men values;    spacial position on axes

  Attributes - Women, Men, Date 

  Data types - Quantitative on y-axis, ordinal (Date on x-axis), categorical - men/women")+
  
  scale_fill_manual(values = c("Women" ="lightpink","Men" = "steelblue3"))+
  
  scale_y_continuous( breaks = c(20,40,60,80,100))+
  
  scale_x_discrete() +
  
  theme_light()+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_text(size=rel(1.5)),
        axis.title.y = element_text(size=rel(1.5)),
        plot.title = element_text(size = 20),
        plot.caption = element_text(colour = "#38B378", hjust = 0, size = 14),
        legend.background = element_blank(), legend.justification = c(1,1),
        axis.line.x.bottom = element_line(colour = "lightgrey", color = "lightgrey", linetype = "solid"),
        axis.line.y.left = element_line(colour = "lightgrey", color = "lightgrey", linetype = "solid"),
        axis.line.x.top = element_line(colour = "lightgrey", color = "lightgrey", linetype = "solid"),
        axis.line.y.right = element_line(colour = "lightgrey", color = "lightgrey", linetype = "solid"))+
  
  geom_text(data = nest_men_women, aes(Date, label= value), colour="black", check_overlap = TRUE, 
            vjust = -0.6, size = 3.8, position = position_dodge2(width =1.0 ), inherit.aes = TRUE)

tmp <- mydata %>% group_by(Church) %>% summarise(tot.guests = sum(Total.Guests))

tmp$Church <- factor(tmp$Church)

ggplot(mydata, aes(reorder(Church, +Total.Guests), Total.Guests, label = Total.Guests))+
  
  geom_bar(stat = "identity", fill = "steelblue3", width = 0.5)+ 
  
  labs(title = "Q2 - In total, how many guests were given shelter by each church(or pair of churches)?", 
       x = "Name of the Church ", y = "Total number of guests that were provided shelter by each church",
       caption = " 
    The church that accomodated highest number of guests in total is Tabernacle/Ohef Sholom Temple
    
    ** Visual Encoding **
      
    Marks - Bar.
       
    Channel - Spatial positioning.

    Encoding - Sorting the churches in a descending order of number of guests accomodated in total. 

    Labels help to identify the total number of guests per each church. 

    Attributes - Church and Total guests.

    Data types - Categorical and Quantitative.")+
  
  theme_minimal(base_size = 16) +
  coord_flip()+
  
  theme(axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.2)),
        plot.title = element_text(size = 20),
        plot.caption = element_text(colour = "#38B378", hjust = 0, size = 14),
        axis.line = element_line(colour = NULL, color = NULL, linetype = 0))+
  
  geom_text(data = temp, aes(Church, tot.guests, label= tot.guests), colour="lightseagreen", 
            check_overlap = TRUE, hjust = 0, nudge_y = 10, size = 6)

ggplot(mydata, aes(reorder(Date, -Total.Demand), y = Total.Demand)) +
  geom_bar(aes(fill = Temp.Low), stat = "identity", alpha= 1, width = 0.7)+
  
  guides(guide = guide_legend(reverse = TRUE))+
  
  labs(title = "Q3 - Is total demand for shelter affected by weather?", x = "Date", y = "Total Demand",
       fill="Temp Low", 
       
       caption = " 
  
    The marks are ordered in a descending total demand as the color already represents the temperature variation. 
    With reference to this, it is safe to say that most of the colder days had comparitively high total demand than 
    the hotter days. Also, more than half of the days have higher demand than the average Total Demand.  

    ** Visual Encoding **
       
    Marks - Bar
       
    Channels - Color (of the bar indicates how cold the day was), and, spatial positioning

    Encoding - The lightest mark shows the lowest temperature i.e the coldest day.

    The orange dotted line shows the Average total demand.

    Attributes - Date, Total Demand, Temp Low

    Data types - Ordinal, Quantitative")+
  
  theme_minimal()+
  
  geom_hline(yintercept=mean(mydata$Total.Demand), color="orange", linetype = "dashed", size = 1, show.legend = TRUE, na.rm = TRUE)+
  
  geom_label(aes(0, mean(mydata$Total.Demand), label = "Total.Demand -- Average -- 95.39", vjust = 0, hjust = -0.05 ), size = 5, label.size = NA, fill= "gray96", label.padding = unit(0, "lines"))+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(),
        legend.title.align = 0,
        legend.justification = c(1,1),
        axis.title.x = element_text(size=rel(1.5)),
        axis.title.y = element_text(size=rel(1.5)),
        plot.caption = element_text(colour = "#38B378", hjust = 0, size = 14),
        plot.title = element_text(size = 20))+
  
  scale_y_continuous( breaks = c(0, 20,40,60,80,100, 120, 140))+
  
  scale_fill_distiller(palette = "Blues", aesthetics = "fill", breaks = 2:5, direction = 1, na.value = 0, space = "Lab")+
  
  geom_text(data = mydata, aes(Date, Total.Demand, label = Total.Demand), colour = "lightseagreen", angle = 90, check_overlap = TRUE, hjust = -0.25)

