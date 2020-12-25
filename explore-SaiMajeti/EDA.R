library(nycflights13)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(hexbin)
library(ggstance)

#Q1 - Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?


#part 1- find number of flights scheduled vs number of flights cancelled

flights_per_day <- flights %>%
  mutate(cancelled_flights = (is.na(dep_delay) | is.na(arr_delay))) %>% 
  mutate(date = paste(month, day, year, sep = '-')) %>%
  group_by(date) %>%
  summarise(numflights = n(), 
            numcancelled = sum(cancelled_flights)) %>%
  ungroup()

#plot number of flights per day vs. number of flights cancelled
ggplot()+
  geom_point(flights_per_day, mapping = aes(numflights, numcancelled))

#delay - part 1 - correlation between delay and cancellation
cancelled_vs_delay <- flights %>%
  mutate(cancelled_flights = (is.na(dep_delay) | is.na(arr_delay))) %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  group_by(date) %>%
  summarise(
    avg_cancelled = mean(cancelled_flights),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE)
  ) %>%
  ungroup()

#delay - part 2 - plot to show correlation between arr_delay/dep_delay and the proportion of cancelled flights
p1 <- ggplot(cancelled_vs_delay) +
  geom_point(aes(avg_dep_delay, avg_cancelled))

p2 <- ggplot(cancelled_vs_delay) +
  geom_point(aes(avg_arr_delay, avg_cancelled))

p <- ggarrange(p1, p2, nrow = 1)

#final plot
p

#Q2 - Which flight(tailnum) has worst on-time record?

#general def. of on time --> flights which are not late are shown on-time

# point 1 - if we consider overall average delay

flights_ontime <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  
  #select flights that have arrival time(means they landed)
  filter(!is.na(arr_time)) %>% 
  select(date, arr_delay, tailnum)%>%
  group_by(tailnum) %>%
  summarise(num_flights = n(),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(num_flights >= 23) %>%
  arrange(desc(avg_arr_delay))

# ans - N203FR	41	59.12195
flights_ontime

#point 2 - if we consider flights(tailnums) that are not late (i.e. avg_arr_delay <= 0)

flights_ontime <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  
  #selecting only flights that have arrival time(means they landed)
  filter(!is.na(arr_time)) %>% 
  select(date, arr_delay, tailnum)%>%
  filter(!is.na(tailnum))%>% 
  filter(arr_delay <= 0)%>%
  group_by(tailnum) %>%
  summarise(num_flights = n(),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  filter(num_flights >= 23) %>%
  arrange(avg_arr_delay)

flights_ontime
# ans - N423AS	25	-33.44000

#Q3 - What time of day should you fly if you want to avoid delays as much as possible?

flight_hour <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  
  #filtering only the flights that are delayed
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(date, flight, tailnum, hour, minute, dep_delay, arr_delay) %>%
  
  #filtering only the flights that have positive delay
  filter(dep_delay > 0 & arr_delay > 0) %>%
  
  #creating time column with hour and minute
  mutate(time = paste(hour, minute, sep = ':') ) %>%
  group_by(time) %>%
  summarise(avg_delay = mean(arr_delay)) %>%
  arrange(desc(avg_delay)) %>%
  filter(min_rank(desc(avg_delay)) <= 25)

flight_hour

# without considering minutes

flight_hour <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  
  #filtering only the flights that are delayed
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(date, flight, tailnum, hour, minute, dep_delay, arr_delay) %>%
  
  #filtering only the flights that have positive delay
  filter(dep_delay > 0 & arr_delay > 0) %>%
  group_by(hour) %>%
  summarise(avg_delay = mean(arr_delay)) %>%
  arrange(desc(avg_delay))

flight_hour

# without considering positive delay
flight_hour <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  
  #filtering only the flights that are delayed
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  select(date, flight, tailnum, hour, minute, dep_delay, arr_delay) %>%
  group_by(hour) %>%
  summarise(avg_delay = mean(arr_delay)) %>%
  arrange(desc(avg_delay))
flight_hour


#Q4 - For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.

total_delay <- flights %>%
  mutate(date = paste(month, day, year, sep = '-')) %>%
  filter(arr_delay > 0) %>%
  group_by(flight, dest) %>%
  select(dest, flight, arr_delay)%>%
  mutate(total_delay = sum(arr_delay),
         delay_prop = arr_delay/total_delay
  )
total_delay

#if we consider carriers also:

proportion <- flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest, carrier, flight) %>%
  summarise(total_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    prop = total_delay / sum(total_delay)
  ) %>%
  arrange(dest, desc(prop))

proportion


#Q5-Explore the distribution of each of the x, y, and z variables in diamonds. What do you learn? Think about a diamond and how you might decide which dimension is the length, width, and depth.

p11 <- ggplot(diamonds) +
  geom_histogram(mapping = aes(x), na.rm = TRUE, binwidth = 0.01)+ theme_minimal()

p12 <- ggplot(diamonds, mapping = aes(x = "", y = x))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()

p1 <- ggarrange(p11, p12, ncol = 1)

p1

p21 <- ggplot(diamonds)+
  geom_histogram(mapping = aes(y), na.rm = TRUE, fill = "lightblue", binwidth = 0.01)+ theme_minimal()

p22 <- ggplot(diamonds, mapping = aes(x = "", y = y))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()+
  scale_y_continuous(limits =c(0, 60), breaks = seq(0, 60, 5))


p2 <- ggarrange(p21, p22, ncol = 1)

p2

p31 <- ggplot(diamonds)+
  geom_histogram(mapping = aes(z), na.rm = TRUE, fill = "lightgreen", binwidth = 0.01) + theme_minimal()

p32 <- ggplot(diamonds, mapping = aes(x = "", y = z))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()+
  scale_y_continuous(limits =c(0, 40), breaks = seq(0, 40, 5))

p3 <- ggarrange(p31, p32, ncol = 1)

p3

#x - length; y - width; z - depth

summary(select(diamonds, x, y, z))
filter(diamonds, min_rank(desc(x)) <= 3)
filter(diamonds, min_rank(desc(y)) <= 3)
filter(diamonds, min_rank(desc(z)) <= 3)

filter(diamonds, min_rank((x)) <= 5)
filter(diamonds, min_rank((y)) <= 5)
filter(diamonds, min_rank((z)) <= 5)


#reducing the limits

p11 <- ggplot(diamonds) +
  geom_histogram(mapping = aes(x), na.rm = TRUE, binwidth = 0.01)+ theme_minimal()+
  scale_x_continuous(limits =c(0, 10), breaks = seq(0, 10, 1))

p12 <- ggplot(diamonds, mapping = aes(x = "", y = x))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()

p1 <- ggarrange(p11, p12, ncol = 1)

p1

p21 <- ggplot(diamonds)+
  geom_histogram(mapping = aes(y), na.rm = TRUE, fill = "lightblue", binwidth = 0.01)+ theme_minimal()+
  scale_x_continuous(limits =c(0, 10), breaks = seq(0, 10, 1))

p22 <- ggplot(diamonds, mapping = aes(x = "", y = y))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()+
  scale_y_continuous(limits =c(0, 10), breaks = seq(0, 10, 2))


p2 <- ggarrange(p21, p22, ncol = 1)

p2

p31 <- ggplot(diamonds)+
  geom_histogram(mapping = aes(z), na.rm = TRUE, fill = "lightgreen", binwidth = 0.01) + theme_minimal()+
  scale_x_continuous(limits =c(0, 10), breaks = seq(0, 10, 1))

p32 <- ggplot(diamonds, mapping = aes(x = "", y = z))+
  geom_boxplot(na.rm = TRUE) +
  coord_flip()+
  scale_y_continuous(limits =c(0, 10), breaks = seq(0, 10, 2))

p3 <- ggarrange(p31, p32, ncol = 1)

p3


#Q6 - Explore the distribution of price. Do you discover anything unusual or surprising? (Hint: Carefully think about the binwidth and make sure you try a wide range of values.)

summary(select(diamonds, price))

price1 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 1)+
  scale_x_continuous(limits =c(0, 20000), breaks = seq(0, 20000, 2000))

price1

price2 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 5)+
  scale_x_continuous(limits =c(0, 20000), breaks = seq(0, 20000, 2000))

price2 

price3 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 10)+
  scale_x_continuous(limits =c(0, 20000), breaks = seq(0, 20000, 2000))

price3 

price4 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 50)+
  scale_x_continuous(limits =c(0, 20000), breaks = seq(0, 20000, 2000))

price4 

price5 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 50)+
  scale_x_continuous(limits =c(0, 8000), breaks = seq(0, 8000, 2000))

price5

price6 <- ggplot(diamonds)+
  geom_histogram(aes(price), na.rm = TRUE, binwidth = 50)+
  scale_x_continuous(limits =c(0, 3000), breaks = seq(0, 3000, 300))

price6


#Q7- How many diamonds are 0.99 carat? How many are 1 carat? What do you think is the cause of the difference?

carat <- diamonds %>%
  filter(carat == 0.99 | carat == 1) %>%
  group_by(carat) %>%
  summarise(count_0.99 = n())
carat

carat_price <- diamonds %>%
  group_by(carat) %>%
  summarise(count = n(),
            min_price = min(price),
            max_price = max(price),
            avg_price = mean(price)
  ) %>%
  arrange(carat)
carat_price

smallset <- carat_price %>%
  filter(carat >= 0.9, carat <= 1)
smallset



#Q8 -  What variable in the diamonds dataset is most important for predicting the price of a diamond? How is that variable correlated with cut? Why does the combination of those two relationships lead to lower quality diamonds being more expensive?
# Carat of the diamond is measured from the dimensions of the diamond. Hence, the other variables cut, color, clarity along with carat can be considered as the factors that effect the price of the diamond.

#Correlation between each variable and price

#2 continuous variables - scatter/hex/box plots

#1 Carat
price_vs_carat1 <- ggplot(diamonds)+
  geom_hex(aes(carat, price))

price_vs_carat1

#hard to see - try boxplot

price_vs_carat2 <- ggplot(diamonds, aes(carat, price))+
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

price_vs_carat2

#2 - Cut1
#one cont. one categorical
price_vs_cut <- ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

price_vs_cut

#freq. poly.
price_vs_cut1 <- ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

price_vs_cut1

#3 - Color
#categorical and cont.

#boxplot
price_vs_color2 <- ggplot(diamonds)+
  geom_boxplot(aes(color, price))

price_vs_color2

#price increasing as the color - quality worsens

#4- Clarity

price_vs_clarity <- ggplot(diamonds)+
  geom_boxplot(aes(clarity, price))

price_vs_clarity

#5 - carat vs. cut

# 2 categorical variables

carat_cut <- ggplot(diamonds)+
  geom_boxplot(aes(cut, carat))+coord_flip()

carat_cut

#Extracredit
#coord_flip()
carat_cut <- ggplot(diamonds)+
  geom_boxplot(aes(cut, carat))+
  coord_flip()

carat_cut

#ggstance

carat_cut2 <- ggplot(diamonds)+
  geom_boxploth(aes(carat, cut))

carat_cut2

#Q9 - How could you rescale the count dataset above to more clearly show the distribution of cut within colour, or colour within cut?

#given
count_dataset <- diamonds %>% 
  count(color, cut)
count_dataset 

#rescaling from count to proportion/density

# Cut within colour
rescale_cutWcol <- diamonds %>%
  count(color, cut)%>%
  group_by(cut)%>%
  mutate(density = n/sum(n))

rescale_cutWcol

cutWcol <- ggplot(rescale_cutWcol)+
  geom_tile(aes(color, cut, fill = density))+
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,1))

cutWcol

# Colour within cut
rescale_colWcut <- diamonds %>%
  count(color, cut)%>%
  group_by(color)%>%
  mutate(density = n/sum(n))

rescale_colWcut

colWcut <- ggplot(rescale_colWcut)+
  geom_tile(aes(color, cut, fill = density))+
  scale_fill_distiller(palette = "RdYlBu", limits = c(0,1))

colWcut

#Q10- Use geom_tile() together with dplyr to explore how average flight delays vary by destination and month of year. What makes the plot difficult to read? How could you improve it?

plot1 <- flights %>%
  select(year, month, day, dest, dep_delay, arr_delay)%>%
  group_by(month, dest)%>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))

plot_dep <- ggplot(plot1)+
  geom_tile(aes(factor(month), dest, fill = avg_dep_delay))

plot_dep

plot_arr <- ggplot(plot1)+
  geom_tile(aes(factor(month), dest, fill = avg_arr_delay))

plot_arr

#improvise

improvise <- flights %>%
  select(year, month, dest, dep_delay, arr_delay) %>%
  filter(!is.na(arr_delay), !is.na(dep_delay)) %>%
  group_by(month, dest)%>%
  filter(dep_delay > 0, arr_delay > 0) %>%
  summarise(avg.dep.delay = mean(dep_delay, na.rm = TRUE), 
            avg.arr.delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(dest) %>%
  filter(n() == 12)

#view(improvise)

p1 <- ggplot(improvise) +
  geom_tile(aes(factor(month), dest, fill = avg.arr.delay))+
  scale_fill_distiller(palette = "RdYlGn")+
  xlab("month") + ylab("destination") +
  ggtitle(label = "Arrival Delay")

p1

p2 <- ggplot(improvise) +
  geom_tile(aes(factor(month), dest, fill = avg.dep.delay)) +
  scale_fill_distiller(palette = "RdYlGn") +
  xlab("month") + ylab("destination")

p2

#Q11 - Instead of summarising the conditional distribution with a boxplot, you could use a frequency polygon. What do you need to consider when using cut_width() vs cut_number()? How does that impact a visualisation of the 2d distribution of carat and price?

cutnumber <- ggplot(data = diamonds) + 
  geom_freqpoly(aes(color = cut_number(carat, 5), x = price)) + 
  labs(color = "Carat")

cutnumber

cutwidth <- ggplot(diamonds)+
  geom_freqpoly(aes(price, color = cut_width(carat, 1.5, boundary = 0))) +
  labs(color = "Carat")

cutwidth

#Q12 -  Visualise the distribution of carat, partitioned by price.

#Carat, price - 2 cont. 

#scatter/box
#too many vars/ can't show distribution using scatterplot

distribution <- ggplot(diamonds)+
  geom_boxploth(aes(carat, cut_number(price, 10)))+
  labs(y = "Price", x = "Carat")

distribution

#Q13 - How does the price distribution of very large diamonds compare to small diamonds? Is it as you expect, or does it surprise you?

price_distribution <- diamonds %>%
  arrange(desc(price)) %>%
  select(carat, depth, price, x, y, z)
price_distribution

summary(select(diamonds, price, carat, depth, x, y, z))


#Q14 - Combine two of the techniques you've learned to visualise the combined distribution of cut, carat, and price.

technique_boxplot <- ggplot(diamonds, aes(x = cut_width(carat, 0.8), y = price, colour = cut))+
  geom_boxplot()

technique_boxplot

technique_hexbin <- ggplot(diamonds)+
  geom_hex(aes(carat, price, fill = cut), alpha = 1/3, na.rm = TRUE)+
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, 2000))

technique_hexbin

#Q15 - Two dimensional plots reveal outliers that are not visible in one dimensional plots. For example, some points in the plot below have an unusual combination of x and y values, which makes the points outliers even though their x and y values appear normal when examined separately. ... Why is a scatterplot a better display than a binned plot for this case? 

binned_boxplot <- ggplot(diamonds)+
  geom_boxploth(aes(x, cut_width(y, 1.5)))

binned_boxplot

scatterplot <- ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

scatterplot

