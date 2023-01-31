library(dplyr)
library(ggplot2)


############
#histogram
###########
normlonoise <- rnorm(100000,-3, 1)
normalhinoise <- rnorm(100000, -3, 2)
datahis <- data.frame(normlonoise) %>%
  rename(Dist = normlonoise) %>%
  mutate(meaurement_error = "biased")
datahis2 <- data.frame(normalhinoise) %>%
  rename(Dist = normalhinoise)%>%
  mutate(meaurement_error = "noisey and biased")
datahis3 <- rbind(datahis, datahis2)


hist <- ggplot(datahis3, aes(x=Dist, fill =meaurement_error)) + 
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("#64A1B4",'#DD7764' )) +
  xlab("Process held to be required by judge") +
  ylab("Frequency judge grants finds this level of process required") +
  annotate(geom="text", x=-6, y=-.02, label="Insufficient Process") +
  annotate(geom="text", x=3, y=-.02, label="Excessive Process") +
  annotate(geom="text", x=0, y=-.02, label="Appropriate Process") +
  theme_bw(base_size = 14)+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        ) +
  labs(
    title = element_text("Reducing noise in judicial decisions about how much process required under Mathews reduces error in the aggregate"),
    subtitle = element_text("Holding bias constant, a reduction in noise reduces error by mitigating the number of really bad decisions"))
hist


############
#line charts
###########
process <- c("No notice, unilateral government action","Notice of proceedings",
             "Notice and opportunity to be heard by impartial decider",
             "Notice and hearing with legal representation", "Jury trial", 
             "Jury trial with opportunity for appeal")
x <- c(0, 5, 10, 15, 20, 25)
y <- c(0, 5, 10, 15, 20, 25)
data = data.frame(process, x, y)

linear = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  scale_y_continuous(breaks = x, labels = process) +
  ylab("Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)= p+r+m-g-c", x=22.5, y=19) +
  labs(
    title = element_text("Process due (d) as a function of Mathew's balancing facors"),
    subtitle = element_text("Factors on a scale of 0-10: The private interest at stake(p), risk of error given current procedure(r), reduction of risk error from extra procedure (m),
goverment interest at stake (g), and cost of additional procedures(c)")) +
  theme_bw(base_size = 12) +
  theme (axis.text = element_text(size = 13),
         axis.title = element_text(size =13))
linear

pandemic = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  geom_point(x=15.9,y=15.9, colour = '#DD7764', size = 5)+
  geom_segment(aes(x=15.9, xend=15.9, y=0, yend=15.9), lty=2, color = '#DD7764') +
  geom_segment(aes(x=0, xend=15.9, y=15.9, yend=15.9), lty=2, color = '#DD7764') +
  scale_y_continuous(labels = process) +
  ylab("Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)= 8.5 + 9.9 + 8.5 - 10 - 1", x=19.5, y=15.5) +
  labs(
    title = element_text("In the hypothetical 'lab leak' scenario, the simple linear function would result in too much process"),
    subtitle = element_text("Red dot represents function's output in this hypothetical"))+
  theme_bw(base_size = 12) +
  theme (axis.text = element_text(size = 13),
         axis.title = element_text(size =13))
pandemic

gov = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  geom_point(x=0,y=0, colour = '#DD7764', size = 5)+
  scale_y_continuous(labels = process) +
  ylab("Adminstrative Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)= 8.5 + 9.9 + 8.5 - 30 - 1", x=3.8, y=0) +
  theme_bw() +
  labs(
    title = element_text("Giving greater weight of the gov. interest allows agencies to take decisive action when a strong gov. interest is at stake"),
    subtitle = element_text("Red dot represents function's output in this hypothetical (with 0 as a minimum)")) +
  theme (axis.text = element_text(size = 13),
         axis.title = element_text(size =13))
 
gov






