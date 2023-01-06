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
  xlab("Adminstrative process granted by judge") +
  ylab("Frequency judge grants this level of process") +
  annotate(geom="text", x=-6, y=-.02, label="Insufficient Process") +
  annotate(geom="text", x=3, y=-.02, label="Excessive Process") +
  annotate(geom="text", x=0, y=-.02, label="Appropriate Process") +
  theme_bw()+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank()) +
  labs(
    title = element_text("Reducing noise in judicial decisions about how much process required under Mathews reduces error in the aggregate"),
    subtitle = element_text("Holding bias constant, a reduction in noise reduces error by mitigating the number of really bad decisions"))
hist


############
#line charts
###########
process <- c("No notice, unilateral adminstrative action","Notice of proceedings",
             "Notice and opportunity to be heard by impartial decider",
             "Notice and hearing with legal representation", "Jury Trial", 
             "Jury trial with opportunity for appeal"
             )
x <- c(0, 10, 20, 30, 40, 50)
y <- c(0, 10, 20, 30, 40, 50)
data = data.frame(process, x, y)

linear = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  scale_y_continuous(labels = process) +
  ylab("Adminstrative Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)=|p+r+m-g-c|", x=40, y=20) +
  labs(
    title = element_text("Process due (d) as a function of Mathew's balancing facors"),
    subtitle = element_text("Factors on a scale of 0-10: The private interest at stake(p), risk of error given current procedure(r), reduction of risk error from extra procedure (m),
goverment interest at stake (g), and cost of additional procedures(c)")) +
  theme_bw()


pandemic = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  geom_point(x=14.9,y=14.9, colour = '#DD7764', size = 5)+
  geom_segment(aes(x=14.9, xend=14.9, y=0, yend=14.9), lty=2, color = '#DD7764') +
  geom_segment(aes(x=0, xend=14.9, y=14.9, yend=14.9), lty=2, color = '#DD7764') +
  scale_y_continuous(labels = process) +
  ylab("Adminstrative Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)=|8+9.9+8-10-1|", x=35, y=15) +
  labs(
    title = element_text("In the hypothetical 'lab leak' scenario, the simple linear function would result in too much process"),
    subtitle = element_text("Red dot represents function output in this hypothetical"))+
  theme_bw()

pandemic

gov = ggplot(data, aes(x, y)) +
  geom_line(col = "#64A1B4") +
  geom_point(x=0,y=0, colour = '#DD7764', size = 5)+
  scale_y_continuous(labels = process) +
  ylab("Adminstrative Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)= |8 + 9.9+ 8 - 50 - 1|", x=35, y=15) +
  labs(
    title = element_text("Giving greater weight of the government interest allows agencies to take decisive action when a strong government interest is at stake"),
    subtitle = element_text("Red dot represents function output in this hypothetical"))+
  theme_bw()

gov






