library(dplyr)
library(ggplot2)

process <- c("No notice of proceedings","Notice of proceedings",
             "Notice and opportunity to be heard by impartial decider",
             "Notice and hearing with legal representation", "Jury Trial", 
             "Jury trial with opportunity for appeal"
             )
x <- c(0, 10, 20, 30, 40, 50)
y <- c(0, 10, 20, 30, 40, 50)
data = data.frame(process, x, y)

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = process) +
  ylab("Adminstrative Process Due")+
  xlab("Mathews Balancing Function Output")+
  geom_label(label = "d(p,r,m,g,c)=p+r+m-g-c", x=40, y=20)

theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")+
  
