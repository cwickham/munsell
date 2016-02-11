library(reshape2)
library(ggplot2)
library(munsell)

## quantitative

enroll <- VGAM::auuc
enroll$SES <- rownames(enroll)
enroll_m <- melt(enroll)

my <- "5BG 5/4"

qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity") + 
  scale_fill_manual("Socio-economic status", 
    values = mnsl(c(my, rygbp(my, 10), rygbp(my, 20), rygbp(my, 30))))

qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity") + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 5), rygbp(my, 10), rygbp(my, 20))))

my <- "5PB 5/12"
qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity") + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 3), rygbp(my, 6), rygbp(my, 9))))

# luminace contrast boundary (Guideline 4.6 Ware)

my <- "5PB 6/12"
qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity", colour = I(mnsl("N 4/0"))) + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 3), rygbp(my, 6), rygbp(my, 9))))

qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity", colour = I(mnsl("N 4/0"))) + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 3), rygbp(my, 6), rygbp(my, 9)))) + theme(panel.background = element_rect(fill = mnsl("5Y 9/2")),
    plot.background = element_rect(fill = mnsl("5Y 9/2")))


my <- "5G 7/6"
qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity", colour = I(mnsl("N 9/0"))) + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 5), rygbp(my, 10), rygbp(my, 15))))


my <- "5G 5/4"
qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity", colour = I(mnsl("N 4/0"))) + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 5), rygbp(my, 10), rygbp(my, 15))))

# with emphasis

my <- "5G 7/6"
qplot(variable, value, data = enroll_m, fill = SES, 
  geom= "bar", stat = "identity", colour = I(mnsl("N 8/0"))) + 
  scale_fill_manual(values = mnsl(c(my, rygbp(my, 5), rygbp(my, 10), complement(rygbp(my, 5)))))

## == quantitative example == ## 
# heatmap county level plots

# or hexbin example

library(fueleconomy)
library(dplyr)

hwy_cty <- vehicles %>% 
  filter(fuel == "Regular") %>% 
  group_by(hwy, cty) %>%
  summarise(count = n())

qplot(hwy, cty, data = hwy_cty, fill = count, geom = "tile") +
  scale_fill_continuous(trans = "sqrt")





