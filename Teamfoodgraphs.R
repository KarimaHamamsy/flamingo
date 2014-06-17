sugar <- read.csv("Sugar.Products.csv", header=TRUE)

# Dot plot of amount of sugar in selected products
ggplot(sugar, aes(x=Sugar.in.grams, y=reorder(Product, Sugar.in.grams))) + 
  geom_point(size=3, colour="#de2d26") + theme_bw() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

# dot plot with lines going up to the dot
ggplot(sugar, aes(x=Sugar.in.grams, y=reorder(Product, Sugar.in.grams))) + 
  geom_segment(aes(yend=Product), xend=0, colour="grey50") + 
  geom_point(size=3, colour="#de2d26") + theme_bw() + 
  theme(panel.grid.major.y = element_blank()) + 
  scale_x_continuous(breaks=seq(0, 80, 5)) + xlab("Sugar(grams)") + ylab("Product")


# clean theme
clean <- theme_bw() + theme(axis.line = element_line(colour = "black"), 
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(), 
                            panel.border = element_blank(), 
                            panel.background = element_blank()) 


# Coke advertising bar chart
# rearrange factors
Cokeads$Age <- factor(Cokeads$Age.Group, levels=c("2_5", "6_11", "12_17", "18_adults"))

Coke <- ggplot(Cokeads, aes(x=Age, y=Ad.Views, fill=Age)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Coca-Cola Classic") + 
  scale_y_continuous(breaks=seq(0,55,5), limits=c(0,55))

# Capri Sun advertising bar chart
# rearrange factors
CapriSun$Age1 <- factor(CapriSun$Age, levels=c("2_5", "6_11", "12_17", "18_adults"))
CapriSun <- ggplot(CapriSun, aes(x=Age1, y=Ad.Views, fill=Age1)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Capri Sun") + scale_y_continuous(breaks=seq(0,55,5)) + 
  theme(legend.position="none")


# Kool-Aid
# rearrange factors
KoolAid$Age <- factor(KoolAid$Age_group, levels=c("2_5", "6_11", "12_17", "18_adults"))
KoolAid <- ggplot(KoolAid, aes(x=Age, y=Ad.Views, fill=Age)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Kool Aid") + 
  scale_y_continuous(breaks=seq(0,55,5), limits=c(0,55))

# Sunny D
# rearrange factors
SunnyD$Age <- factor(SunnyD$Age_Group, levels=c("2_5", "6_11", "12_17", "18_adults"))
SunnyD <- ggplot(SunnyD, aes(x=Age, y=Ad.Views, fill=Age)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Sunny D") + 
  scale_y_continuous(breaks=seq(0,55,5), limits=c(0,55))

# Gatorate
# rearrange factors
Gatorade$Age <- factor(Gatorade$Age_Group, levels=c("2_5", "6_11", "12_17", "18_adults"))
Gatorade <- ggplot(Gatorade, aes(x=Age, y=Ad.Views, fill=Age)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Gatorade") + 
  scale_y_continuous(breaks=seq(0,55,5), limits=c(0,55))

# Dr Pepper
DrPepper$Age <- factor(DrPepper$Age_Group, levels=c("2_5", "6_11", "12_17", "18_adults"))
DrPepper <- ggplot(DrPepper, aes(x=Age, y=Ad.Views, fill=Age)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#cb181d", "#fb6a4a", "#fcae91", "#fee5d9")) + 
  clean + ggtitle("Dr Pepper") + 
  scale_y_continuous(breaks=seq(0,55,5), limits=c(0,55))

# Arrange plots
grid.arrange(CapriSun, KoolAid, SunnyD, Gatorade, Coke, DrPepper, ncol=2)


# Timeline for obesity rates
ggplot(obesityRates, aes(x=Year, y=Obesity.Rate)) + 
  geom_line(colour="#a50f15") + 
  clean + scale_y_continuous(breaks=seq(0,100,10), limits=c(0,100)) + 
  scale_x_continuous(breaks=c(1950, 1960, 1970, 1980, 1990, 2000, 2010))



