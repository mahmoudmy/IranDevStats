# loading packages
require(dplyr)
require(ggplot2)
require(scales)
require(grid)

# reading data
dev <- read.csv("~/data_stackoverflow_2015_.csv")
irDev <- filter(dev, Country == 'Iran')

# Produce frequency table for categorical variables
tabFunc <- function(key) {
  var_ <- irDev[key]
  tab_ <- as.data.frame(table(var_))
  tab_ <- filter(tab_, tab_[,1] != "" & tab_[,2] > 0 & tab_[,1] != "Prefer not to disclose")
  names(tab_) <- c("category", "freq")
  return(tab_)
}

### Age
ageTab <- tabFunc('Age')
ggplot(ageTab, aes(x = category, y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("گروه سنی") + ylab("تعداد") +
  ggtitle("فراوانی گروه های سنی در توسعه دهندگان ایرانی") +
  geom_text(aes(label = sprintf("%.2f%%", freq/sum(freq) * 100)), 
            hjust = -.1, size = 4, color="#e74c3c")+
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 2),
        axis.ticks = element_line(size = 2)
  )

ggsave(file="age.png", dpi=300)

### Gender
genderTab <- tabFunc('Gender')
ggplot(genderTab, aes(x = category, y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("جنسیت") + ylab("تعداد") +
  ggtitle("توزیع جنسیتی توسعه دهندگان ایرانی") +
  geom_text(aes(label = sprintf("%.2f%%", freq/sum(freq) * 100)), 
            hjust = 0, size = 4.2, color="#e74c3c")+
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 2.5),
        axis.ticks = element_line(size = 2.5),
        plot.margin = unit(c(3,.5,3,.5), "cm")
  )

ggsave(file="gender.png", dpi=300)

### Desktop.Operating.System
occTab <- tabFunc('Desktop.Operating.System')
occTab <- occTab[order(-as.numeric(occTab[,2])),]
occTab <- within(occTab, category <- factor(category, 
                                              levels=occTab$category[9:1]))
ggplot(occTab, aes(x = category, y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("") + ylab("تعداد") +
  ggtitle("فراوانی سیستم عامل های مورد استفاده") +
  geom_text(aes(label = sprintf("%.2f%%", freq/sum(freq) * 100)),
            hjust = 0, size = 4.1, color="#e74c3c") +
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 1.8),
        axis.ticks = element_line(size = 2.5)
  )

ggsave(file="Desktop.Operating.System.png", dpi=300)

### Current Langeuage and Thech in use
techNames <- names(irDev)[9:50]
techNames <- strsplit(techNames, "Current.Lang...Tech..")
techNames <- matrix(unlist(techNames), ncol=2, byrow=TRUE)
techNames <- techNames[,2]
techNames[5] <- "C++"
techNames[6] <- "C++11"
techNames[7] <- "C#"
techFreq <- c()
for(i in 9:50) {
  techFreq[i-8] <-length(irDev[,i][irDev[,i]!=""])
}
techTab <- data.frame(names = techNames, freq = techFreq)
names(techTab) <- c("category", "freq")
techTab <- techTab[order(-as.numeric(techTab[,2])),]
topTech <- techTab[1:10, ]
topTech <- within(topTech, category <- factor(category, 
                                              levels=topTech$category[10:1]))
ggplot(topTech, aes(x = category,
                    y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("") + ylab("تعداد") +
  ggtitle("فراوانی زبان های برنامه نویسی مورد استفاده") +
  geom_text(aes(label = sprintf("%.2f%%", freq/nrow(irDev) * 100)),
            hjust = 1.1, size = 4.1, color="#ffffff") +
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 1.8),
        axis.ticks = element_line(size = 2.5)
  )

ggsave(file="pref.top.tech.png", dpi=300)

### Future Langeuage and Thech in use
fTechNames <- names(irDev)[52:93]
fTechNames <- strsplit(fTechNames, "Future.Lang...Tech..")
fTechNames <- matrix(unlist(fTechNames), ncol=2, byrow=TRUE)
fTechNames <- fTechNames[,2]
fTechNames[5] <- "C++"
fTechNames[6] <- "C++11"
fTechNames[7] <- "C#"
fTechFreq <- c()
for(i in 52:93) {
  fTechFreq[i-51] <-length(irDev[,i][irDev[,i]!=""])
}
fTechTab <- data.frame(names = fTechNames, freq = fTechFreq)
names(fTechTab) <- c("category", "freq")
fTechTab <- fTechTab[order(-as.numeric(fTechTab[,2])),]
fTopTech <- fTechTab[1:10, ]
fTopTech <- within(fTopTech, category <- factor(category, 
                                              levels=fTopTech$category[10:1]))
ggplot(fTopTech, aes(x = category,
                    y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("") + ylab("تعداد") +
  ggtitle("تکنولوژی و زبان های برنامه نویسی مورد علاقه برای یادگیری در آینده") +
  geom_text(aes(label = sprintf("%.2f%%", freq/nrow(irDev) * 100)),
            hjust = 1.1, size = 4.1, color="#ffffff") +
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 1.8),
        axis.ticks = element_line(size = 2.5)
  )

ggsave(file="future.top.tech.png", dpi=300)

### Job.Satisfaction
jobLevels <- unique(irDev$Job.Satisfaction)
jobSat <- irDev$Job.Satisfaction
jobSat <- subset(jobSat, jobSat %in% jobLevels[c(2,4,7,8)] )
jobTab <- as.data.frame(table(jobSat))
jobTab <- filter(jobTab, jobTab[,2] > 0)
names(jobTab) <- c("category", "freq")
jobTab <- within(jobTab, category <- factor(category, 
                                            levels=jobTab$category[c(3,2,4,1)]))
ggplot(jobTab, aes(x = category, y = freq, fill = category)) +
  geom_bar(width=.4, stat="identity", fill="#27ae60") +
  coord_flip() +
  theme_minimal()+
  guides(fill=FALSE) +
  xlab("") + ylab("تعداد") +
  ggtitle("رضایت شغلی توسعه دهندگان ایرانی (n=66)")+
  geom_text(aes(label = sprintf("%.1f%%", freq/sum(freq) * 100)),
            hjust = 1.1, size = 4.1, color="#ffffff") +
  theme(text=element_text(family="Droid Arabic Naskh"),
        axis.title.y = element_text(size = rel(1), vjust= 1),
        axis.title.x = element_text(size = rel(1), vjust= -.5),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(vjust = 1.5, hjust = 7),
        axis.ticks = element_line(size = 2.5)
  )

ggsave(file="Job.Satisfaction.png", dpi=300)