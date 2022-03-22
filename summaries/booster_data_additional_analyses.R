library("ggpubr")
ggboxplot(output2, x = "pop_race2", y = "knowledge_avg0to5", 
          color = "pop_race2",
          
          ylab = "Weight", xlab = "Treatment")





ggboxplot(output2, x = "pop_race2", y = "attitudes_avg1to21", 
          color = "pop_race2",
          
          ylab = "Weight", xlab = "Treatment")






ggline(output2, x = "pop_race2", y = "attitudes_avg1to21", 
       add = c("mean_se", "jitter"), 
       ylab = "Weight", xlab = "Treatment")


test <- aov(knowledge_avg0to5 ~ pop_race2, data = output2)
summary(test)
TukeyHSD(test)

library(multcomp)
summary(glht(test, linfct = mcp(pop_race2 = "Tukey")))


test <- aov(attitudes_avg1to21 ~ pop_race2, data = output2)
summary(test)
TukeyHSD(test)
summary(glht(test, linfct = mcp(pop_race2 = "Tukey")))
