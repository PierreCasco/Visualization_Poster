# Author: Pierre Casco
# Purpose: Poster Project

fname = file.choose()
df <- read.csv(fname, sep = ';', header = T)

head(df)
colnames(df)
summary(df)

tapply_season <- tapply(df$Day.of.the.week, list(df$Seasons), sum)

barplot(tapply_season, names.arg = c('Summer','Fall','Winter','Spring'),
        col = c('#F49625'), border = NA,
        ylim = c(0,800), main = 'Absenteeism by Season', 
        xlab = 'Season', ylab = 'Count')

boxplot(df$Distance.from.Residence.to.Work, horizontal = T,
        xlab = 'Distance From Work (km)', main = 'Distance from Work',
        col = '#F49625')

d <- density(df$Son)
par(bty='n', bg = 'gray')
plot(d, main = 'Number of Kids\nAbsent Parents', xlab = 'Number of Kids',
     col = 'red', lwd = 3,
     sub = 'Source: http://www.uninove.br/curso/informatica-e-gestao-do-conhecimento/')

drinker <- table(df$Social.drinker,df$Day.of.the.week)
barplot(drinker, beside = T, 
        names.arg = c('Monday','Tuesday','Wednesday','Thursday','Friday'),
        legend.text = c('Social Drinker = No', 'Social Drinker = Yes'),
        xlab = 'Day of Week', main = 'Absences by Day of Week', ylim = c(0,90),
        col = c('#F49625','#FFFFFF'), border = TRUE, 
        args.legend = list(x ='topright', bty='n', inset=c(0,-.15)))

plot(df$Distance.from.Residence.to.Work, df$Transportation.expense, 
     xlab = 'Distance From Residence to Work', ylab = 'Transportation Expenses',
     main = 'Travel Expense by Distance', col = alpha('#F49625',.6), cex = 1, pch=16
     )

g <- ggplot(df, aes(x=Age, y=Distance.from.Residence.to.Work)) + geom_point(aes(size=Son, color=Education))
g <- g + scale_color_gradient(high = '#F49625', low = '#F9C48C')
g <- g + ylab('Distance From Residence to Work') + ggtitle('Worker Information')
g <- g + theme_bw() + labs(size='Kids')
