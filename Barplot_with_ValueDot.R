library(ggplot2)
library(reshape2)
#Iron related gene error bar
tbl = read.table('191002_Iron_treat_SH-SY5Y_WST-1.txt',sep='\t',row.names = 1, header = TRUE)
#Change values into percentage
tbl = apply(t(tbl,1,function(x){(x/tbl[,1])*100}))
val.Mean = apply(tbl,2,mean)
val.Mean = c(as.numeric(val.Mean))
#Calculate standard error
val.se = apply(tbl,2,function(x){sd(x)/sqrt(5)})
val.se = c(as.numeric(val.se))
tbl.dot = tbl
#Melt table for ggplot2 plotting
tbl.dotplot = melt(tbl.dot)
tbl.dotplot$variable = rep(c('CTRL','100uM','200uM','500uM','1mM','2mM','5mM','10mM','20mM'), times = 1, each = 5) 
tbl.new = t(data.frame(val.Mean))
#Synchronize barplot name to dotplot variable name
colnames(tbl.new) = c('CTRL','100uM','200uM','500uM','1mM','2mM','5mM','10mM','20mM')
tbl.barplot = melt(tbl.new)
tbl.barplot$se = val.se
tbl.barplot = tbl.barplot[,2:4]
colnames(tbl.barplot) = c('variable', 'value', 'se')
#ggplot states barplot, geom_point states value of each variable
p = ggplot(tbl.barplot,aes(x = variable, y = value)) + geom_bar(stat='identity', position = 'dodge')+
  geom_errorbar(aes(ymin = value - se, ymax = value + se), position =position_dodge(0.9), width = 0.2)+
  geom_point(aes(x = variable, y = value), tbl.dotplot, position = position_dodge(0.9))+
  theme(text= element_text(size = 15), legend.title = element_blank())+ xlab(NULL) +ylab('Absorbance')
plot(p)
