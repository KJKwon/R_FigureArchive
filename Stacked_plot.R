library(ggplot2)
library(reshape2)
library(data.table)
library(wesanderson)
Dop.neurons <- c('foo1', 'foo2', 'foo3', 'foo4')
hMLO3.results <- c(17,9,74,60)
hMLO3.ratio <- round((f3.results/sum(f3.results))*100, digits = 2)
hMLO4.results <- c(13,9,61,86)
hMLO4.ratio <- round((f4.results/sum(f4.results))*100, digits = 2)
MS126.results <- c(31,0,8,556)
MS126.ratio <- round((f126.results/sum(f126.results))*100, digits = 2)
p.ready <- data.table(f3 = f3.ratio, 
                     f4 = f4.ratio,
                     f126 = f126.ratio)
p.ready <- as.data.frame(p.ready)
p.ready$foos <- factor(foos, levels = foos)
p.ready = melt(p.ready)
p <- ggplot(p.ready, aes(x = variable, y = value, fill = foos)) +
  geom_bar(stat = 'identity', width = 0.5) + 
  scale_y_continuous(limits = c(0,101), expand = c(0,0),
                     breaks = seq(0,100, by = 10),
                     labels = scales::percent_format(suffix = '%', 
                                                     preifx = '',
                                                     scale = 1))+
  scale_fill_manual(values=wes_palette(n=4, name='GrandBudapest2'))+
  theme(axis.line = element_line(size = 1, colour = 'black'), 
        axis.text.x = element_text(size = 18, colour = 'black'),
        axis.text.y = element_text(size = 15, colour = 'black'),
        axis.title = element_blank(), 
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(color = 'grey80'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
        )+
  labs(fill = 'foos')
p
