install.packages("vegan") 
library("vegan") 
setwd("D:/xx") 
otu <- read.delim('otu_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)#读取OTU表格
otu<- data.frame(t(otu))
dis<- read.delim('bray.txt',row.names=1,sep = '\t',stringsAsFactors = F)
group<- read.delim('group.txt',sep = '\t',stringsAsFactors = F)
library("vegan")
distance<- vegdist(otu,method = 'bray')
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = TRUE)
write.csv(as.matrix(distance),'distance.csv',quote = F)

ordiplot(scores(pcoa)[,c(1,2)],type='t')
summary(pcoa)
pcoa$eig
point<- data.frame(pcoa$point)
write.csv(point,'pcoa.sample.csv')
species<- wascores(pcoa$points[,1:2],otu)
write(species,'pcoa.otu.csv')
pcoa_eig<-(pcoa$eig)[1:2]/sum(pcoa$eig)

sample_site <- data.frame({pcoa$point})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')

sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)
write.csv(sample_site, 'sample_site.csv', quote = F)
sample_site$site <- factor(sample_site$site, levels = c('A', 'B', 'C', 'D'))
sample_site$deal <- factor(sample_site$deal, levels = c('low', 'high'))
sample_site$time <- factor(sample_site$time, levels = c('1', '2', '3', '4'))

library(plyr)
group_border <- ddply(sample_site, 'site', function(df) df[chull(df[[2]], df[[3]]), ])

library(ggplot2)

pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2, group = site)) +
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) + 
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.4) +
  geom_polygon(data = group_border, aes(fill = site)) + 
  geom_point(aes(color = time, shape = deal), size = 1.5, alpha = 0.8) + 
  scale_shape_manual(values = c(17, 16)) + 
  scale_color_manual(values = c('yellow', 'orange', 'red', 'red4')) + 
  scale_fill_manual(values = c('#C673FF2E', '#73D5FF2E', '#49C35A2E', '#FF985C2E')) + 
  guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2), color = guide_legend(order = 3)) + 
  labs(x = paste('PCoA axis1: ', round(100 * pcoa_eig[1], 2), '%'), y = paste('PCoA axis2: ', round(100 * pcoa_eig[2], 2), '%')) +
  
  annotate('text', label = 'A', x = -0.31, y = -0.15, size = 5, colour = '#C673FF') +
  annotate('text', label = 'B', x = -0.1, y = 0.3, size = 5, colour = '#73D5FF') +
  annotate('text', label = 'C', x = 0.1, y = 0.15, size = 5, colour = '#49C35A') +
  annotate('text', label = 'D', x = 0.35, y = 0, size = 5, colour = '#FF985C')

ggsave('PCoA.pdf', pcoa_plot, width = 6, height = 5)
