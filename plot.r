library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
fn <- args[1]
d <- read.csv(paste0(fn))

pdf(width=5, height=3, 
    file=paste0(gsub('.csv', '', fn), '.pdf'))
ggplot(data=d, aes(x=Name, group=1)) + 
    geom_line(aes(y=Mean*1000)) + 
    geom_line(aes(y=MeanLB*1000)) + 
    geom_line(aes(y=MeanUB*1000)) + 
    geom_ribbon(aes(
        ymin=(Mean-1.96*Stddev)*1000,
        ymax=(Mean+1.96*Stddev)*1000),
        alpha=0.3)+
    xlab('Density of matrix') +
    ylab('Mean, micro seconds')
dev.off()
