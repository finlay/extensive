library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
fn <- args[1]
d <- read.csv(paste0(fn))

pdf(width=8, height=5, 
    file=paste0(gsub('.csv', '', fn), '.pdf'))
ggplot(data=d, aes(x=Name, group=1)) + 
    geom_line(aes(y=Mean*1000)) + 
    geom_line(aes(y=MeanLB*1000)) + 
    geom_line(aes(y=MeanUB*1000)) + 
    geom_ribbon(aes(
        ymin=(Mean-1.96*Stddev)*1000,
        ymax=(Mean+1.96*Stddev)*1000),
        alpha=0.3)+
    scale_x_discrete(labels=seq(0.0,1.0,0.1)) +
    xlab('Probability of non-zero elements, p') +
    ylab('Mean, micro seconds')
dev.off()
