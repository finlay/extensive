library(ggplot2)
args <- commandArgs(trailingOnly = TRUE)
fn <- args[1]
d <- read.csv(paste0(fn))

        
pdfn = paste0(gsub('.csv', '', fn), '.pdf')
pdf(width=5, height=3, file=paste0(fn,'.pdf'))
ggplot(data=d, aes(x=Name, group=1)) + 
    geom_line(aes(y=Mean)) + 
    geom_line(aes(y=MeanLB)) + 
    geom_line(aes(y=MeanUB)) + 
    geom_ribbon(aes(
        ymin=Mean-1.96*Stddev,
        ymax=Mean+1.96*Stddev),
        alpha=0.3)+
    xlab('Density of matrix') +
    ylab('Mean, micro seconds')
dev.off()
