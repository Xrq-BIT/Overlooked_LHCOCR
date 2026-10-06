suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(svglite)})
args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
folder <- if(length(args)) dirname(normalizePath(sub("^--file=","",args[1]))) else getwd()
h <- read.csv(file.path(folder,"source_data/SAM_training_history.csv"))
stopifnot(identical(h$epoch,1:10),all(is.finite(as.matrix(h))))
best <- h$epoch[which.min(h$val_loss)]
theme_set(theme_classic(base_size=9,base_family='sans') +
            theme(legend.position='top',legend.title=element_blank(),
                  plot.title=element_text(size=10,face='bold'),
                  axis.line=element_line(linewidth=.35)))
loss <- rbind(data.frame(epoch=h$epoch,value=h$train_loss,series='Training'),
              data.frame(epoch=h$epoch,value=h$val_loss,series='Validation'))
overlap <- rbind(data.frame(epoch=h$epoch,value=h$val_iou,series='IoU'),
                 data.frame(epoch=h$epoch,value=h$val_dice,series='Dice'))
p1 <- ggplot(loss,aes(epoch,value,colour=series,shape=series))+
  geom_vline(xintercept=best,linetype='dotted',colour='grey55',linewidth=.4)+
  geom_line(linewidth=.65)+geom_point(size=1.8)+
  scale_colour_manual(values=c(Training='#246B83',Validation='#B16E3A'))+
  scale_x_continuous(breaks=1:10)+labs(x='Epoch',y='Composite loss',title='a  Training and validation loss')
p2 <- ggplot(overlap,aes(epoch,value,colour=series,shape=series))+
  geom_vline(xintercept=best,linetype='dotted',colour='grey55',linewidth=.4)+
  geom_line(linewidth=.65)+geom_point(size=1.8)+
  scale_colour_manual(values=c(IoU='#246B83',Dice='#81629A'))+
  scale_x_continuous(breaks=1:10)+scale_y_continuous(limits=c(.78,.93),breaks=seq(.8,.92,.04))+
  labs(x='Epoch',y='Validation overlap score',title='b  Validation IoU and Dice')

for(i in 1:2) {
  base <- file.path(folder,paste0("SI_Fig1_",letters[i]))
  ggsave(paste0(base,".svg"),list(p1,p2)[[i]],width=3.6,height=3.05,device=svglite::svglite)
  rsvg::rsvg_pdf(paste0(base,".svg"),paste0(base,".pdf"))
}
ggsave(file.path(folder,"SI_Fig1_ab.svg"),p1+p2,width=7.2,height=3.05,device=svglite::svglite)
rsvg::rsvg_pdf(file.path(folder,"SI_Fig1_ab.svg"),file.path(folder,"SI_Fig1_ab.pdf"))
