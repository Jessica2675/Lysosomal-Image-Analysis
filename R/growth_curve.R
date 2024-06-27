library(ggplot2)

growth<-read.csv("../excel files/growth_curve.csv")
growth$in.cm2 <- rowMeans(subset(growth, select = c(X1, X2, X3, X4, X5, X6, X7)), na.rm = TRUE) / (growth$plate.diameter/2)^2 * pi

growth$condition <- paste(growth$cell.line, growth$treatment)
growth$Time<-growth$days.after.dilution*24


p1 <- ggplot(growth, aes(days.after.dilution, in.cm2, colour = treatment)) + geom_point()
p1+facet_wrap(vars(cell.line))

p1+theme_classic()

growth[is.na(growth)] <- 0
growth$aopimean <- (growth$X1*growth$aopi.1 + growth$X2*growth$aopi.2 + growth$X3*growth$aopi.3 + growth$X4*growth$aopi.4 + growth$X5*growth$aopi.5 + growth$X6*growth$aopi.6 + growth$X7*growth$aopi.7)/(growth$X1+growth$X2+growth$X3+growth$X4+growth$X5+growth$X6+growth$X7)

qplot(cell.line, aopimean, data = growth, colour = treatment)


##############################
####poster
growth_poster<-subset(growth,light.condition=='light')
p2<-ggplot(data=growth_poster,aes(x=Time,y=OD730nm,group=strain, color=strain))+geom_line()+geom_point()
p2+theme_classic()+scale_color_brewer(palette='Dark2')+xlab('Time (h)')+ylab(bquote(OD['730nm'])) + theme(legend.position = "none")


###with replicates included
growth_poster$strain_replicate<-paste(growth_poster$strain,growth_poster$replicate.number)
p3<-ggplot(data=growth_poster,aes(x=Time,y=OD730nm,group=strain_replicate, color=strain))+geom_line()+geom_point()
p3+theme_classic()+scale_color_brewer(palette='Dark2')+xlab('Time (h)')+ylab(bquote(OD['730nm'])) 

olive<-ggplot(data=subset(growth_poster,strain=='olive'),aes(x=days.after.dilution,y=OD730nm,group=strain_replicate, color=strain_replicate))+geom_line()+geom_point()+theme_classic()+ylab(bquote(OD['730nm'])) 
k264q<-ggplot(data=subset(growth_poster,strain=='K264Q'),aes(x=days.after.dilution,y=OD730nm,group=strain_replicate, color=strain_replicate))+geom_line()+geom_point()+theme_classic()+ylab(bquote(OD['730nm'])) 
psba2<-ggplot(data=subset(growth_poster,strain=='PsbA2'),aes(x=days.after.dilution,y=OD730nm,group=strain_replicate, color=strain_replicate))+geom_line()+geom_point()+theme_classic()+ylab(bquote(OD['730nm'])) 
y246f<-ggplot(data=subset(growth_poster,strain=='Y246F'),aes(x=days.after.dilution,y=OD730nm,group=strain_replicate, color=strain_replicate))+geom_line()+geom_point()+theme_classic()+ylab(bquote(OD['730nm'])) 
