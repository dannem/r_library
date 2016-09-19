#Read in the ANT data (see ?ANT).
data(ANT)
head(ANT)
ezPrecis(ANT)

## Not run:
#Run an ANOVA on the mean correct RT data.
mean_rt_anova = ezANOVA(
  data = ANT[ANT$error==0,]
  , dv = .(rt)
  , wid = .(subnum)
  , within = .(cue,flank)
  , between = .(group)
)
#Show the ANOVA and assumption tests.
print(mean_rt_anova)

#Plot the main effect of group.
group_plot = ezPlot(
  data = ANT[ANT$error==0,]
  , dv = .(rt)
  , wid = .(subnum)
  , between = .(group)
  , x = .(group)
  , do_lines = FALSE
  , x_lab = 'Group'
  , y_lab = 'RT (ms)'
)
#Show the plot.
print(group_plot)


group_plot = group_plot +theme(
   panel.grid.major = element_blank()
  , panel.grid.minor = element_blank()
  )
  print(group_plot)
  
  
  group_plot_data = ezPlot(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , between = .(group)
    , x = .(group)
    , do_lines = FALSE
    , x_lab = 'Group'
    , y_lab = 'RT (ms)'
    , print_code = TRUE
  )
  
  #Re-plot the main effect of group, using the levels
  ##argument to re-arrange/rename levels of group
  group_plot = ezPlot(
    data = ANT[ANT$error==0,]
    , dv = .(rt)
    , wid = .(subnum)
    , between = .(group)
    , x = .(group)
    , do_lines = FALSE
    , x_lab = 'Group'
    , y_lab = 'RT (ms)'
    , levels = list(
      group = list(
        new_order = c('Treatment','Control')
        , new_names = c('Treatment\nGroup','Control\nGroup')
      )
    )
  )
  #Show the plot.
  print(group_plot)
  
  
  #barplot where x is the independent on the x-axis, y is the 
  #dependent on the y-axis and z is the independent given by 
  #different colored bars
  anova.plot<-function(x, y, z, ylab="y", xlab="x", 
                       ylim=c(0, max(xx)+max(yy)), length=0.05){
    
    #height of the bars
    xx<-tapply(y,list(z,x),mean)
    
    #standard deviation
    yy<-tapply(y,list(z,x),sd)
    
    #number of replicates
    zz<-tapply(y,list(z,x),length)
    
    #standard error
    er<-yy/sqrt(zz)
    
    #number of colors for bars
    w<-length(levels(z))
    
    #simple barplot without the errorbars
    barx<-barplot(xx, col=c(1:w), beside=T, ylab=ylab, xlab=xlab, 
                  ylim=ylim,xpd=FALSE)
    
    #box around the plot
    box()
    
    #error bars
    arrows(barx,xx+er, barx, xx, angle=90, code=1, length=length)
    
    #legend (after making the plot, indicate where the legend has 
    #to come with the mouse)
    legend(locator(1),c(levels(z)),fill=c(1:w),bty="n",cex=0.8)
  }
  
  data <- data.frame(Olfactory_Bulb = NA, OlfactoryEp_Total = NA, Olfactory_resp = NA,
                     functional_nonfunctional = c(rnorm(5, 10, 5), rnorm(11, 20, 5), rnorm(3, 30, 5),rnorm(1, 40, 5)),habitat = c(rep(1,5), rep(2,11), rep(3,3), rep(4,1)))
  
  rownames(data) <- c("Anoura_aequatoris", "Anoura_cadenai", "Anoura_canishina", "Anoura_caudifera", "Anoura_cultrata", "Anoura_fistulata", "Anoura_geoffroyi", "Anoura_latidens", "Anoura_luismanueli", "Anoura_peruana", "Choeroniscus_godmani", "Choeroniscus_periosus", "Choeroniscus_minor", "Choeronycteris_mexicana", "Glossophaga_commissarisi", "Glossophaga_leachii", "Glossophaga_longirostris", "Glossophaga_morenoi", "Glossophaga_soricina", "Hylonycteris underwoodi")
  
  library("plyr")
  
  library("ggplot2")
  
  graph_summary <- ddply(data, c("habitat"), summarize,
                         AVERAGE=mean(functional_nonfunctional),
                         SE=sqrt(var(functional_nonfunctional)/length(functional_nonfunctional)))
  
  ggplot(data = graph_summary, aes(x = habitat, y = AVERAGE, colour = habitat))+
    geom_point()+
    geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE))+
    theme(axis.text.x = element_text(angle = 90, hjust = 0, size=11),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(angle = 90, size=14))+
    scale_x_discrete("Habitat")+
    scale_y_continuous("Functional/Nonfunctional")
  
  
  mm = tapply(unf_all$value, list(unf_all$conf, unf_all$ims), mean)
  graph1 = barplot(mm, beside=T, legend=T)
  