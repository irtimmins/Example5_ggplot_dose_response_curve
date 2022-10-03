


library(ggplot2)



###################################################################################################
# Read in individual level simulated survival data, with predicted hazard ratios and CIs.
# (Generated previously from stata mkspline, stcox, and predict loghaz functions).
###################################################################################################

pa.df <- read.csv("Sim_physial_activity_data_dose_response.txt")
 
head(pa.df)

# create confidence interval.

pa.df$rel.haz.95.low <- 0
pa.df$rel.haz.95.high <- 0
pa.df$rel.haz.95.low <- exp(pa.df$logcil)
pa.df$rel.haz.95.high <- exp(pa.df$logciu)

pdf(file = "Spline_fig.pdf", width = 6, height = 4)
ggplot(pa.df, aes(x = PA, y = relhaz))+
    theme_classic() +
    theme(strip.text = element_text(size = 12)  ,
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.text.x= element_text(size = 11)) +
    geom_hline(yintercept= c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05), linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.80, xend = 100, yend= 0.80) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.85, xend = 100, yend= 0.85) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.90, xend = 100, yend= 0.90) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.95, xend = 100, yend= 0.95) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 1.00, xend = 100, yend= 1.00) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 1.05, xend = 100, yend= 1.05) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_ribbon(aes(ymin=rel.haz.95.low, ymax=rel.haz.95.high),alpha = 0.8, fill = "grey70", colour = "grey60")+
    geom_line()+   
    geom_point(aes(x=10,y=1),colour="black", size = 0.7)+
    scale_y_continuous(name = "Hazard Ratio", breaks =  c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05, 1.10), limits = c(0.78, 1.07), expand = c(0,0))+
    scale_x_continuous(name = "Leisure-time Physical Activity (Percentile)", breaks = c(0,10,20,30,40,50,60, 70, 80, 90, 100), limits = c(0,102), expand = c(0,0))
dev.off()







































pa.df <- read.csv("Phys_data_covariates5_meno_cc_spline1.csv")
length(pa.df$censor_id)
pa.df <- pa.df[!duplicated(pa.df$censor_id),]
sample.vec <- sample(1:length(pa.df$censor_id), 1000)
pa.df <- pa.df[sample.vec,]
pa.df <- pa.df[order(pa.df$dec_phys1),]
pa.df$PA <- 0
pa.df$PA <- 100*pa.df$dec_phys1
 
head(pa.df)

pa.df$rel.haz.95.low <- 0
pa.df$rel.haz.95.high <- 0
pa.df$rel.haz.95.low <- exp(pa.df$logcil)
pa.df$rel.haz.95.high <- exp(pa.df$logciu)

pdf(file = "Spline_fig1.pdf", width = 6, height = 4)
ggplot(pa.df, aes(x = PA, y = relhaz))+
    theme_classic() +
    theme(strip.text = element_text(size = 12)  ,
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.text.x= element_text(size = 11)) +
    geom_hline(yintercept= c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05), linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.80, xend = 100, yend= 0.80) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.85, xend = 100, yend= 0.85) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.90, xend = 100, yend= 0.90) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 0.95, xend = 100, yend= 0.95) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 1.00, xend = 100, yend= 1.00) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_segment(aes(x= 0, y= 1.05, xend = 100, yend= 1.05) , linetype = "solid", col = "gray85", size = 0.25)+
    geom_ribbon(aes(ymin=rel.haz.95.low, ymax=rel.haz.95.high),alpha = 0.8, fill = "grey70", colour = "grey60")+
    geom_line()+   
    geom_point(aes(x=10,y=1),colour="black", size = 0.7)+
    scale_y_continuous(name = "Hazard Ratio", breaks =  c(0.80, 0.85, 0.90, 0.95, 1.00, 1.05, 1.10), limits = c(0.78, 1.07), expand = c(0,0))+
    scale_x_continuous(name = "Leisure-time Physical Activity (Percentile)", breaks = c(0,10,20,30,40,50,60, 70, 80, 90, 100), limits = c(0,102), expand = c(0,0))
dev.off()








