# surplus production functions
schaefer = function(x){
  x+x*(1-x/2)
}

fox = function(x){
  x+x*(1-log(x)/log(2))
}

ggplot() + xlim(0, 4) + geom_function(aes(colour = "Schaefer"), fun = schaefer) + theme_classic()+
  theme(legend.title=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + geom_function(aes(colour="Fox"),fun=fox) +labs(x="Biomass", y="Surplus production")

# calculate the means for each each from the extracted simulations from the Schaefer model
schaefer_sim_means = numeric()
for(i in 1:48){
  schaefer_sim_means[i] = mean(schaefer_sim$B[,i])
}

schaefer_sim_means = data.frame(year=1967:2014, Biomass = schaefer_sim_means,model="Schaefer")

# calculate the means for each each from the extracted simulations from the Fox model
fox_sim_means = numeric()
for(i in 1:48){
  fox_sim_means[i] = mean(fox_sim$B[,i])
}

fox_sim_means = data.frame(year=1967:2014, Biomass = fox_sim_means,model="Fox")

# calculate the means for each each from the extracted simulations from the Schaefer (B[1]~0.9*K) model
schaefer_p1_sim_means = numeric()
for(i in 1:48){
  schaefer_p1_sim_means[i] = mean(schaefer_sim_p1$B[,i])
}

schaefer_p1_sim_means = data.frame(year=1967:2014, Biomass = schaefer_p1_sim_means,model="Schaefer (B[1]~0.9*K)")

# calculate the means for each each from the extracted simulations from the Fox (B[1]~0.9*K) model
fox_p1_sim_means = numeric()
for(i in 1:48){
  fox_p1_sim_means[i] = mean(fox_sim_p1$B[,i])
}

fox_p1_sim_means = data.frame(year=1967:2014, Biomass = fox_p1_sim_means,model="Fox (B[1]~0.9*K)")

# combine the simulated means
model_sim_means = rbind(schaefer_sim_means,fox_sim_means,schaefer_p1_sim_means,fox_p1_sim_means)
model_sim_means$model = as.factor(model_sim_means$model)

# plot the means from the four models over time
ggplot(model_sim_means,aes(year,Biomass,group=model)) +geom_line() +theme_classic() + ylab ("Biomass (1000's t)") +xlab("Year")

# plot the density of K for the four models
df = cbind(Schaefer = schaefer_sim$K,Fox = fox_sim$K,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$K,`Fox (B[1]~0.9*K)` = fox_sim_p1$K)
df = melt(df)
p1 = ggplot(df,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() +xlab("K (1000 t)") + labs(fill="Model")

# plot the density of r for the four models
df_r = cbind(Schaefer = schaefer_sim$r,Fox = fox_sim$r,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$r,`Fox (B[1]~0.9*K)` = fox_sim_p1$r)
df_r = melt(df_r)
p2 = ggplot(df_r,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() +xlab("r") + labs(fill="Model")

# plot the density of q for the four models
df_q = cbind(Schaefer = schaefer_sim$q,Fox = fox_sim$q,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$q,`Fox (B[1]~0.9*K)` = fox_sim_p1$q)
df_q = melt(df_q)
p3 = ggplot(df_q,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() +xlab("q") + labs(fill="Model")

# plot the density of sigma2 for the four models
df_sig = cbind(Schaefer = schaefer_sim$sigma2,Fox = fox_sim$sigma2,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$sigma2,`Fox (B[1]~0.9*K)` = fox_sim_p1$sigma2)
df_sig = melt(df_sig)
p4 = ggplot(df_sig,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() +xlab(expression(paste("Process error variance ",sigma^2))) + labs(fill="Model")

# plot the density of tau2 for the four models
df_tau = cbind(Schaefer = schaefer_sim$tau2,Fox = fox_sim$tau2,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$tau2,`Fox (B[1]~0.9*K)` = fox_sim_p1$tau2)
df_tau = melt(df_tau)
p5 = ggplot(df_tau,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() +xlab(expression(paste("Observation error variance ",tau^2))) + labs(fill="Model")

# plot the density of MSP for the four models
df_MSP = cbind(Schaefer = schaefer_sim$MSP,Fox = fox_sim$MSP,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$MSP,`Fox (B[1]~0.9*K)` = fox_sim_p1$MSP)
df_MSP = melt(df_MSP)
p6 = ggplot(df_MSP,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() + xlab("Maximum Surplus Production (1000 t)") + labs(fill="Model")

# plot the density of B_MSP for the four models
df_B_MSP = cbind(Schaefer = schaefer_sim$B_MSP,Fox = fox_sim$B_MSP,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$B_MSP,`Fox (B[1]~0.9*K)` = fox_sim_p1$B_MSP)
df_B_MSP = melt(df_B_MSP)
p7 = ggplot(df_B_MSP,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic()+  xlab("Biomass at MSP") + labs(fill="Model")

# plot the density of B_2014 for the four models
df_finalB = cbind(Schaefer = schaefer_sim$B[,48],Fox = fox_sim$B[,48],`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$B[,48],`Fox (B[1]~0.9*K)` = fox_sim_p1$B[,48])
df_finalB = melt(df_finalB)
p8 = ggplot(df_finalB,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() + xlab("2014 biomass") + labs(fill="Model")

# plot the density of B_2014/K for the four models
df_finalBK = cbind(Schaefer = schaefer_sim$B[,48]/schaefer_sim$K,Fox = fox_sim$B[,48]/fox_sim$K,`Schaefer (B[1]~0.9*K)` = schaefer_sim_p1$B[,48]/schaefer_sim_p1$K,`Fox (B[1]~0.9*K)` = fox_sim_p1$B[,48]/fox_sim_p1$K)
df_finalBK = melt(df_finalBK)
p9 = ggplot(df_finalBK,aes(x=value,fill=Var2)) +geom_density(alpha=0.3) + theme_classic() + xlab("(2014 biomass)/K") + labs(fill="Model")

# arrange all 9 plots in a grid
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3,nrow=3,common.legend = TRUE,legend = 'right')
