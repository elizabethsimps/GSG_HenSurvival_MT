# Known fate survival model(s) in RMark
### Elizabeth Simpson & David Messmer
### Started: 2023-June-13
### Updated: 2024-Aug-13

# *** Run after "generate_encounter_histories.R" and "assess_habitat_covariates.R"
# *** Double check working directory

library(RMark)

# Set up model set for the 341 included hens 
rmark.EH.hab.age[is.na(rmark.EH.hab.age)] <- 0 

hab.age.hens.processed = process.data(rmark.EH.hab.age, 
                                  model = "Known",
                                  begin.time = 2010,
                                  groups = 'age',
                                  age.var= 1,
                                  initial.age= c(0,1))

#Make design data
#Add age bins, https://www.rdocumentation.org/packages/RMark/versions/2.2.0/topics/make.design.data
hab.age.hens.ddl = make.design.data(hab.age.hens.processed,
                                parameters=list(S=list(age.bins=c(0,1,13))))

hab.age.hens.ddl$S$yrlng <- ifelse(hab.age.hens.ddl$S$group== 0 & hab.age.hens.ddl$S$age=='[0,1]', yes=1, no=0)
hab.age.hens.ddl$S$adult <- ifelse(hab.age.hens.ddl$S$age=='(1,13]', yes=1, no=0)

# setup a function
hab.age.run.hens <- function() {
  #  Define range of models for S
  S.dot = list(formula =  ~ 1)
  S.time = list(formula =  ~ time)
  S.age = list(formula = ~ age)
  S.age.Time = list(formula =  ~ time + age)
  
  S.MAT = list(formula = ~ MAT_)
  S.MAT.PFGsd.I = list(formula = ~ MAT_ * PFGsd_)
  S.MAT.BGmn.I = list(formula = ~ MAT_ * BGmn_)
  
  S.MCP = list(formula = ~ MCP_)
  S.MCP.SHRmn.I = list(formula = ~ MCP_ * SHRmn_)
  S.MCP.PFGmn.I = list(formula = ~ MCP_ * PFGmn_)
  S.MCP.AFGmn.I = list(formula = ~ MCP_ * AFGmn_)
  S.MCP.BGmn.I = list(formula = ~ MCP_ * BGmn_)
  S.MCP.SHRmn.PFGsd.I = list(formula = ~ MCP_ * SHRmn_ * PFGsd_)
  S.MCP.SHRmn.BGmn.I = list(formula = ~ MCP_ * SHRmn_ * BGmn_)
  
  S.SHRmn = list(formula = ~ SHRmn_)
  S.PFGmn = list(formula = ~ PFGmn_)
  S.PFGsd = list(formula = ~ PFGsd_)
  S.AFGmn = list(formula = ~ AFGmn_)
  S.BGmn = list(formula = ~ BGmn_)
 
  # Create model list
  model.list = create.model.list("Known")
  
  # NOTE: to avoid having all the output for each model appear when you
  # call the function, add ', output=FALSE' after 'ddl=fawns.ddl' below.
  # Here, I don't do that so you can see the output for each model,
  # but this might not be desired if you have many models.
  hab.age.hens.results = mark.wrapper(model.list, data = hab.age.hens.processed,
                                  ddl = hab.age.hens.ddl, invisible = TRUE, threads = 2)
  
  # Return model table and list of models
  return(hab.age.hens.results)
}

hab.age.hens.results = hab.age.run.hens()

# OUTPUTS
# Model ranking table
xtable(hab.age.hens.results$model.table, digits = 3)

# Survival results
hab.age.hens.results$S.dot$results$real # overall study-wise survival
hab.age.hens.results$S.age$results$real # differences in yearling versus adult survival

xtable(hab.age.hens.results$S.time$results$real, digits=3) # inter-annual variation in survival

xtable(exp(hab.age.hens.results$S.MAT$results$beta), digits = 3) # log-odds ratios of top model

##########################################################
# PLOTTING outputs - make a 3 panel plot interannual var, age var, and predicted MAT var.

# Make table of annual survival results with years
hab.age.year.tab <- hab.age.hens.results$S.time$results$real
hab.age.year.tab$year<- 2011:2020

# Make table of age survival results
hab.age.age.tab <- hab.age.hens.results$S.age$results$real
hab.age.age.tab$age <- c('Yearling', "Adult")

# PLOTTING
S.inter.plot <- ggplot(data = hab.age.year.tab, aes(x = as.factor(year), y = estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl),width=0, linewidth=1, col='darkgrey')+
  geom_point(size=3)+
  labs(x='Year', y='Survival estimate')+
  theme_bw()+
  lims(y=c(0.05,0.95))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

# Try to get these to plot flip flopped
S.age.plot <- ggplot(data = hab.age.age.tab, aes(x = factor(age, levels=c("Yearling", "Adult")), y = estimate)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl),width=0, linewidth=1, col='darkgrey')+
  geom_point(size=3)+
  labs(x='Age', y='Survival estimate')+
  theme_bw()+
  lims(y=c(0.47,0.87))
#theme(axis.title.y=element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank())

# Predicted survival across MAT
# Predicted values of S, PLOTTED
# Set up all the ranges of covariates to predict off of and then do those predictions & plots
# MAT
MAT.val <- min(habitat$MAT)+(0:29)*(max(habitat$MAT)-min(habitat$MAT))/30
p.MAT <- data.frame(MAT.val, MAT.val, MAT.val, MAT.val, MAT.val, MAT.val, MAT.val, MAT.val, MAT.val, MAT.val)
colnames(p.MAT) <- c("MAT_2011", "MAT_2012", "MAT_2013", "MAT_2014", "MAT_2015", "MAT_2016", "MAT_2017", "MAT_2018", "MAT_2019", "MAT_2020")

predicted_S <- covariate.predictions(hab.age.hens.results$S.MAT,
                                     data= p.MAT,
                                     indices=c(1:10))

pred.MAT <- ggplot(data = predicted_S$estimates, aes(x = MAT_2011, y = estimate)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "gray80") +
  geom_smooth(col = "black", formula = y~x, method = "loess") +
  labs(x = "MAT (°C)", y = "Predicted survival") +
  theme_bw()+
  lims(y=c(0.34,0.91))

jpeg("../analysis/figures/Hen_Survival_Res_Fig.jpeg", width=9, height=3, units="in", res=300)
ggarrange(S.inter.plot, S.age.plot, pred.MAT,
          ncol=3, nrow=1, align="hv", labels=c("(a)", "(b)","(c)"),hjust=0.01, font.label = list(size = 11, face = "bold"))
dev.off()