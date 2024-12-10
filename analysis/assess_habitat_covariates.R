# Preliminary analysis & setup of habitat covariates
# Elizabeth Simpson
### Started: 2023-June-13
### Updated: 2024-Aug-23

# *** Run after "generate_encounter_histories.R"
# *** Double check working directory

library(dplyr)
library(xtable)
library(ggplot2)
library(ggpubr)
library(corrplot)

########################################################
# WEATHER COVARS.
# Temperature - mean daily temperature calculated for each month
habitat <- read.csv("HenHabCovar.csv", as.is=TRUE)
habitat <- habitat[,-1]

# Summary of covariates for whole study - reported in the text
all.covar.sum <- as.data.frame(habitat %>%
                                 summarize(MATmean = mean(MAT),
                                           MATse = sd(MAT)/sqrt(length(MAT)),
                                           CPmean = mean(CP),
                                           CPse = sd(CP)/sqrt(length(CP)),
                                           SHRmean = mean(SHRmn),
                                           SHRse = sd(SHRmn)/sqrt(length(SHRmn)),
                                           PFGmean = mean(PFGmn),
                                           PFGse = sd(PFGsd)/sqrt(length(PFGmn)),
                                           AFGmean = mean(AFGmn),
                                           AFGse = sd(AFGmn)/sqrt(length(AFGmn)), 
                                           BGmean = mean(BGmn),
                                           BGse = sd(BGmn)/sqrt(length(BGmn))
                                 ))

xtable(all.covar.sum, digits=3)


by.yr.covar.sum <- as.data.frame(habitat %>%
                                   group_by(year) %>% 
                                   summarize(MATmean = mean(MAT),
                                             MATmean.se = sd(MAT)/sqrt(length(MAT)),
                                             
                                             MAT_SD = mean(MATsd),
                                             MAT_SD.se = sd(MATsd)/sqrt(length(MATsd)),
                                             
                                             MCPmean = mean(MCP),
                                             MCPmean.se = sd(MCP)/sqrt(length(MCP)),
                                             
                                             MCP_SD = mean(MCPsd),
                                             MCP_SD.se = sd(MCPsd)/sqrt(length(MCPsd)),
                                             
                                             SHRmean = mean(SHRmn),
                                             SHRmean.se = sd(SHRmn)/sqrt(length(SHRmn)),
                                             
                                             SHR_SD = mean(SHRsd),
                                             SHR_SD.se = sd(SHRsd)/sqrt(length(SHRsd)),
                                             
                                             PFGmean = mean(PFGmn),
                                             PFGmean.se = sd(PFGmn)/sqrt(length(PFGmn)),
                                             
                                             PFG_SD = mean(PFGsd),
                                             PFG_SD.se = sd(PFGsd)/sqrt(length(PFGsd)),
                                             
                                             AFGmean = mean(AFGmn),
                                             AFGmean.se = sd(AFGmn)/sqrt(length(AFGmn)),
                                             
                                             AFG_SD = mean(AFGsd), 
                                             AFG_SD.se = sd(AFGsd)/sqrt(length(AFGsd)),
                                             
                                             BGmean = mean(BGmn),
                                             BGmean.se = sd(BGmn)/sqrt(length(BGmn)),
                                             
                                             BG_SD = mean(BGsd),
                                             BG_SD.se = sd(BGsd)/sqrt(length(BGsd))
                                   ))

xtable(by.yr.covar.sum[,c(1,24:25)], digits=3)



# PLOT the covariates across all of the years
habitat$Year <- as.factor(habitat$year)

# CORRELATION plot
# Use Spearman's correlation because all variables (mostly weather) are not normally distributed
cor.mat <- cor(habitat[,c(4:5,7:8, 15:16, 13:14, 9:12)], method = "spearman") # Anything over 0.3 is considered too correlated to be in the same model

jpeg("../analysis/figures/covar_correlation_raw.jpeg", width=8, height=8, units="in", res=300)
corrplot(cor.mat, type="upper", diag=FALSE, tl.col="black")
dev.off()

# PALLETTE
CBPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#FFDAB9","#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p.mcp <- habitat %>%
  ggplot( aes(x=Year, y=MCP, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab(expression("MCP (mm)")) +
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.mcp.sd <- habitat %>%
  ggplot( aes(x=Year, y=MCPsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab(expression("Variation (SD) in MCP (mm)")) +
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.mat <- habitat %>%
  ggplot( aes(x=Year, y=MAT, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("MAT (°C)")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.mat.sd <- habitat %>%
  ggplot( aes(x=Year, y=MATsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("Variation (SD) in MAT (°C)")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

jpeg("../analysis/figures/weather_covar_across_years.jpeg", width=9, height=7, units="in", res=300)
ggarrange(p.mcp, p.mcp.sd, p.mat, p.mat.sd, ncol=2, common.legend=TRUE, legend="right", labels=c("(a)", "(b)","(c)", "(d)"), nrow=2)
dev.off()

# VEGETATION covariates
p.shr <- habitat %>%
  ggplot( aes(x=Year, y=SHRmn, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("Shrub Cvr.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.shr.sd <- habitat %>%
  ggplot( aes(x=Year, y=SHRsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("Shrub Cvr. Var.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.pfg <- habitat %>%
  ggplot( aes(x=Year, y=PFGmn, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("PFG Cvr.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.pfg.sd <- habitat %>%
  ggplot( aes(x=Year, y=PFGsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("PFG Cvr. Var.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.afg <- habitat %>%
  ggplot( aes(x=Year, y=AFGmn, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("AFG Cvr.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.afg.sd <- habitat %>%
  ggplot( aes(x=Year, y=AFGsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("AFG Cvr. Var.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.bg <- habitat %>%
  ggplot( aes(x=Year, y=BGmn, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("BG Cvr.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

p.bg.sd <- habitat %>%
  ggplot( aes(x=Year, y=BGsd, fill=Year)) + 
  geom_violin() +
  xlab("Year") +
  ylab("BG Cvr. Var.")+
  theme_bw() + 
  scale_fill_manual(values=CBPalette)

jpeg("../analysis/figures/vegetation_covar_across_years.jpeg", width=15, height=19, units="in", res=300)
ggarrange(p.shr, p.shr.sd, p.pfg, p.pfg.sd, p.afg, p.afg.sd, p.bg, p.bg.sd, common.legend=TRUE, legend="right",
          ncol=2, labels=c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)"), nrow=4)
dev.off()

# VISUALIZE the relationship between mean and variation within variables
# WEATHER
# Temperature
cor.MAT <- habitat %>%
  ggplot(aes(x=MAT, y=MATsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("MAT (°C)") +
  ylab("Variation (SD) in MAT (°C)")+
  theme_bw() 

cor.MCP <- habitat %>%
  ggplot(aes(x=MCP, y=MCPsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("MCP (mm)") +
  ylab("Variation (SD) in MCP (mm)")+
  theme_bw() 

jpeg("../analysis/figures/weather_mean_sd.jpeg", width=8, height=3.5, unit="in",res=300)
ggarrange(cor.MAT, cor.MCP,ncol=2, nrow=1, common.legend=TRUE, legend="right",
          labels=c("(a)","(b)"))
dev.off()

# VEGETATION
cor.shr <- habitat %>%
  ggplot(aes(x=SHRmn, y=SHRsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("Shrub cvr. (%)") +
  ylab("Variation (SD) in Shrub cvr. (%)")+
  theme_bw() 

cor.pfg <- habitat %>%
  ggplot(aes(x=PFGmn, y=PFGsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("PFG cvr. (%)") +
  ylab("Variation (SD) in PFG cvr. (%)")+
  theme_bw() 

cor.afg <- habitat %>%
  ggplot(aes(x=AFGmn, y=AFGsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("AFG cvr. (%)") +
  ylab("Variation (SD) in AFG cvr. (%)")+
  theme_bw() 

cor.bg <- habitat %>%
  ggplot(aes(x=BGmn, y=BGsd, colour=Year)) + 
  geom_point() +
  scale_colour_manual(values=CBPalette) +
  xlab("Bare ground (%)") +
  ylab("Variation (SD) in Bare ground (%)")+
  theme_bw() 

jpeg("../analysis/figures/vegetation_mean_sd.jpeg", width=8, height=7, unit="in",res=300)
ggarrange(cor.shr, cor.pfg, cor.afg, cor.bg, ncol=2, nrow=2, common.legend=TRUE, legend="right",
          labels=c("(a)","(b)","(c)","(d)"))
dev.off()

# SET up habitat covars for analysis in MARK
wide.var <- as.data.frame(habitat[,-c(2,6,17)] %>%
                            group_by(HenID, year) %>%
                            pivot_wider(
                              names_from = c(year),
                              values_from = c(MAT, MATsd, MCP, MCPsd, AFGmn, AFGsd, BGmn, BGsd, PFGmn, PFGsd, SHRmn, SHRsd)
                            ))

rmark.EH.hab.age <- merge(rmark.EH.age.ID, wide.var, by.x="HenID", by.y="HenID")
nrow(rmark.EH.hab.age)

# What happened to the hens included in the survival analysis
included.summary <- individuals[individuals$HenID %in% rmark.EH.hab.age$HenID,]
included.summary <- table(included.summary$Fate)
names(included.summary) <- c("no-fate", "censored-lost", "pred-mammal", "pred-avian", "pred-unknown", "fence", "vehicle","dropped-collar","handling","undetermined")