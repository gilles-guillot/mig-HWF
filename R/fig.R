# Figures for paper
# setwd("~/Dropbox/Work/WHO/SHARED/IPUMS/IPUMS_MB/R")

# data loading -----
if(!require('pacman')) install.packages('pacman')
library(pacman)
p_load( tidyverse ,
        kableExtra ,
        readxl,
        # gt ,
        # htmlTable ,
        magrittr,
        reshape2 ,
        wesanderson )

# .Rdat file loaded next line was produced by ./format.R
load('~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/AllHWF-Data_Formatted.Rdat')
# load('~/Dropbox/Work/WHO/SHARED/IPUMS/IPUMS_MB/MB_2021-05-10/SubHWF-Data_Formatted.Rdat')

source("~/Dropbox/Work/WHO/SHARED/IPUMS/R/tasks/EDA_inmig.R")
source("~/Dropbox/Work/WHO/SHARED/IPUMS/R/tasks/comp_ratio.R")

load('~/Dropbox/Work/WHO/SHARED/IPUMS/Data/misc_data/counpop.Rdat')
# counpop = read.table("~/Dropbox/Work/WHO/SHARED/IPUMS/Country_pop_World_Bank.csv.txt",
#                      sep=";",header=TRUE,skip = 2)

# load HRH costs data 
med_cost = read_excel("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/misc_data/Salaries for HRH Costing - 2 Sept 2020.xlsx")
med_cost = med_cost %>% mutate(country = Country) %>% select(country,Professionals,Nurses,`Patient Support`)
# check country names
# unique(dat$country)[ !unique(dat$country) %in% med_cost$country ]
med_cost$country[med_cost$country=="United States of America"] = "United States"


# load IMF GDP data
gdp = read.csv("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/misc_data/GDP_IMF.csv",
               sep="\t", 
               header=TRUE,
               skip = 1,
               check.names = FALSE)
gdp = gdp %>% select(country,`2010`)
gdp$GDP2010_MUSD = gdp$`2010`  
gdp = gdp %>% select(-`2010`)
gdp$country = gdp$country %>% str_sub(2)

# END dta loading

# Inward migration population sizes by country ----


res = dat %>%   group_by(country) %>%
  summarise(TotWF = sum(perwt),
            InMigWF_comp = sum(perwt * (mig=="mig")),
            regionw.ord = first(regionw.ord)) %>%
  mutate(PctMigWF_comp = 100*InMigWF_comp/TotWF) %>% 
  arrange(PctMigWF_comp) %>% 
  mutate(country = factor(country,
                          levels=country,ordered = TRUE))

res %>% kable(caption="Inward migration population size",align = "c",digits=1) %>% 
  kable_styling(full_width = F) 

ggplot() + 
  geom_col(data = res, 
           aes(y=country,
               x=PctMigWF_comp,
               fill = regionw.ord),
           width = 0.5, position = position_dodge(0.7)) + 
  # labs(title="Percentage of migrants in local health work force") + 
  xlab("") + ylab("") + theme(legend.title=element_blank()) +
  ggsave("~/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/Inward_by_country.png",
         width=16,height=16,units="cm")











# Financial transfer inward migration ----

res = dat %>%   group_by(country,ISCO08WHO_yrstud) %>%
  summarise(TotWF = sum(perwt),
            InMigWF_comp = sum(perwt * (mig=="mig")),
            InMigWF_bplcountry = sum(perwt * mig_bplcountry),
            regionw.ord = first(regionw.ord))

# merge with costs data 
res %<>% inner_join(med_cost)
# financial transfer by job categories
res %<>% mutate(transf_comp = 
                  case_when(ISCO08WHO_yrstud == "Short" ~  InMigWF_comp * 3 * Nurses,
                            ISCO08WHO_yrstud == "Medium" ~ InMigWF_comp * 4 * Nurses,
                            ISCO08WHO_yrstud == "Long" ~   InMigWF_comp * (3 * Nurses  + Professionals)),
                transf_bplcountry = 
                  case_when(ISCO08WHO_yrstud == "Short" ~  InMigWF_bplcountry * 3 *  Nurses,
                            ISCO08WHO_yrstud == "Medium" ~ InMigWF_bplcountry * 4 * Nurses,
                            ISCO08WHO_yrstud == "Long" ~   InMigWF_bplcountry * (3 * Nurses  + Professionals)))
# from USD to MUSD
res %<>% mutate(transf_comp = transf_comp *1e-6 ,
                transf_bplcountry =   transf_bplcountry*1e-6 )

# summing over yrstud (job categories)
res %<>% 
  summarise(TotWF = sum(TotWF , na.rm = TRUE),
            InMigWF_comp  = sum(InMigWF_comp,na.rm = TRUE),	
            InMigWF_bplcountry  = sum(InMigWF_bplcountry,na.rm = TRUE),
            #PctMigWF_comp	 = first(PctMigWF_comp),
            #PctMigWF_bplcountry  = sum(PctMigWF_bplcountry,na.rm = TRUE), 
            transf_comp = sum(transf_comp,na.rm = TRUE),
            transf_bplcountry = sum(transf_bplcountry,na.rm = TRUE),
            regionw.ord = first(regionw.ord))

# merge with GDP data
res %<>% inner_join(gdp)


res %<>% arrange(transf_comp)  
res %<>% mutate(Region = regionw.ord,country = factor(country,
                                 levels=country,ordered = TRUE))
ggplot(res) + 
  geom_col(aes(y=country,
               x=transf_comp,
               #fill = "darkred" 
               fill = Region),
           width = 0.8, position = position_dodge(0.7)) + 
  labs(title="") + 
  xlab("millions of USD (non linear scale)") + ylab("") +
  scale_x_sqrt()+ #theme(legend.position = "none") + 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/FinTrans_absolute.png",
         width=16,height=16,units="cm")


res %<>% mutate(transf_comp_over_GDP2010_MUSD = 100*transf_comp/ GDP2010_MUSD)  
res %<>% arrange(transf_comp_over_GDP2010_MUSD)
res %<>% mutate(country = factor(country,
                                 levels=country,
                                 ordered = TRUE))
ggplot(res) + 
  geom_col(aes(y=country,
               x=transf_comp_over_GDP2010_MUSD,
               fill = "darkred"),
           width = 0.5, position = position_dodge(0.7)) + 
  labs(title="") + 
  xlab("Financial transfer in percentage of GDP") + theme(legend.position = "none") + 
  ylab("")+ 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/FinTrans_relative.png",
         width=16,height=16,units="cm")






# Outward migration -----

{
  cy = "ARM2011"
  sumperwt = sum(dat$perwt[dat$countryyear==cy] )
  TotWF = dat %>% filter(countryyear == cy) %>%
    group_by(bplcountry) %>%
    summarise(TotWF = sum(perwt) )
  PctWF = TotWF %>% 
    mutate(PctWF = 100* TotWF / sumperwt) %>% 
    select(-TotWF)
  colnames(TotWF)[2] = cy
  colnames(PctWF)[2] = cy
  
  for(i in 2:length(sort(unique(dat$countryyear))) )
  {
    cy = sort(unique(dat$countryyear))[i]
    sumperwt = sum(dat$perwt[dat$countryyear==cy] )
    tmp1 = dat %>% filter(countryyear == cy) %>%
      group_by(bplcountry) %>%
      summarise(TotWF = sum(perwt) ) 
    tmp2 = tmp1 %>%  mutate(PctWF = 100* TotWF / sumperwt) %>% 
      select(-TotWF)
    colnames(tmp1)[2] = cy
    colnames(tmp2)[2] = cy
    
    TotWF =merge(TotWF,tmp1,all=TRUE)
    PctWF =merge(PctWF,tmp2,all=TRUE)
    
  }
  
  tmp = TotWF[,-1]
  tmp[is.na(tmp)] = 0
  TotWF[,-1] = tmp 
  tmp = PctWF[,-1]
  tmp[is.na(tmp)] = 0
  PctWF[,-1] = tmp 
  
  TotWF = TotWF 
  PctWF = PctWF 
  TotWF[is.na(TotWF[,1]),1] = "NA_Gilles"
  rownames(TotWF) = rownames(PctWF) = TotWF[,1]
  TotWF = TotWF[,-1]
  PctWf = PctWF[,-1]
}

#  now we need to match bplcountry and countryyear
cy_to_bplc = c(
  "Armenia", #   "ARM2011" 
  "Benin" ,#   "BEN2013" 
  "Belarus" , #   "BLR2009" 
  "Brazil" , #   "BRA2010" 
  "Botswana" , #BWA2011" 
  "Ecuador" , #   "ECU2010" 
  "Egypt" , #   "EGY2006" 
  "Fiji" , #   "FJI2007" 
  #"France", #   "FRA2006" 
  "France" ,#   "FRA2011"
  "Greece" , #   "GRC2011" 
  #"Ireland" , #   "IRL2006" 
  "Ireland" , #   "IRL2011" 
  "Iran" , #   "IRN2006" "
  "Cambodia", #   "KHM2008" 
  #"Mexico" , #   "MEX2010" 
  "Mexico" , #   "MEX2015" 
  "Mali" , #   "MLI2009" 
  "Mozambique" , #   "MOZ2007" 
  "Malawi" , #   "MWI2008"
  "Panama" , #   "PAN2010" 
  "Peru" , #   "PER2007" 
  "Philippines" , #   "PHL2010" 
  #"Puerto Rico" , #   "PRI2005" 
  "Puerto Rico" , #   "PRI2010" 
  "Portugal" , #   "PRT2011" 
  "Romania" , #   "ROU2011" 
  "El Salvador", #   "SLV2007" 
  "Uruguay" , #   "URY2006" 
  # "United States" , #   "USA2005" 
  # "United States" , #   "USA2010" 
  "United States" , #   "USA2015" 
  "South Africa" , #   "ZAF2007" 
  "Zambia" , #   "ZMB2010" 
  "Zimbabwe" #   "ZWE2012"
)

## Set "diagonal" values to 0 
colnames_TotWF_SAVE = TotWF %>% colnames()
colnames(TotWF) = cy_to_bplc

outward_mig = rep(NA,nrow(TotWF))
names(outward_mig) =  rownames(TotWF)

for(i in 1:length(outward_mig))
{
  ## check if source belongs to list  of destinations 
  if(rownames(TotWF)[i] %in% colnames(TotWF))
  {
    j = which(rownames(TotWF)[i] == colnames(TotWF))
    outward_mig[i] = sum(TotWF[i,-j])
  }else
  {
    outward_mig[i] = sum(TotWF[i,])
  }
}

outward_mig = tibble(country = names(outward_mig),
                     n=outward_mig)
outward_mig <- outward_mig %>% arrange(n)


tail_outward_mig <- outward_mig[150:200,]
tail_outward_mig$country = factor(tail_outward_mig$country,
                                  levels=tail_outward_mig$country )

ggplot(tail_outward_mig,
       aes(y=country,
           x=n/1000))+ 
  geom_col(fill = "darkgreen", width = 0.4, position = position_dodge(0.7))+ 
  theme(legend.position="none") + 
  xlab("Number of migrants (x1000)") + 
  ylab("") + 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/outward_mig.png",
         width=16,height=16,units="cm")
   
  
  



# Inward migration: sex ratio ----
source("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/R/tasks/comp_ratio.R")
res = comp_sex_ratio(dat)


res_long = res %>% 
  select(country , starts_with("pct") ) %>% 
  mutate(country = factor(country,
                          levels=rev(levels(dat$country.ord)),
                          ordered = TRUE)) %>% 
  melt(id.vars = "country")
  # mutate(variable=case_when(variable=="sex_ratio_mig"~ "migrants",
  #                           variable=="sex_ratio_nonmig"~ "non migrants")) %>% 
  # mutate(variable=factor(variable,levels=c("non migrants","migrants",ordered=TRUE)))
  

ggplot() + 
  geom_col(data = res_long, 
           aes(y=country,
               x=value, 
               fill = variable),
           width = 0.5, position = position_dodge(0.7)) + 
xlab("")+ 
  ylab("") + 
  scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pct_W.png",
         width=16,height=16,units="cm")
 


df1 = dat %>% group_by(country,mig,sex) %>% 
  summarize(n = sum(perwt)) %>% 
  mutate(N=sum(n)) %>% mutate(f=n/N) %>% 
  filter(sex=="Female")%>% select(country,mig,f)

df2 =  dat %>% group_by(country,sex) %>% 
  summarize(n = sum(perwt)) %>%  mutate(N=sum(n))%>% mutate(f=n/N)%>% 
  filter(sex=="Female") %>% mutate(mig="All") %>% select(country,mig,f)

df = bind_rows(df1,df2) %>%  mutate(country = factor(country,
                                                     levels=rev(levels(dat$country.ord)),
                                                     ordered = TRUE))
df %>% mutate(mig = case_when(mig=="mig" ~ "Migrants",
                               mig=="non mig" ~ "Non migrants",
                              mig=="All" ~ "All")) %>% 
  mutate(pct = f*100) -> df
df$mig = factor(df$mig)

ggplot() + 
  geom_col(data = df, 
           aes(y=country,
               x=pct, 
               fill = mig),
           width = 0.5, position = position_dodge(0.7)) + 
  xlab("")+ 
  ylab("")  +
#scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) + 
 # scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")) +
  theme(legend.title=element_blank(),
        legend.position=c(0.85,0.9),
        legend.text=element_text(size=12)) + 
 guides(fill = guide_legend(reverse=TRUE)) 

  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pct_W_2.png",
         width=16,height=16,units="cm")

  
           
# ratio urban -----
res = comp_urban_ratio(dat)


res_long = res %>% 
  select(country , starts_with("pct") ) %>% 
  mutate(country = factor(country,
                          levels=rev(levels(dat$country.ord)),
                          ordered = TRUE)) %>% 
  melt(id.vars = "country") 
  # mutate(variable=case_when(variable=="urban_ratio_mig"~ "migrants",
  #                           variable=="urban_ratio_nonmig"~ "non migrants")) %>% 
  # mutate(variable=factor(variable,levels=c("non migrants","migrants",ordered=TRUE)))


ggplot() + 
  geom_col(data = res_long, 
           aes(y=country,
               x=value, 
               fill = variable),
           width = 0.5, position = position_dodge(0.7)) + 
  xlab("Urban to rural ratio") + 
  ylab("") +
  scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/urban_ratio.png",
         width=16,height=16,units="cm")


df1 = dat %>% group_by(country,mig,urban) %>% 
  summarize(n = sum(perwt)) %>% 
  mutate(N=sum(n)) %>% mutate(f=n/N) %>% 
  filter(urban=="Urban")%>% select(country,mig,f)

df2 =  dat %>% group_by(country,urban) %>% 
  summarize(n = sum(perwt)) %>%  mutate(N=sum(n))%>% mutate(f=n/N)%>% 
  filter(urban=="Urban") %>% mutate(mig="All") %>% select(country,mig,f)

df = bind_rows(df1,df2) %>%  mutate(country = factor(country,
                                                     levels=rev(levels(dat$country.ord)),
                                                     ordered = TRUE))
df %>% mutate(pct = f*100) -> df

df %>% mutate(mig = case_when(mig=="mig" ~ "Migrants",
                              mig=="non mig" ~ "Non migrants",
                              mig=="All" ~ "All")) -> df
df$mig = factor(df$mig)

ggplot() + 
  geom_col(data = df, 
           aes(y=country,
               x=pct, 
               fill = mig),
           width = 0.5, position = position_dodge(0.7)) + 
  xlab("")+ 
  ylab("")  +
  #scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) + 
  # scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")) +
  theme(legend.title=element_blank(),
        legend.position=c(0.87,0.88),
        legend.text=element_text(size=10)) + 
  guides(fill = guide_legend(reverse=TRUE)) 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pct_Urb.png",
         width=16,height=16,units="cm")

###################
# age distribution
  dat %>% select(country.ord,mig,agenum) %>% group_by(country.ord,mig) %>% 
    mutate(mig = case_when(mig=="mig"~"Migrants",
                           mig=="non mig"~"Non migrants")) %>% 
    mutate(country.ord = factor(country.ord,
                                levels=rev(levels(country.ord)),
                                ordered = TRUE))-> res
  
  
  ggplot(res,aes(x=agenum,y=country.ord,fill=mig)) + 
    scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) + 
    geom_boxplot(outlier.shape = NA) + 
    xlab("Age")+ 
    ylab("") +
    theme(legend.title=element_blank(),
          legend.position=c(0.87,0.88),
          legend.text=element_text(size=10)) +
    guides(fill = guide_legend(reverse=TRUE)) 
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/boxplot_age.png",
         width=16,height=25,units="cm")
  
# age distribution -  country ordered by median age difference mig -nonmig 
  df = dat %>% select(country.ord,mig,agenum) %>% group_by(country.ord,mig) %>% 
    summarise(median_age = median(agenum,na.rm=TRUE)) %>% pivot_wider
  subs1 = seq(1,57,2)
  subs2 = seq(2,58,2)
  df = data.frame(country = df[subs1,1], med_age_diff = df[subs1,3] -df[subs2,3])
  df %<>%  arrange(median_age) 
  df %<>% mutate(country.ord = factor(country.ord,
                                      levels=country.ord,
                                      ordered = TRUE))
  
  dat %>% select(country.ord,mig,agenum) %>% group_by(country.ord,mig) %>% 
    mutate(mig = case_when(mig=="mig"~"Migrants",
                           mig=="non mig"~"Non migrants")) %>% 
    mutate(country.ord = factor(country.ord,
                                levels=rev(levels(df$country.ord)),
                                ordered = TRUE))-> res
  
  ggplot(res,aes(x=agenum,y=country.ord,fill=mig)) + 
    scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) + 
    geom_boxplot(outlier.shape = NA) + 
    xlab("Age")+ 
    ylab("") +
    theme(legend.title=element_blank(),
          legend.position=c(0.87,0.88),
          legend.text=element_text(size=10)) +
    guides(fill = guide_legend(reverse=TRUE)) 
  # ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/boxplot_age.png",
  #        width=16,height=25,units="cm")
  
################################## 
# age:  percentage of HW over 55

source("~/Dropbox/Work/WHO/SHARED/IPUMS/R/tasks/comp_ratio.R")
  
# res = comp_over55_ratio(dat)
#   
#   ggplot() + 
#     geom_col(data = res_long, 
#              aes(y=country,
#                  x=value, 
#                  fill = variable),
#              width = 0.5, position = position_dodge(0.7)) + 
#     xlab("Urban to rural ratio") + 
#     ylab("") +
#     scale_fill_manual("",values=c("darkgreen","darkolivegreen3"))   
  
  dat = dat %>% mutate(over55 = agenum>55)
  df1 = dat %>% group_by(country,mig,over55) %>% 
    summarize(n = sum(perwt)) %>% 
    mutate(N=sum(n)) %>% mutate(f=n/N) %>% 
    filter(over55)%>% select(country,mig,f)
  
  df2 =  dat %>% group_by(country,over55) %>% 
    summarize(n = sum(perwt)) %>%  mutate(N=sum(n))%>% mutate(f=n/N)%>% 
    filter(over55) %>% mutate(mig="All") %>% select(country,mig,f)
  
  df = bind_rows(df1,df2) %>%  mutate(country = factor(country,
                                                       levels=rev(levels(dat$country.ord)),
                                                       ordered = TRUE))
  df %>% mutate(mig = case_when(mig=="mig" ~ "Migrants",
                                mig=="non mig" ~ "Non migrants",
                                mig=="All" ~ "All")) %>% 
    mutate(pct = f*100) -> df
  df$mig = factor(df$mig)
 
  
  dif_pct = df$pct[seq(1,57,2)] - df$pct[seq(2,58,2)]
  names(dif_pct) = df$country[seq(1,57,2)]
  dif_pct = sort(dif_pct,decreasing = TRUE)
  df = df %>% mutate(country = factor(country,ordered = TRUE,
                                      levels=names(dif_pct)))
  
  ggplot() + 
    geom_col(data = df, 
             aes(y=country,
                 x=pct, 
                 fill = mig),
             width = 0.5, position = position_dodge(0.7)) + 
    xlab("")+ 
    ylab("")  +
    #scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) + 
    # scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07")) +
    theme(legend.title=element_blank(),
          legend.position=c(0.85,0.9),
          legend.text=element_text(size=12)) + 
    guides(fill = guide_legend(reverse=TRUE)) 
  
  ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pct_over55.png",
         width=16,height=16,units="cm")  
  
# pyramid plot for ISCO8WHO_yrstud ------

# mydf = dat %>% filter(!is.na(ISCO08WHO_yrstud)) %>% 
#   group_by(country,mig,ISCO08WHO_yrstud) %>% 
#   summarize(n = sum(perwt)) %>% 
#   mutate(N = sum(n)) %>% 
#   mutate(f=n/N) %>% select(-n,-N) 
# 
#   mydf = mydf %>% mutate(f = ifelse(mig == "mig", f *-1 , f))
#   ggplot(mydf) + 
#     geom_col(aes(fill =  interaction(mig,ISCO08WHO_yrstud, sep = "-"),
#                  y = country,
#                  x = f), 
#              width = 0.8,
#              position = "stack") + 
#     # scale_y_continuous(labels = abs,
#     #                    expand = c(0, 0)) +
#     scale_fill_manual(values = hcl(h = c(100,200,130,220,160,240), #c(15,45,110,150, 200,250),
#                                    c = 70,
#                                    l = 70,
#                                    alpha=c(0.4,0.4,1,1)),
#                       name = "")  
  

# ggplot(mydf) + 
#   geom_col(aes(fill =  interaction(mig,ISCO08WHO_yrstud, sep = "-"),
#                x = country,
#                y = f), 
#            width = 0.8,
#            position = "dodge") + 
#   scale_y_continuous(labels = abs,
#                      expand = c(0, 0)) +
#   scale_fill_manual(values = hcl(h = c(15,45,110,150, 200,250),
#                                  c = 150,
#                                  l = 60,
#                                  alpha=c(0.4,0.4,1,1)),
#                     name = "") + 
#   xlab("")+
#   coord_flip() +
#   facet_wrap(.~ mig,
#              scale = "free_x",
#              strip.position = "bottom") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         panel.spacing.x = unit(0, "pt"),
#         strip.background = element_rect(colour = "black")) + 
#   ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pyramid_yrstud.png",
#          width=16,height=20,units="cm")

  
  
  mydf = dat %>% 
    mutate(country = factor(country,
                            levels=rev(levels(dat$country.ord)),
                            ordered = TRUE)) %>% 
    filter(!is.na(ISCO08WHO_yrstud)) %>%
    filter(!(countryyear=="BLR2009" | 
               countryyear=="EGY2006" | 
               countryyear=="IRL2011" | 
               countryyear=="MOZ2007" | 
               countryyear=="MWI2008" | 
               countryyear=="PHL2010" | 
               countryyear=="SLV2007" | 
               countryyear=="URY2006" | 
               countryyear=="ZAF2007" | 
               countryyear=="ZMB2010")) %>% 
    group_by(country,mig,ISCO08WHO_yrstud) %>%
    summarize(n = sum(perwt)) %>%
    mutate(N = sum(n)) %>%
    mutate(f=n/N) %>% select(-n,-N) 
  mydf = mydf %>% mutate(mig=as.factor(mig))
  levels(mydf$mig) = c("Mig","Non Mig")
  mydf = mydf %>% mutate(f = 100*ifelse(mig == "Mig", f *-1 , f))

  ggplot(mydf) + 
      geom_col(aes(fill =  interaction(mig,ISCO08WHO_yrstud, sep = "-"),
                   x = country,
                   y = f), 
               width = 0.7,
               position = "dodge") + 
      scale_y_continuous(labels = abs,
                         expand = c(0, 0)) +
      # scale_fill_manual(values = hcl(h = c(15,45,110,150, 200,250),
      #                                c = 150,
      #                                l = 60,
      #                                alpha=c(0.4,0.4,1,1)),
      #                   name = "") + 
      xlab("")+
      coord_flip() +
      facet_wrap(.~ mig,
                 scale = "free_x",
                 strip.position = "bottom") +
      theme_minimal() +
      theme(legend.position = "bottom",
            panel.spacing.x = unit(0, "pt"),
            strip.background = element_rect(colour = "black")
            # panel.grid.minor = element_line(size = 0.5), 
            # panel.grid.major = element_line(size = 0.5)
      ) +
      theme(legend.title=element_blank()) + 
      scale_fill_manual(values = hcl(h = c(100,200,120,220,140,240), 
                                     c = 70,
                                     l = 70,
                                     alpha=c(0.4,0.4,1,1)),
                        name = "")
    
    ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/pyramid_yrstud.png",
           width=16,height=20,units="cm")
    
  
  
  
#  by sex and yrstud
res1 = dat %>% group_by(country, mig,sex,ISCO08WHO_yrstud) %>% 
  summarise(n_cmsi =  sum(perwt)) %>% 
  mutate(f_cmsi = 100 * n_cmsi / sum(n_cmsi)) 

res2 = dat %>% group_by(country, mig,sex) %>% 
  summarise(n_cms =  sum(perwt)) %>% 
  mutate(f_cms = 100 * n_cms / sum(n_cms)) 

res3 = inner_join(res2,res1)
 

ggplot(res3) + 
  geom_col(aes(y=interaction(country,mig),
               x=freq, 
               fill = interaction(sex,ISCO08WHO_yrstud, sep = "-")) , 
           width = 0.5, position = "stack") 
  xlab("")+ 
  ylab("") + 
  scale_fill_manual("",values=c("darkgreen","darkolivegreen3")) 


  # ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/sex_ratio.png",
  #        width=16,height=16,units="cm")


##
#  bplcountry x country ----
  library(pacman)
  p_load( tidyverse ,
          kableExtra ,
          magrittr )
  
  # .Rdat file loaded next line was produced by ./format.R
load('~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/AllHWF-Data_Formatted.Rdat')

# cy = "Greece"
# sumperwt = dat %>% select(perwt) %>% filter(dat$country==cy) %>% sum()
# summig = dat %>% select(perwt,bplcountry) %>% 
#   filter(dat$country==cy & bplcountry !=cy) %>% select(perwt) %>%  sum()
# df = dat %>% filter(country == cy & bplcountry !=cy) %>%
#   group_by(bplcountry) %>%
#   summarise(TotWF = sum(perwt) ) %>%
#   mutate(PctWF = 100* TotWF / summig)  
# df  = df %>% arrange(-PctWF)  


# cy = "ARM2011"
# sumperwt = sum(dat$perwt[dat$countryyear==cy] )
# TotWF = dat %>% filter(countryyear == cy) %>%
#   group_by(bplcountry) %>%
#   summarise(TotWF = sum(perwt) )
# PctWF = TotWF %>% 
#   mutate(PctWF = 100* TotWF / sumperwt) %>% 
#   select(-TotWF)
# colnames(TotWF)[2] = cy
# colnames(PctWF)[2] = cy
# 
# for(i in 2:length(sort(unique(dat$countryyear))) )
# {
#   cy = sort(unique(dat$countryyear))[i]
#   sumperwt = sum(dat$perwt[dat$countryyear==cy] )
#   tmp1 = dat %>% filter(countryyear == cy) %>%
#     group_by(bplcountry) %>%
#     summarise(TotWF = sum(perwt) ) 
#   tmp2 = tmp1 %>%  mutate(PctWF = 100* TotWF / sumperwt) %>% 
#     select(-TotWF)
#   colnames(tmp1)[2] = cy
#   colnames(tmp2)[2] = cy
#   
#   TotWF =merge(TotWF,tmp1,all=TRUE)
#   PctWF =merge(PctWF,tmp2,all=TRUE)
#   
# }
# 
# tmp = TotWF[,-1]
# tmp[is.na(tmp)] = 0
# TotWF[,-1] = tmp 
# tmp = PctWF[,-1]
# tmp[is.na(tmp)] = 0
# PctWF[,-1] = tmp 
# TotWF = TotWF %>% as_tibble()
# PctWF = PctWF %>% as_tibble()
# 
# PctWF %>%   kable(caption="All",align = "c",digits=1) %>%
#   kable_styling(full_width = F) 
# 
# TotWF %>%   kable(caption="All",align = "c",digits=1) %>%
#   kable_styling(full_width = F) 


#
# pct by origin, excluding non migrants
# aggregating some rows  
cy = "Armenia"
sumperwt = sum(dat$perwt[dat$country==cy & 
                         dat$bplcountry!=cy] )
TotWF = dat %>% filter(country == cy & 
                         dat$bplcountry!=cy) %>%
  group_by(bplcountry) %>%
  summarise(TotWF = sum(perwt) )
PctWF = TotWF %>% 
  mutate(PctWF = 100* TotWF / sumperwt) %>% 
  select(-TotWF)
colnames(TotWF)[2] = cy
colnames(PctWF)[2] = cy

for(i in 2:length(sort(unique(dat$country))) )
{
  cy = sort(unique(dat$country))[i]
  sumperwt = sum(dat$perwt[dat$country==cy& 
                             dat$bplcountry!=cy] )
  tmp1 = dat %>% filter(country == cy & 
                        bplcountry!=cy) %>%
    group_by(bplcountry) %>%
    summarise(TotWF = sum(perwt) ) 
  tmp2 = tmp1 %>%  mutate(PctWF = 100* TotWF / sumperwt) %>% 
    select(-TotWF)
  colnames(tmp1)[2] = cy
  colnames(tmp2)[2] = cy
  
  TotWF =merge(TotWF,tmp1,all=TRUE)
  PctWF =merge(PctWF,tmp2,all=TRUE)
  
}

tmp = TotWF[,-1]
tmp[is.na(tmp)] = 0
TotWF[,-1] = tmp 
tmp = PctWF[,-1]
tmp[is.na(tmp)] = 0
PctWF[,-1] = tmp 
TotWF = TotWF %>% as_tibble()
PctWF = PctWF %>% as_tibble()

PctWF %>%   kable(caption="All",align = "c",digits=1) %>%
  kable_styling(full_width = F) 

TotWF %>%   kable(caption="All",align = "c",digits=1) %>%
  kable_styling(full_width = F) 

df = PctWF %>% pivot_longer(!bplcountry,names_to = "Destination",values_to = "Pct_HW")
afr_cy = c("Egypt",
           "Mali",
           "Benin",
           "Zambia",
           "Malawi",
           "Mozambique",
           # "Zimbabwe", # lacks data
           "Botswana",
           "South Africa")
eur_cy = c("Armenia",
           "Belarus",
           "France",
           "Greece",
           "Ireland",
           "Portugal",
           "Romania")
asia_cy = c("Fiji",
            "Philippines",
            "Cambodia",
            "Iran")
am_cy = c("United States",
          "Puerto Rico",
          "Mexico",
          "El Salvador",
          "Panama",
          "Ecuador",
          "Peru",
          "Brazil",
          "Uruguay")

# % migrant
pct_mig = dat %>%   group_by(country) %>%
  summarise(TotWF = sum(perwt),
            InMigWF_comp = sum(perwt * (mig=="mig")),
            regionw.ord = first(regionw.ord)) %>%
  mutate(PctMigWF_comp = 100*InMigWF_comp/TotWF) 
pct_mig %>% rename(Destination = country) -> pct_mig
pct_mig %>% mutate(Dest_pctmig = paste(Destination," ",
                                       "(",
                                       signif(PctMigWF_comp,dig=2),
                                       "%)",sep="")) -> pct_mig


# Africa ====
tmp = df %>% filter(Destination %in% afr_cy) %>%
  filter(bplcountry != "Unknown") %>%
  filter(Pct_HW > 1 )

ggplot() +
  geom_col(data = tmp ,
           aes(y=bplcountry,x=Pct_HW,fill=bplcountry),
           width = 0.5, position = position_dodge(0.7), show.legend = FALSE) +
    facet_wrap(~Destination,scales="free")
  # guides(col = guide_legend(reverse=TRUE))
  # scale_fill_manual(values = c("deepskyblue","deepskyblue4")) +
  # labs(title="Percentage of migrants in local health work force") +
  xlab("") + ylab("") + theme(legend.title=element_blank())
  # theme(legend.position = c(.7,.1),legend.text=element_text(size=12))
# ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/tmp.png",
#        width=16,height=25,units="cm")

# Europe ====
tmp = df %>% filter(Destination %in% eur_cy) %>% 
  filter(bplcountry != "Unknown") %>% 
  filter(Pct_HW > 1 ) 

inner_join(tmp,pct_mig) -> tmp

ggplot() + 
  geom_col(data = tmp , 
           aes(y=bplcountry,x=Pct_HW,fill=bplcountry),
           width = 0.5, position = position_dodge(0.7), show.legend = FALSE) + 
  facet_wrap(~Dest_pctmig   ,scales="free",ncol=2) +
# guides(col = guide_legend(reverse=TRUE))
# scale_fill_manual(values = c("deepskyblue","deepskyblue4")) +
# labs(title="Percentage of migrants in local health work force") + 
xlab("") + ylab("") + theme(legend.title=element_blank()) 
# theme(legend.position = c(.7,.1),legend.text=element_text(size=12))
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/Inward_mig_Europe.png",
       width=16,height=25,units="cm")

# Asia ====
tmp = df %>% filter(Destination %in% asia_cy) %>%
  filter(bplcountry != "Unknown") %>%
  filter(Pct_HW > .11 )

ggplot() +
  geom_col(data = tmp ,
           aes(y=bplcountry,x=Pct_HW,fill=bplcountry),
           width = 0.5, position = position_dodge(0.7), show.legend = FALSE) +
  facet_wrap(~Destination,scales="free") +
# guides(col = guide_legend(reverse=TRUE))
# scale_fill_manual(values = c("deepskyblue","deepskyblue4")) +
# labs(title="Percentage of migrants in local health work force") +
xlab("") + ylab("") + theme(legend.title=element_blank())
# # theme(legend.position = c(.7,.1),legend.text=element_text(size=12))
# ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/tmp.png",
#        width=16,height=25,units="cm")

# Africa and Asia ====
tmp = df %>% filter(Destination %in% c(afr_cy,asia_cy)) %>% 
  filter(bplcountry != "Unknown") %>% 
  filter(Pct_HW > 1 ) 

inner_join(tmp,pct_mig) -> tmp
tmp = tmp %>% mutate(Destination = factor(Dest_pctmig,
                                          levels=c("Mali (5.5%)"    ,
                                                   "Benin (4.5%)"    , 
                                                   "Zambia (2.8%)"  ,   
                                                   "Malawi (4.1%)",
                                                   "Mozambique (2.7%)", 
                                                   "Fiji (2.9%)"      ,
                                                    "Cambodia (2.3%)"  )))
                                                  
ggplot() + 
  geom_col(data = tmp , 
           aes(y=bplcountry,x=Pct_HW,fill=bplcountry),
           width = 0.5, position = position_dodge(0.7), show.legend = FALSE) + 
  facet_wrap(~Destination,scales="free",ncol=2) +
# guides(col = guide_legend(reverse=TRUE))
# scale_fill_manual(values = c("deepskyblue","deepskyblue4")) +
# labs(title="Percentage of migrants in local health work force") + 
xlab("") + ylab("") + theme(legend.title=element_blank()) 
# theme(legend.position = c(.7,.1),legend.text=element_text(size=12))
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/Inward_mig_Af_As.png",
       width=16,height=25,units="cm")

# America ====
tmp = df %>% filter(Destination %in% am_cy) %>% 
  filter(bplcountry != "Unknown") %>% 
  filter(Pct_HW > 1 ) 
inner_join(tmp,pct_mig) -> tmp

ggplot() + 
  geom_col(data = tmp , 
           aes(y=bplcountry,x=Pct_HW,fill=bplcountry),
           width = 0.5, position = position_dodge(0.7), show.legend = FALSE) + 
  facet_wrap(~Dest_pctmig,scales="free",ncol=2) + 
# guides(col = guide_legend(reverse=TRUE))
# scale_fill_manual(values = c("deepskyblue","deepskyblue4")) +
# labs(title="Percentage of migrants in local health work force") + 
xlab("") + ylab("") + theme(legend.title=element_blank()) 
# theme(legend.position = c(.7,.1),legend.text=element_text(size=12))
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/Inward_mig_Am.png",
       width=16,height=25,units="cm")



# # radarchart =====
# df = PctWF %>% select(c(bplcountry,all_of(afr_cy)))
# subs = apply(df[,-1],1,sum)
# df = df[subs>0,]
# rn = df $bplcountry 
# df = df[,-1]
# rownames(df) = rn
# df = data.frame(t(df))
# radarchart(df)
# 
# # ggradar =====
# p_load(ggradar,scales)
# 
# df = PctWF %>% select(c(bplcountry,all_of(afr_cy)))
# cn = colnames(df)[-1]
# rn = df$bplcountry
# df = t(df[,-1])
# colnames(df) = rn
# rownames(df) = NULL
# df = as_tibble(df)
# df %>% mutate(Destination = cn) -> df
# df %>% relocate(Destination) -> df
# 
# # drop emitting countries with only 0's 
# subc = c(T,!(apply(df[,-1] == 0, 2,sum) == nrow(df)))
# df = df[,subc]
# df %>% mutate_each(funs(rescale), -Destination) ->df
# 
# ggradar(df)
# 
# # drop emitting countries with low percentages only
# subc = c(T,!(apply(df[,-1] < 5, 2,sum) == nrow(df)))
# df = df[,subc]
# df %>% mutate_each(funs(rescale), -Destination) ->df
# 
# ggradar(df)



#######################
# Back to outward migration
# 
# Analysing jointly outward migration from IPUMS data and 
# World bank country populations
load('~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/AllHWF-Data_Formatted.Rdat')

# picking bigfive pop size from nhwa:
nhwa %<>% select(organisationunitname,bigfivebirthall,bigfivetraingall)

load('~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/counpop.Rdat')
counpop$Country[counpop$Country=="India"] = "India "
counpop %<>% mutate(Country  = str_sub(Country,1,nchar(Country)-1))
# from 
# counpop = read.table("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted¨Country_pop_World_Bank.csv.txt",
#                      sep=";",header=TRUE,skip = 2)

# computing outward mig
################
## Building a table with flow from origins to destinations

{
  cy = "ARM2011"
  sumperwt = sum(dat$perwt[dat$countryyear==cy] )
  TotWF = dat %>% filter(countryyear == cy) %>%
    group_by(bplcountry) %>%
    summarise(TotWF = sum(perwt) )
  PctWF = TotWF %>% 
    mutate(PctWF = 100* TotWF / sumperwt) %>% 
    select(-TotWF)
  colnames(TotWF)[2] = cy
  colnames(PctWF)[2] = cy
  
  for(i in 2:length(sort(unique(dat$countryyear))) )
  {
    cy = sort(unique(dat$countryyear))[i]
    sumperwt = sum(dat$perwt[dat$countryyear==cy] )
    tmp1 = dat %>% filter(countryyear == cy) %>%
      group_by(bplcountry) %>%
      summarise(TotWF = sum(perwt) ) 
    tmp2 = tmp1 %>%  mutate(PctWF = 100* TotWF / sumperwt) %>% 
      select(-TotWF)
    colnames(tmp1)[2] = cy
    colnames(tmp2)[2] = cy
    
    TotWF =merge(TotWF,tmp1,all=TRUE)
    PctWF =merge(PctWF,tmp2,all=TRUE)
    
  }
  
  tmp = TotWF[,-1]
  tmp[is.na(tmp)] = 0
  TotWF[,-1] = tmp 
  tmp = PctWF[,-1]
  tmp[is.na(tmp)] = 0
  PctWF[,-1] = tmp 
  
  TotWF = TotWF 
  PctWF = PctWF 
  TotWF[is.na(TotWF[,1]),1] = "NA_Gilles"
  rownames(TotWF) = rownames(PctWF) = TotWF[,1]
  TotWF = TotWF[,-1]
  PctWf = PctWF[,-1]
}

#  now we need to match bplcountry and countryyear
cy_to_bplc = c(
  "Armenia", #   "ARM2011" 
  "Benin" ,#   "BEN2013" 
  "Belarus" , #   "BLR2009" 
  "Brazil" , #   "BRA2010" 
  "Botswana" , #BWA2011" 
  "Ecuador" , #   "ECU2010" 
  "Egypt" , #   "EGY2006" 
  "Fiji" , #   "FJI2007" 
  #"France", #   "FRA2006" 
  "France" ,#   "FRA2011"
  "Greece" , #   "GRC2011" 
  #"Ireland" , #   "IRL2006" 
  "Ireland" , #   "IRL2011" 
  "Iran" , #   "IRN2006" "
  "Cambodia", #   "KHM2008" 
  #"Mexico" , #   "MEX2010" 
  "Mexico" , #   "MEX2015" 
  "Mali" , #   "MLI2009" 
  "Mozambique" , #   "MOZ2007" 
  "Malawi" , #   "MWI2008"
  "Panama" , #   "PAN2010" 
  "Peru" , #   "PER2007" 
  "Philippines" , #   "PHL2010" 
  #"Puerto Rico" , #   "PRI2005" 
  "Puerto Rico" , #   "PRI2010" 
  "Portugal" , #   "PRT2011" 
  "Romania" , #   "ROU2011" 
  "El Salvador", #   "SLV2007" 
  "Uruguay" , #   "URY2006" 
  # "United States" , #   "USA2005" 
  # "United States" , #   "USA2010" 
  "United States" , #   "USA2015" 
  "South Africa" , #   "ZAF2007" 
  "Zambia" , #   "ZMB2010" 
  "Zimbabwe" #   "ZWE2012"
)

## Set "diagonal" values to 0 
colnames_TotWF_SAVE = TotWF %>% colnames()
colnames(TotWF) = cy_to_bplc

outward_mig = rep(NA,nrow(TotWF))
names(outward_mig) =  rownames(TotWF)

for(i in 1:length(outward_mig))
{
  ## check if source belongs to list  of destinations 
  if(rownames(TotWF)[i] %in% colnames(TotWF))
  {
    j = which(rownames(TotWF)[i] == colnames(TotWF))
    outward_mig[i] = sum(TotWF[i,-j])
  }else
  {
    outward_mig[i] = sum(TotWF[i,])
  }
}

outward_mig = tibble(country = names(outward_mig),
                     n=outward_mig)
outward_mig <- outward_mig %>% arrange(n)


tail_outward_mig <- outward_mig[150:200,]
# tail_outward_mig$country = factor(tail_outward_mig$country,
#                                   levels=tail_outward_mig$country )

subs = !(tail_outward_mig$country %in% counpop$Country)
tail_outward_mig$country[subs]

# subs = !(outward_mig$country %in% counpop$country)
# outward_mig$country[subs]

tail_outward_mig = tail_outward_mig %>% mutate( country =
                                                  case_when(#country== "Hong Kong"
                                                    #country=="United States" ~  "United States of America" ,
                                                    #country== "Taiwan"
                                                    country=="Guyana/British Guiana" ~  "Guyana",
                                                    #country== "Iran" ~   "Iran (Islamic Republic of)",
                                                    #country== "European Union"
                                                    country== "Russia/USSR" ~    "Russia",
                                                    #country=="Other countries, not specified"
                                                    #country=="United Kingdom" ~    "United Kingdom of Great Britain and Northern Irela",
                                                    #country== "Puerto Rico"
                                                    #country== "Vietnam" ~   "Viet Nam"  ,
                                                    country== "Korea"   ~     "South Korea",
                                                    #country== "Africa, other and n.s."
                                                    TRUE  ~ country
                                                  ))



# outward_mig = outward_mig %>% mutate( country =
#                                        case_when(#country== "Hong Kong"                     
#                                          country=="United States" ~  "United States of America" ,               
#                                          #country== "Taiwan"                        
#                                          country=="Guyana/British Guiana" ~  "Guyana",       
#                                          country== "Iran" ~   "Iran (Islamic Republic of)",              
#                                          #country== "European Union"                
#                                          country== "Russia/USSR" ~    "Russian Federation",               
#                                          #country=="Other countries, not specified"
#                                          country=="United Kingdom" ~    "United Kingdom of Great Britain and Northern Irela",        
#                                          #country== "Puerto Rico"                   
#                                          country== "Vietnam" ~   "Viet Nam"  ,               
#                                          country== "Korea"   ~     "Republic of Korea",                 
#                                          #country== "Africa, other and n.s."
#                                          TRUE  ~ country
#                                        ))




# matching ipums and nhwa country names
counpop %<>% rename(country = Country)
df = full_join(counpop,tail_outward_mig)
df
df %<>% arrange(-n)
df
df %<>% mutate(pct = 100*n /Population) 
df %<>% arrange(-pct)
df

# df %<>%  pivot_longer(-country)
# df

df %<>% arrange(-n)
df = df[1:51,] 
df %<>% arrange(n)
df %<>% mutate(country=factor(country,levels=country,ordered=TRUE))


ggplot(df,
       aes(y=country,
           x=n/1000))+ 
  geom_col(fill = "olivedrab4", width = 0.4, position = position_dodge(0.7))+ 
  theme(legend.position="none") + 
  xlab("Number of people (x1000)") + 
  ylab("") 
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/outward_mig_count.png",
       width=16,height=16,units="cm")


df %<>% arrange(pct)
df = df[1:48,]
df %<>% mutate(country=factor(country,levels=country,ordered=TRUE))

ggplot(df,
       aes(y=country,
           x=pct))+ 
  geom_col(fill = "seagreen4", width = 0.4, position = position_dodge(0.7))+ 
  theme(legend.position="none") + 
  xlab("Percentage of source country population") + 
  ylab("") 
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/outward_mig_pct.png",
       width=16,height=16,units="cm")

##############################
##
## table and figure on migration variables

df = dat %>% group_by(country) %>% 
  summarise(bplcountry = 100*mean(is.na(bplcountry)),
            na.nation = 100*mean(is.na(nation)),
            na.nativity = 100*mean(is.na(nativity)),
            na.citizen = 100*mean(is.na(citizen))) 

colnames(df) = c("Country",
                 "Country of birth",
                 "Country of citizenship",
                 "Nativity status",
                 "Citizenship")

save(df,file="/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/dat/desc_mig.Rdat")


##  composite
res0 = dat %>%   group_by(countryyear) %>%
  summarise(TotWF = sum(perwt),
            InMigWF_comp = sum(perwt * (mig=="mig"))) %>%
  mutate(PctMigWF_comp = 100*InMigWF_comp/TotWF) 

## country
res1 = dat %>%   group_by(countryyear,bplcountry) %>%
  summarise(WF = sum(perwt),country=first(country) ) %>%
  mutate(TotWF = sum(WF)) %>%
  mutate(PctWF = 100*WF/TotWF) %>%
  mutate(InMigWF = case_when(country == bplcountry ~ 0,
                             country != bplcountry ~ WF)) %>%
  summarise(TotWF=first(TotWF) , TotInMigWF_bplcountry = sum(InMigWF))  %>%
  mutate(PctInMigWF_bplcountry = 100*TotInMigWF_bplcountry / TotWF) 

## nation
res2 = dat %>%   group_by(countryyear,nation) %>%
  summarise(WF = sum(perwt),country=first(country) ) %>%
  mutate(TotWF = sum(WF)) %>%
  mutate(PctWF = 100*WF/TotWF) %>%
  mutate(InMigWF_nation = case_when(country == nation ~ 0,
                                    country != nation ~ WF)) %>%
  summarise(TotWF=first(TotWF) , TotInMigWF_nation = sum(InMigWF_nation)) %>%
  mutate(PctInMigWF_nation = 100*TotInMigWF_nation / TotWF)

## nativity
res3 = dat %>%   group_by(countryyear,nativity) %>%
  summarise(WF = sum(perwt),country=first(country) ) %>%
  mutate(TotWF = sum(WF)) %>%
  mutate(PctWF = 100*WF/TotWF) %>%
  mutate(InMigWF_nativity = case_when(nativity != "Foreign-born" ~ 0,
                                      nativity == "Foreign-born" ~ WF)) %>%
  summarise(TotWF=first(TotWF) , TotInMigWF_nativity = sum(InMigWF_nativity)) %>%
  mutate(PctInMigWF_nativity = 100*TotInMigWF_nativity / TotWF)

## citizen
res4 = dat %>%   group_by(countryyear,citizen) %>%
  summarise(WF = sum(perwt),country=first(country) ) %>%
  mutate(TotWF = sum(WF)) %>%
  mutate(PctWF = 100*WF/TotWF) %>%
  mutate(InMigWF_citizen = case_when(!(citizen == "Not a citizen" 
                                       | citizen == "Naturalized citizen") ~ 0,
                                     citizen == "Not a citizen" 
                                     | citizen == "Naturalized citizen" ~ WF)) %>%
  summarise(TotWF=first(TotWF) , TotInMigWF_citizen = sum(InMigWF_citizen)) %>%
  mutate(PctInMigWF_citizen = 100*TotInMigWF_citizen / TotWF)

# res = res1 %>%  full_join(res2,by=c("countryyear","TotWF")) %>% 
#  full_join(res3, by=c("countryyear","TotWF")) %>% full_join(res4, by=c("countryyear","TotWF"))
res <- bind_cols(res0, 
                 select(res1,-c("countryyear","TotWF")),
                 select(res2,-c("countryyear","TotWF")),
                 select(res3,-c("countryyear","TotWF")),
                 select(res4,-c("countryyear","TotWF")))
res <- as_tibble(res)
res <- res %>% mutate(year=as.numeric(str_extract(countryyear, "[0-9]+")))

res_Pct <- res %>% select(starts_with("Pct"))  
colnames(res_Pct) = c("Composite",
                      "Country of birth",
                      "Country of citizenship",
                      "Nativity status",
                      "Citizenship")
p_load(GGally)

ggpairs(res_Pct) + theme_grey(base_size = 8) 
ggsave("/home/gilles/Dropbox/Work/WHO/SHARED/IPUMS/paper/fig/cor_pctmig.png",
         width=16,height=16,units="cm")



  
  
  
  
  
  
  










