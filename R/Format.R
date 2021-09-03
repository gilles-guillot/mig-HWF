if(!require('pacman')) install.packages('pacman')
library(pacman)
p_load(tidyverse,
       kableExtra,
       knitr,
       readxl,
       stringr,
       magrittr)


## load raw data (from MB)  pre exported to Rdat format
# dat = read_csv(file = '../MB_2021-05-10/OneDrive_1_5-11-2021/AllHWF-Data.csv.gz',
#                guess_max = 4e+6# more than nb of rows
#                )
# save(dat,file = '../MB_2021-05-10/OneDrive_1_5-11-2021/AllHWF-Data.Rdat')



# Loading external main data file ---------------------------------------------
load("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/IPUMS_MB/MB_2021-05-10/OneDrive_1_5-11-2021/AllHWF-Data.Rdat")


# loading and fomatting ISCO8WHO job codes -------------------------------------
dat$ISCO08WHO[dat$ISCO08WHO == 9999] = NA


# Files ISCO08WHO.csv : 
# selection of jobs to be considered as Health Workers ; 
# created by GG in May 2021 by copy pasting NHWA handbook appendix 4
# https://www.who.int/hrh/documents/brief_nhwa_handbook/en/
#   http://apps.who.int/iris/bitstream/handle/10665/259360/9789241513111-eng.pdf;jsessionid=7C474B4323763903AF919985C41BD7A8?sequence=


isco_names = read_delim("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/misc_data/ISCO08WHO.csv",
                        delim=",",col_names = TRUE) %>% 
  dplyr::select(-c(2:4,8:9)) %>%
  unite("All", 1:3, na.rm = TRUE, remove = FALSE) %>% 
  mutate(All=as.numeric(All))
colnames(isco_names)[5] = "names"


## in dat ISCO8WHO variable 
## splittiing num code and additional char code into two columns
ISCO08WHO_num <- as.numeric(str_extract(dat$ISCO08WHO, "[0-9]+"))
ISCO08WHO_cha <- (str_extract(dat$ISCO08WHO, "[aA-zZ]+"))
dat = dat %>% mutate(ISCO08WHO_num=ISCO08WHO_num , 
                     ISCO08WHO_cha=ISCO08WHO_cha)

## matching job codes in ISCO08WHO and job names in isco_names
subs = match(dat$ISCO08WHO_num,isco_names$All)
ISCO08WHO_names = isco_names$names[subs]
dat$ISCO08WHO_names = ISCO08WHO_names

## Could we assign a job name to each single job code?
a = dat$ISCO08WHO_num %>% unique() %>% sort() 
b = isco_names$All %>% unique() %>% sort() 
a[! a %in% b] # NO, some are missing
    # (those are not Health workers in the sense of the present study)

## finishing by hand for those codes appearing in data file 
## but not documented in ../../../ISCO08WHO.csv
## info for conversion of job code to job name 
##  https://www.ilo.org/public/english/bureau/stat/isco/docs/groupdefn08.pdf


ISCO08WHO_names[dat$ISCO08WHO_num == 2131] = 
  "Biologists, Botanists, Zoologists and Related Professionals"

ISCO08WHO_names[dat$ISCO08WHO_num == 2149] = 
  "Engineering Professionals Not Elsewhere Classified"

ISCO08WHO_names[dat$ISCO08WHO_num == 3412] = 
  "Social Work Associate Professionals"

ISCO08WHO_names[dat$ISCO08WHO_num == 4411] = 
  "Library Clerk"

ISCO08WHO_names[dat$ISCO08WHO_num == 9112] = 
"Cleaners and Helpers in Offices, Hotels and other Establishments"


ll = c("Medical doctors" ,   
       "Generalist medical practitioners",    
       "Specialist medical practitioners" ,      
       "Medical assistants"     ,   
       "Nursing associate professionals",
       "Nursing professionals"        ,
       "Midwifery associate professionals"   ,
       "Midwifery professionals"     ,
       "Nursing and midwifery professionals"  ,   
       "Medical secretaries"  ,
       "Dental assistants and therapists"   ,
       "Dentists"    ,
       "Audiologists and speech therapists"    , 
       "Optometrists and ophthalmic opticians"   ,     
       "Dispensing opticians"     ,    
       "Medical and pathology laboratory technicians"   ,          
       "Medical and pharmaceutical technicians"    ,     
       "Medical imaging and therapeutic equipment technicians"     ,     
       "Pharmaceutical technicians and assistants" ,    
       "Pharmacists"    ,
       "Psychologists"   , 
       "Physiotherapy technicians and assistants"   ,
       "Physiotherapists"       ,      
       "Dieticians and nutritionists"   ,
       "Ambulance workers"    ,
       "Paramedical practitioners"    ,
       "Health associate professionals"    ,  
       "Health care assistants"     ,      
       "Social work and counselling professionals"  ,    
       "Community health workers"    , 
       "Home-based personal care workers"    ,
       "Personal care workers in health services"   , 
       "Personal care workers in health services not elsewhere classified",
       "Environmental and occupational health inspectors and associates"  ,
       "Environmental and occupational health and hygiene professionals",
       "Medical records and health information technicians"  ,
       "Traditional and complementary medicine associate professionals"   ,
       "Traditional and complementary medicine professionals"    ,
       "Health associate professionals not elsewhere classified"    ,         
       "Health professionals not elsewhere classified"     ,  
       "Additional health-related unit groups"   )
dat = dat %>% mutate(ISCO08WHO.ord = factor(ISCO08WHO_names,
                                            levels = ll,                
                                            ordered = TRUE))

# Aggregated job categories ------------------
dat = dat %>% mutate(ISCO08WHO_agg = case_when(ISCO08WHO_names == "Medical doctors" ~ "Medical doctors"  ,
                                               ISCO08WHO_names == "Generalist medical practitioners"~ "Medical doctors"  ,
                                               ISCO08WHO_names == "Specialist medical practitioners" ~ "Medical doctors"  ,
                                               ISCO08WHO_names == "Medical assistants"     ~  "Others" ,
                                               ISCO08WHO_names == "Nursing associate professionals" ~ "Nursing and midwifery professionals",
                                               ISCO08WHO_names == "Nursing professionals" ~ "Nursing and midwifery professionals"       ,
                                               ISCO08WHO_names == "Midwifery associate professionals" ~ "Nursing and midwifery professionals"  ,
                                               ISCO08WHO_names == "Midwifery professionals"  ~ "Nursing and midwifery professionals"   ,
                                               ISCO08WHO_names == "Nursing and midwifery professionals"  ~ "Nursing and midwifery professionals" ,
                                               ISCO08WHO_names == "Medical secretaries" ~ "Others" ,
                                               ISCO08WHO_names == "Dental assistants and therapists" ~   "Dentists & assistants",
                                               ISCO08WHO_names == "Dentists"   ~  "Dentists & assistants",
                                               ISCO08WHO_names == "Audiologists and speech therapists"    ~ "Others" ,
                                               ISCO08WHO_names == "Optometrists and ophthalmic opticians"   ~  "Others",
                                               ISCO08WHO_names == "Dispensing opticians"     ~ "Others" ,
                                               ISCO08WHO_names == "Medical and pathology laboratory technicians"   ~  "Others",
                                               ISCO08WHO_names == "Medical and pharmaceutical technicians"    ~ "Others" ,
                                               ISCO08WHO_names == "Medical imaging and therapeutic equipment technicians"     ~ "Others" ,
                                               ISCO08WHO_names == "Pharmaceutical technicians and assistants" ~  "Pharmacists & assistants",
                                               ISCO08WHO_names == "Pharmacists"  ~ "Pharmacists & assistants"  ,
                                               ISCO08WHO_names == "Psychologists"   ~  "Others",
                                               ISCO08WHO_names == "Physiotherapy technicians and assistants"  ~"Others" ,
                                               ISCO08WHO_names == "Physiotherapists"       ~  "Others",
                                               ISCO08WHO_names == "Dieticians and nutritionists"  ~ "Others" ,
                                               ISCO08WHO_names == "Ambulance workers"  ~  "Others",
                                               ISCO08WHO_names == "Paramedical practitioners"  ~  "Others",
                                               ISCO08WHO_names == "Health associate professionals"    ~ "Others" ,
                                               ISCO08WHO_names == "Health care assistants"     ~ "Others" ,
                                               ISCO08WHO_names == "Social work and counselling professionals"  ~ "Others" ,
                                               ISCO08WHO_names == "Community health workers"    ~ "Others" ,
                                               ISCO08WHO_names == "Home-based personal care workers"  ~ "Personal care workers" ,
                                               ISCO08WHO_names == "Personal care workers in health services"   ~ "Personal care workers" ,
                                               ISCO08WHO_names == "Personal care workers in health services not elsewhere classified" ~ "Personal care workers",
                                               ISCO08WHO_names == "Environmental and occupational health inspectors and associates" ~ "Others",
                                               ISCO08WHO_names == "Environmental and occupational health and hygiene professionals" ~ "Others",
                                               ISCO08WHO_names == "Medical records and health information technicians"  ~ "Others",
                                               ISCO08WHO_names == "Traditional and complementary medicine associate professionals" ~ "Others" ,
                                               ISCO08WHO_names == "Traditional and complementary medicine professionals" ~   "Others",
                                               ISCO08WHO_names == "Health associate professionals not elsewhere classified"    ~ "Others" ,
                                               ISCO08WHO_names == "Health professionals not elsewhere classified"     ~ "Others" ,
                                               ISCO08WHO_names == "Additional health-related unit groups" ~ "Others"))
dat = dat %>% mutate(ISCO08WHO_agg = factor(ISCO08WHO_agg,
                                            levels = c("Personal care workers"  ,
                                                         "Nursing and midwifery professionals" ,
                                                         "Pharmacists & assistants"   ,
                                                        "Medical doctors"  ,
                                                       "Dentists & assistants"  ,
                                                       "Others"   ),
                                            ordered = TRUE))

# Job categories aggregated by duration of study =======
dat = dat %>% mutate(ISCO08WHO_yrstud = 
                       case_when(ISCO08WHO_names == "Medical doctors" ~ "Long"  ,
                                 ISCO08WHO_names == "Generalist medical practitioners"~ "Long"  ,
                                 ISCO08WHO_names == "Specialist medical practitioners" ~ "Long"  ,
                                 ISCO08WHO_names == "Medical assistants"     ~  "Medium" ,
                                 ISCO08WHO_names == "Nursing associate professionals" ~ "shoty",
                                 ISCO08WHO_names == "Nursing professionals" ~ "Medium"       ,
                                 ISCO08WHO_names == "Midwifery associate professionals" ~ "short"  ,
                                 ISCO08WHO_names == "Midwifery professionals"  ~ "Medium"   ,
                                 ISCO08WHO_names == "Nursing and midwifery professionals"  ~ "Medium" ,
                                 ISCO08WHO_names == "Medical secretaries" ~ "Medium" ,
                                 ISCO08WHO_names == "Dental assistants and therapists" ~   "Long",
                                 ISCO08WHO_names == "Dentists"   ~  "Long",
                                 ISCO08WHO_names == "Audiologists and speech therapists"    ~ "Medium" ,
                                 ISCO08WHO_names == "Optometrists and ophthalmic opticians"   ~  "Medium",
                                 ISCO08WHO_names == "Dispensing opticians"     ~ "Medium" ,
                                 ISCO08WHO_names == "Medical and pathology laboratory technicians"   ~  "Medium",
                                 ISCO08WHO_names == "Medical and pharmaceutical technicians"    ~ "Medium" ,
                                 ISCO08WHO_names == "Medical imaging and therapeutic equipment technicians"     ~ "Medium" ,
                                 ISCO08WHO_names == "Pharmaceutical technicians and assistants" ~  "Medium",
                                 ISCO08WHO_names == "Pharmacists"  ~ "Long"  ,
                                 ISCO08WHO_names == "Psychologists"   ~  "Medium",
                                 ISCO08WHO_names == "Physiotherapy technicians and assistants"  ~ "Medium" ,
                                 ISCO08WHO_names == "Physiotherapists"       ~  "Medium",
                                 ISCO08WHO_names == "Dieticians and nutritionists"  ~ "Medium" ,
                                 ISCO08WHO_names == "Ambulance workers"  ~  "Short",
                                 ISCO08WHO_names == "Paramedical practitioners"  ~  "Short",
                                 ISCO08WHO_names == "Health associate professionals"    ~ "Short" ,
                                 ISCO08WHO_names == "Health care assistants"     ~ "Short" ,
                                 ISCO08WHO_names == "Social work and counselling professionals"  ~ "Medium" ,
                                 ISCO08WHO_names == "Community health workers"    ~ "Short" ,
                                 ISCO08WHO_names == "Home-based personal care workers"  ~ "Short" ,
                                 ISCO08WHO_names == "Personal care workers in health services"   ~ "Short" ,
                                 ISCO08WHO_names == "Personal care workers in health services not elsewhere classified" ~ "Short",
                                 ISCO08WHO_names == "Environmental and occupational health inspectors and associates" ~ "Medium",
                                 ISCO08WHO_names == "Environmental and occupational health and hygiene professionals" ~ "Medium",
                                 ISCO08WHO_names == "Medical records and health information technicians"  ~ "Medium",
                                 ISCO08WHO_names == "Traditional and complementary medicine associate professionals" ~ "Short" ,
                                 ISCO08WHO_names == "Traditional and complementary medicine professionals" ~   "Medium",
                                 ISCO08WHO_names == "Health associate professionals not elsewhere classified"    ~ "Short" , 
                                 ISCO08WHO_names == "Health professionals not elsewhere classified"     ~ "Medium" ,
                                 ISCO08WHO_names == "Additional health-related unit groups" ~ NA_character_))
dat = dat %>% mutate(ISCO08WHO_yrstud = factor(ISCO08WHO_yrstud,
                                               levels = c("Short"  ,
                                                          "Medium" ,
                                                          "Long" ),
                                               ordered = FALSE))

dat = dat %>% mutate(ISCO08WHO_yrstud_ord = factor(ISCO08WHO_yrstud,
                                               levels = c("Short"  ,
                                                          "Medium" ,
                                                          "Long" ),
                                               ordered = TRUE))





# Variables involving country names ----------------------------------------------
## A modest attempt at sorting the mess of country names:
dat$nation[dat$nation == "Cambodia (Kampuchea)" ] <-"Cambodia"
dat$bplcountry[dat$bplcountry == "Cambodia (Kampuchea)" ] <- "Cambodia"
dat$nation[dat$nation == "Egypt/United Arab Rep."] <- "Egypt" 
dat$country[dat$country == "Cote D'Ivoire"] <- "Ivory Coast"


## dropping all rows with countryyear lacking info on migration
## none of bplcountry	nation	PctInMigWF_nation	nativity	citizen	contains data
dat <- dat %>% filter(countryyear != "CIV2014" &
                        countryyear != "FRA2014" &
                        countryyear != "IND2004" &
                        countryyear != "IND2009" &
                        countryyear != "IRN2011" &
                        countryyear != "KHM2013" &
                        countryyear != "MUS2011" &
                        countryyear != "NGA2008" &
                        countryyear != "NGA2009" &
                        countryyear != "NGA2010" &
                        countryyear != "SEN2013" &
                        countryyear != "VNM2009" )

# dropping all rows where year in countryyear 
# is not the latest year with data on migration (bplcountry) 
dat <- dat %>% filter(countryyear != "FRA2006" &
                        countryyear != "IRL2006" &
                        countryyear != "MEX2010" &
                        countryyear != "PRI2005" &
                        countryyear != "USA2005" &
                        countryyear != "USA2010"  )



# Age ------
dat = dat %>% mutate(age2.ord = factor(age2,
                                       levels = c("0 to 4" , "5 to 9",   
                                                  "10 to 14", "15 to 19", "20 to 24",
                                                  "25 to 29", "30 to 34" ,"35 to 39", 
                                                  "40 to 44", "45 to 49" ,
                                                  "50 to 54" ,"55 to 59" ,
                                                  "60 to 64",  "65 to 69" ,
                                                  "70 to 74", "75 to 79",  "80+" ,    
                                                  "Unknown" ),
                                       ordered = TRUE))


                     


# edattain --------
dat = dat %>% mutate(edattain.ord = factor(edattain,
                                           levels = c("Less than primary completed",
                                                      "Primary completed",
                                                      "Secondary completed" ,
                                                      "University completed" ,
                                                      "NIU (not in universe)",
                                                      "Unknown"  ),
                                           ordered = TRUE))
dat = dat %>% mutate(edattaind.ord = factor(edattaind,
                                            levels = c(
                                              "No schooling" ,   
                                              "Some primary completed", 
                                              "Less than primary completed (n.s.)",   
                                              "Primary (4 yrs) completed",
                                              "Primary (5 yrs) completed",
                                              "Primary (6 yrs) completed",    
                                              "Lower secondary technical completed",  
                                              "Lower secondary general completed", 
                                              "Secondary, technical track completed", 
                                              "Secondary, general track completed",   
                                              "Post-secondary technical education",   
                                              "Some college completed",
                                              "University completed" ,                                                              
                                              "NIU (not in universe)",
                                              "Unknown/missing"   ),                
                                            ordered = TRUE))




# empstat ----------
ll = c("At work" ,  
       "Employed, not specified" ,                 
       "Housework",  
       "At work, family holding, agricultural",             
       "At work, family holding, not specified",                
       "Marginally employed",                               
       "Not working, seasonal worker",                           
       "Intermittent worker",     
       "Have job, not at work in reference period",             
       "Armed forces, at work",     
       "In school",                    
       "Unemployed 6 or more months",   
       "Unemployed, not specified" ,
       "Retirees and living on rent" ,                                        
       "Prisoner",                                  
       "Unable to work, disabled or health reasons", 
       "Unemployed, experienced worker"  ,           
       "Retired"    ,     
       "Inactive, other reasons",    
       "No work available",                        
       "Inactive (not in labor force)",  
       "NIU (not in universe)")
dat = dat %>% mutate(empstat.ord = factor(empstatd,
                                          levels = ll,                
                                          ordered = TRUE))


# empsect ------------------
ll = c(
  "Individual/family enterprise, and self-employed" ,
  "Private, not elsewhere classified",     
  "Mixed: public-private or parastatal" ,
  "Public"  ,                                           
  "Collective or cooperative" ,      
  "Foreign government or non-governmental organization" ,
  "Foreign"   ,     
  "Other, unspecified"   ,     
  "NIU (not in universe)",
  "Unknown" )                            
dat = dat %>% mutate(empsect.ord = factor(empsect,
                                          levels = ll,                
                                          ordered = TRUE))


# hrsmain   ------------------------
dat = dat %>%  
  mutate(hrsmain.num = replace(hrsmain, 
                               hrsmain=="Unknown", NA)) %>% 
  mutate(hrsmain.num = str_extract( hrsmain.num,"[0-9]+")) %>% 
  mutate(hrsmain.num = as.numeric(hrsmain.num))



# incearn -----------------------
dat = dat  %>% 
  mutate(incearn.clean = replace(incearn,
                                 incearn < 0  | incearn == 99999998 ,
                                 NA))

# composite mig ---------------------
## Creating mig variables
# NB: because F | NA is F
# T | NA is T 
# F | NA is NA
# defining mig as 
# mig =  (country != bplcountry) |
#   (country != nation) |
#   (nativity == "Foreign-born") |
#   ( citizen == "Not a citizen" |
#       citizen == "Naturalized citizen" ))
# would preoduce a variable with a lot of NAs

dat = dat %>% mutate(mig_bplcountry = country != bplcountry ,
                     mig_nation = country != nation ,
                     mig_fgnborn = nativity == "Foreign-born",
                     mig_citizen = ( citizen == "Not a citizen" |
                                       citizen == "Naturalized citizen" ))

## A synthetic mig variable
## twisted logical OR
## T,NA -> T
## F,NA -> F
`%altOR%` = function(x,y)
{
  case_when( (!is.na(x) & !is.na(y) ) | (is.na(x) & is.na(y) ) ~ x|y,
             is.na(x) ~ y,
             is.na(y) ~ x)
}
dat = dat %>% 
  mutate(mig =  mig_bplcountry  %altOR% mig_nation 
         %altOR% mig_fgnborn  %altOR% mig_citizen) 
dat$mig = c("non mig","mig") [dat$mig+1]



# bplregionw -------------------
## creating a variable for birthplace at a coarser scale: bplregionw
dat = dat %>% mutate(bplregionw = case_when(
  bplcountry == "bplregionwMalawi"  ~ "Eastern Africa" ,
  bplcountry == "Zambia"   ~ "Southern Africa", 
  bplcountry == "Tanzania" ~  "Eastern Africa",                        
  bplcountry == "Zimbabwe" ~ "Southern Africa" ,                          
  bplcountry == "Mozambique"  ~   "Southern Africa" ,                    
  bplcountry == "South Africa"   ~   "Southern Africa" ,                      
  bplcountry == "Congo" ~      "Central Africa",                     
  bplcountry == "Kenya"   ~ "Eastern Africa" ,                            
  bplcountry == "Other countries, not specified" ~ NA_character_,     
  bplcountry == "Democratic Republic of Congo"   ~      "Central Africa",   
  bplcountry == "Burundi"    ~  "Eastern Africa"    ,                          
  bplcountry == "Hong Kong"    ~     "Eastern Asia" ,                
  bplcountry == "Afghanistan"       ~  "Eastern Asia",                 
  bplcountry == "Albania"              ~  "Southern Europe"  ,          
  bplcountry == "United States"  ~    "North America",                
  bplcountry == "Guam"                 ~   "South-Eastern Asia",            
  bplcountry == "United Kingdom"       ~      "Western Europe",        
  bplcountry == "India"    ~ "Southern Asia"   ,                       
  bplcountry == "Nigeria"             ~    "Western Africa" ,          
  bplcountry == "Mali"    ~    "Western Africa" ,    
  bplcountry == "Ivory Coast"    ~    "Western Africa" ,                       
  bplcountry == "Burkina Faso"     ~    "Western Africa" ,                     
  bplcountry == "Gabon"        ~"Central Africa"     ,                 
  bplcountry == "Guinea"  ~    "Western Africa" ,                                
  bplcountry == "Africa, other and n.s."  ~"Africa",           
  bplcountry == "Americas, other and n.s."  ~"America",         
  bplcountry == "Guadeloupe"    ~      "Caribbean"   ,            
  bplcountry == "Senegal"     ~   "Western Africa",                    
  bplcountry == "Cameroon"      ~       "Central Africa",              
  bplcountry == "Niger"   ~   "Western Africa",                               
  bplcountry == "Angola"  ~    "Southern Africa",      
  bplcountry == "France"      ~          "Western Europe",             
  bplcountry == "United Arab Emirates"     ~       "Western Asia",   
  bplcountry == "China"              ~     "Eastern Asia",           
  bplcountry == "Asia, other and n.s."      ~"Asia",         
  bplcountry == "Andorra"     ~         "Western Europe",              
  bplcountry == "Europe, other and n.s."     ~"Europe",         
  bplcountry == "Ghana"                  ~     "Western Africa",       
  bplcountry == "Sudan"              ~ "Eastern Africa",               
  bplcountry == "Algeria"          ~ "Northern Africa" ,                   
  bplcountry == "Sierra Leone"        ~ "Western Africa" ,                
  bplcountry == "Unknown"         ~ NA_character_,           
  bplcountry == "Togo"        ~ "Western Africa" ,                                
  bplcountry == "Russia/USSR"     ~ "Russia" ,                    
  bplcountry == "Cuba"         ~ "Caribbean" ,                      
  bplcountry == "Gambia"           ~ "Western Africa" ,                   
  bplcountry == "Ethiopia"       ~ "Eastern Africa" ,                     
  bplcountry == "South America, other and n.s."  ~ "South America" ,     
  bplcountry == "Central America, n.s."            ~ "Central America" ,  
  bplcountry == "Seychelles"     ~ "Eastern Africa" ,                     
  bplcountry == "Cape Verde"                  ~ "Western Africa" ,        
  bplcountry == "Portugal"                 ~ "Southern Europe" ,           
  bplcountry == "Italy"                         ~ "Southern Europe" ,      
  bplcountry == "Namibia"  ~ "Southern Africa" ,                           
  bplcountry == "Somalia"            ~ "Eastern Africa" ,                 
  bplcountry == "North America, other and n.s."   ~ "North America" ,    
  bplcountry == "Germany"        ~ "Northern Europe" ,                     
  bplcountry == "Brazil"~ "South America" , 
  bplcountry == "Pakistan"         ~ "Southern Asia" ,                   
  bplcountry == "Sao Tome and Principe"    ~ "Central Africa" ,          
  bplcountry == "Equatorial Guinea"         ~ "Central Africa" ,         
  bplcountry == "Fiji"          ~ "South-Eastern Asia" ,                      
  bplcountry == "Uganda"        ~ "Eastern Africa" ,                      
  bplcountry == "Tunisia"      ~ "Northern Africa" ,                       
  bplcountry == "Iraq"              ~ "Western Asia"  ,                
  bplcountry == "Myanmar (Burma)"        ~ "South-Eastern Asia" ,             
  bplcountry == "Bolivia"       ~ "South America" ,                      
  bplcountry == "Peru"             ~ "South America" ,                   
  bplcountry == "Costa Rica"      ~ "Central America" ,                   
  bplcountry == "Uruguay"        ~ "South America" ,                     
  bplcountry == "Suriname"       ~ "South America" ,                     
  bplcountry == "Belgium"           ~ "Northern Europe" ,                  
  bplcountry == "Colombia"                   ~ "South America" ,             
  bplcountry == "Venezuela"                  ~ "South America" ,             
  bplcountry == "Japan"                       ~ "Eastern Asia" ,        
  bplcountry == "Guinea-Bissau"              ~ "Western Africa" ,         
  bplcountry == "Switzerland"            ~ "Western Europe" ,           
  bplcountry == "Panama"                   ~ "Central America" ,          
  bplcountry == "Chile"   ~ "South America" ,                            
  bplcountry == "Argentina"   ~ "South America" ,                        
  bplcountry == "Paraguay"   ~ "South America" ,                         
  bplcountry == "Ecuador"           ~ "South America" ,                  
  bplcountry == "Lebanon"          ~ "Western Asia" ,                  
  bplcountry == "Spain"            ~ "Southern Europe" ,                   
  bplcountry == "Jordan"                 ~ "Western Asia" ,                
  bplcountry == "Netherlands"          ~ "Northern Europe" ,               
  bplcountry == "Mexico"    ~ "Central America" ,                         
  bplcountry == "Romania"       ~ "Eastern Europe" ,                      
  bplcountry == "Poland"      ~ "Eastern Europe" ,                        
  bplcountry == "Ireland"    ~ "Western Europe" ,                       
  bplcountry == "Korea, RO (South)"     ~ "Eastern Asia" ,              
  bplcountry == "Syria"           ~ "Western Asia" ,                   
  bplcountry == "Morocco" ~ "Northern Africa" ,                            
  bplcountry == "Egypt"    ~ "Northern Africa" ,                           
  bplcountry == "Hungary"          ~ "Eastern Europe" ,                   
  bplcountry == "Czech Republic/Czechoslovakia"   ~ "Eastern Europe" ,    
  bplcountry == "Canada"          ~ "North America" ,                    
  bplcountry == "El Salvador"     ~ "Central America" ,                   
  bplcountry == "Guatemala"        ~ "Central America" ,                  
  bplcountry == "Nicaragua"        ~ "Central America" ,                  
  bplcountry == "Honduras"          ~ "Central America" ,                 
  bplcountry == "Puerto Rico"        ~ "Caribbean" ,                
  bplcountry == "Dominican Republic"    ~ "Caribbean" ,             
  bplcountry == "Haiti"              ~ "Caribbean" ,                
  bplcountry == "Belize/British Honduras"   ~ "South America" ,          
  bplcountry == "Ukraine"        ~ "Eastern Europe" ,                     
  bplcountry == "Sweden"               ~ "Northern Europe" ,               
  bplcountry == "Other Caribbean and n.s."      ~ "Caribbean" ,     
  bplcountry == "Israel"          ~ "Western Asia" ,                   
  bplcountry == "Croatia"      ~ "Eastern Europe" ,                       
  bplcountry == "Armenia"     ~ "Western Asia" ,                       
  bplcountry == "Georgia"       ~ "Western Asia" ,                     
  bplcountry == "Azerbaijan"   ~ "Western Asia" ,                      
  bplcountry == "Iran"         ~ "Western Asia" ,                      
  bplcountry == "Tajikistan"      ~ "Western Asia" ,                   
  bplcountry == "Uzbekistan"      ~ "Western Asia" ,                   
  bplcountry == "Turkmenistan"    ~ "Western Asia" ,                   
  bplcountry == "Kazakhstan"~ "Western Asia" ,
  bplcountry == "Moldova"    ~ "Eastern Europe" ,                         
  bplcountry == "Belarus"      ~ "Eastern Europe" ,                       
  bplcountry == "European Union"  ~ "Europe" ,                    
  bplcountry == "Turkey"         ~ "Western Asia" ,                    
  bplcountry == "Greece"             ~ "Southern Europe" ,                 
  bplcountry == "Bulgaria"     ~ "Eastern Europe" ,                       
  bplcountry == "Australia"   ~ "Oceania" ,                       
  bplcountry == "Cyprus"         ~ "Southern Europe" ,                     
  bplcountry == "Indonesia"     ~ "South-Eastern Asia" ,                      
  bplcountry == "Finland"           ~ "Northern Europe" ,                  
  bplcountry == "Central/South America and Caribbean" ~ "South America",
  bplcountry == "Serbia"            ~ "Eastern Europe" ,                  
  bplcountry == "Philippines"        ~ "South-Eastern Asia" ,                 
  bplcountry == "Austria"             ~ "Eastern Europe" ,                
  bplcountry == "Macedonia"                 ~ "Eastern Europe" ,          
  bplcountry == "Palestinian Territories"    ~ "Western Asia" ,        
  bplcountry == "Qatar"    ~ "Western Asia" ,                          
  bplcountry == "Bangladesh"        ~ "Southern Asia" ,                  
  bplcountry == "Oceania, n.s."    ~ "Oceania" ,                  
  bplcountry == "Thailand"          ~ "South-Eastern Asia" ,                  
  bplcountry == "Lithuania"             ~ "Northern Europe" ,              
  bplcountry == "Sri Lanka (Ceylon)"        ~ "Southern Asia" ,          
  bplcountry == "Denmark"    ~ "Northern Europe" ,                         
  bplcountry == "Kyrgyzstan"      ~ "Western Asia" ,                    
  bplcountry == "Slovakia"           ~ "Eastern Europe" ,                 
  bplcountry == "Malaysia"      ~ "Southern Asia" ,                      
  bplcountry == "New Zealand"       ~ "Oceania" ,                 
  bplcountry == "Latvia"              ~ "Northern Africa" ,                
  bplcountry == "Mongolia"            ~ "Eastern Asia" ,                
  bplcountry == "Libya"                ~ "Northern Africa" ,               
  bplcountry == "Vietnam"         ~ "South-Eastern Asia" ,                    
  bplcountry == "Saudi Arabia"         ~ "Western Asia" ,              
  bplcountry == "Mauritius"              ~ "Southern Africa" ,             
  bplcountry == "Singapore"   ~ "South-Eastern Asia" ,                        
  bplcountry == "Estonia"        ~ "Northern Europe" ,                     
  bplcountry == "Norway"         ~ "Northern Europe" ,                     
  bplcountry == "Eastern Asia"   ~ "Eastern Asia" ,                     
  bplcountry == "Nepal"                ~ "Eastern Asia" ,               
  bplcountry == "Bosnia and Herzegovina"   ~ "Eastern Europe" ,           
  bplcountry == "Malta"             ~ "Southern Europe" ,                  
  bplcountry == "Taiwan"                     ~ "South-Eastern Asia" ,         
  bplcountry == "Kuwait"    ~ "Western Asia" ,                         
  bplcountry == "Slovenia"     ~ "Eastern Europe" ,                       
  bplcountry == "Bermuda"      ~ "Caribbean" ,                      
  bplcountry == "Luxembourg"            ~ "Western Europe" ,            
  bplcountry == "Macau"        ~ "South-Eastern Asia" ,                       
  bplcountry == "East Timor"     ~ "South-Eastern Asia" ,                     
  bplcountry == "Cambodia"       ~ "South-Eastern Asia" ,                     
  bplcountry == "Tonga"   ~ "Oceania" ,                           
  bplcountry == "Benin"              ~ "Western Africa" ,                 
  bplcountry == "West Africa, n.s."     ~ "Western Africa" ,              
  bplcountry == "Mauritania"   ~ "Northern Africa" ,                       
  bplcountry == "Chad"          ~ "Eastern Africa" ,                     
  bplcountry == "Americas"     ~"America"   ,                   
  bplcountry == "Korea"        ~ "Eastern Asia" ,                       
  bplcountry == "Jamaica"        ~ "Caribbean" ,                    
  bplcountry == "Guyana/British Guiana"      ~ "South America" ,         
  bplcountry == "Trinidad and Tobago"     ~ "Caribbean" ,           
  bplcountry == "Barbados"              ~ "Caribbean" ,             
  bplcountry == "Eastern Africa, n.s."          ~ "Eastern Africa" ,      
  bplcountry == "Yugoslavia"~ "Eastern Europe" , 
  bplcountry == "Liberia"               ~ "Western Africa" ,              
  bplcountry == "Laos"            ~ "South-Eastern Asia" ,                    
  bplcountry == "St. Vincent"     ~ "Caribbean" ,                   
  bplcountry == "Dominica"        ~ "Caribbean" ,                   
  bplcountry == "American Samoa"        ~ "Oceania" ,             
  bplcountry == "U.S. Virgin Islands"     ~ "Oceania" ,           
  bplcountry == "Israel/Palestine"        ~ "Western Asia" ,           
  bplcountry == "Eritrea"               ~ "Eastern Africa" ,              
  bplcountry == "Samoa"                   ~ "Oceania" ,            
  bplcountry == "Yemen"                ~ "Western Asia" ,              
  bplcountry == "Iceland"             ~ "Northern Europe" ,                
  bplcountry == "Bahamas"              ~ "Caribbean" ,              
  bplcountry == "Antigua-Barbuda"             ~ "Caribbean" ,       
  bplcountry == "St. Lucia"                   ~ "Caribbean" ,       
  bplcountry == "Micronesia"                    ~ "Oceania" ,     
  bplcountry == "Northern Mariana Isls."       ~ "Oceania" ,      
  bplcountry == "Northern Africa, n.s."         ~ "Northern Africa" ,      
  bplcountry == "Grenada"                       ~ "Caribbean" ,      
  bplcountry == "Marshall Islands"                  ~ "Oceania" , 
  bplcountry == "Bhutan"      ~ "Southern Asia" ,                        
  bplcountry == "Montenegro"         ~ "Eastern Europe" )   )


## an ordered factor for bplregionw
dat = dat %>% 
  mutate(bplregionw.ord = factor(bplregionw,
                                 levels = c(  "Northern Africa" ,"Western Africa" ,  
                                              "Central Africa"  ,  "Eastern Africa" ,    
                                              "Southern Africa"   ,    "Africa" ,
                                              "Western Asia",  "Eastern Asia" ,      
                                              "South-Eastern Asia",   "Southern Asia",    
                                              "Asia" , 
                                              "Northern Europe" ,  "Western Europe" ,      
                                              "Eastern Europe" ,    "Southern Europe",   
                                              "Russia" ,   "Europe" ,   
                                              "North America" ,   "Central America"  ,   
                                              "Caribbean"  ,"South America"  ,  
                                              "America" ,          
                                              "Oceania" ),
                                 ordered = TRUE))


## Completing regionw
dat = dat %>% mutate(regionw= case_when(country=="Benin" ~ "Western Africa",
                                        country=="Philippines" ~ "South-Eastern Asia",
                                        country=="United States" ~ "North America",
                                        country=="Zimbabwe" ~ "Southern Africa" ,
                                        TRUE ~ regionw))
## Ordered factor for regionw
dat =  dat %>% mutate(regionw.ord = factor(regionw,
                                           levels = c("Southern Africa" ,   
                                                      "Eastern Africa" ,   
                                                      "Western Africa" , 
                                                      "Northern Africa" ,
                                                      "South America",     
                                                      "Central America" , 
                                                      "North America",
                                                      "Caribbean" , 
                                                      "Melanesia"        , 
                                                      "South-Eastern Asia" ,
                                                      "Southern Asia" ,    
                                                      "Western Asia" ,     
                                                      "Eastern Europe"  ,  
                                                      "Western Europe" ,    
                                                      "Southern Europe" ,  
                                                      "Northern Europe" ),
                                           ordered = TRUE))

# ## a coarser aggregation level for countries
# dat = dat %>% mutate(regionW.ord = case_when(regionw == "Southern Africa" ~ "Africa",   
#                                              regionw == "Eastern Africa"  ~ "Africa",   
#                                              regionw == "Western Africa"  ~ "Africa", 
#                                              regionw == "Northern Africa"  ~ "Africa",
#                                              regionw == "South America" ~ "C&S America",     
#                                              regionw == "Central America"  ~ "C&S America", 
#                                              regionw == "North America" ~ "N America",
#                                              regionw == "Caribbean" ~ , 
#                                              regionw == "Melanesia"        , 
#                                              regionw == "South-Eastern Asia" ~ "Asia",
#                                              regionw == "Southern Asia"  ~ "Asia",    
#                                              regionw == "Western Asia"  ~ "Asia",     
#                                              regionw == "Eastern Europe" ~ "Europe" ,  
#                                              regionw == "Western Europe" ~ "Europe" ,    
#                                              regionw == "Southern Europe"  ~ "Europe",  
#                                              regionw == "Northern Europe"  ~ "Europe"))


## yrimm as numeric:
dat = dat %>%
  mutate(yrimm = case_when(yrimm =="NIU (not in universe)" ~ NA_character_ ,
                           yrimm == "Unknown" ~ NA_character_ ,
                           TRUE ~ yrimm)) %>%
  mutate(yrimm = as.numeric(yrimm))


## agenum: age as numeric
dat = dat %>%
  mutate(agenum = case_when(age == "Not reported/missing" ~ NA_character_ ,
                            age == "2 years" ~ "2" ,
                            age == "100+" ~ "100",
                            TRUE ~ age)) %>%
  mutate(agenum = as.numeric(agenum))

# Age binned
dat = dat %>% mutate(age_binned = case_when(agenum <= 30 ~ "less than 30",
                                            agenum > 30 & agenum <= 40 ~ "30-40",
                                            agenum  > 40 & agenum <= 50 ~ "40-50",
                                            agenum > 50 ~ "more than 50")) %>% 
  mutate(age_binned = factor(age_binned,
                             levels = c("less than 30",
                                        "30-40",
                                        "40-50",
                                        "more than 50"),
                             ordered = FALSE))

# Age binned ordered
dat = dat %>% 
  mutate(age_binned_ord = factor(age_binned,
                             levels = c("less than 30",
                                        "30-40",
                                        "40-50",
                                        "more than 50"),
                             ordered = TRUE))



## Age at migration
dat = dat %>% mutate(age_imm = yrimm - year + agenum )


## Country populations in 2010 (OECD) -------
# copy pasted from https://en.wikipedia.org/wiki/List_of_countries_by_population_in_2010
counpop = readLines("~/Dropbox/Work/WHO/SHARED/IPUMS/Data/misc_data/Country_pop_World_Bank_2010.csv")
counpop = as_tibble(counpop)
counpop %<>% separate(sep=";",
                      col="value",
                      into = c("Rank","Country", "Population",
                               "Change from 2005","Area","Density"))
counpop %<>% select(2,3)
counpop %<>% mutate(Country = str_replace_all(Country, 
                                              pattern="align=left| \\{\\{flag", 
                                              replacement=""))
counpop %<>% mutate(Country = str_replace_all(Country, 
                                              pattern="\\}\\}", 
                                              replacement=""))
counpop %<>% mutate(Country = str_sub(Country, 3))   
counpop %<>% mutate(Country = str_replace(Country,"<ref.*",""))
counpop %<>% mutate(Population = str_replace_all(Population,",",""))
counpop %<>% mutate(Population = as.numeric(Population))

# an ordered factor for country ----
dat %<>% mutate(country.ord = factor(ordered =TRUE,
                                     country,levels= c("Egypt",
                                                       "Mali",
                                                       "Benin",
                                                       "Zambia",
                                                       "Malawi",
                                                       "Mozambique",
                                                       "Zimbabwe",
                                                       "Botswana",
                                                       "South Africa",
                                                       "United States",
                                                       "Puerto Rico",
                                                       "Mexico",
                                                       "El Salvador",
                                                       "Panama",
                                                       "Ecuador",
                                                       "Peru",
                                                       "Brazil",
                                                       "Uruguay",
                                                       "Fiji",
                                                       "Philippines",
                                                       "Cambodia",
                                                       "Iran",
                                                       "Armenia",
                                                       "Belarus",
                                                       "France",
                                                       "Greece",
                                                       "Ireland",
                                                       "Portugal",
                                                       "Romania")))

# urban
dat$urban[dat$urban=="Unknown"] = NA

# saving  ----------------
save(dat,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/AllHWF-Data_Formatted.Rdat')


save(counpop,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/counpop.Rdat')

## saving a random subset of 1% of full dataset
set.seed(12345)
subs = sample(1:nrow(dat),replace=FALSE, size=floor(nrow(dat)/100))
subdat0.01 = dat[subs,]
save(subdat0.01,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/Sub0.01_HWF-Data_Formatted.Rdat')

subs = sample(1:nrow(dat),replace=FALSE, size=floor(nrow(dat)/10))
subdat0.1 = dat[subs,]
save(subdat0.1,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/Sub0.1_HWF-Data_Formatted.Rdat')

subs = sample(1:nrow(dat),replace=FALSE, size=floor(nrow(dat)/3))
subdat0.3 = dat[subs,]
save(subdat0.3,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/Sub0.3_HWF-Data_Formatted.Rdat')

subs = sample(1:nrow(dat),replace=FALSE, size=floor(nrow(dat)/2))
subdat0.5 = dat[subs,]
save(subdat0.5,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/Sub0.5_HWF-Data_Formatted.Rdat')


readmetext = "Data file AllHWF-Data_Formatted.Rdat generated by R script \n ~/Dropbox/Work/WHO/SHARED/IPUMS/R/Format.R "
write.table(readmetext,file = '~/Dropbox/Work/WHO/SHARED/IPUMS/Data/Formatted/README.txt',
            col.names = FALSE,quote=FALSE)
## EOF







 






