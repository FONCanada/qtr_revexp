# Setup the Environment
source("R/setup.R")

# Fetch the Quarterly GDP, Population, Rev/Exp Data, and monthly CPI data
gdpdata<-getTABLE("36100104")
gdp<-gdpdata %>%
  filter(GEO=="Canada" & 
           Estimates=="Gross domestic product at market prices" & 
           Prices=="Current prices" &
           Seasonal.adjustment=="Seasonally adjusted at annual rates") %>%
  select(Ref_Date,GDP=Value)
popdata<-getTABLE("17100009")
pop<-popdata %>%
  filter(GEO=="Canada",Ref_Date>="Jan 1961") %>%
  select(Ref_Date,Pop=Value)
revexpdata<-getTABLE("36100477")
revexp<-revexpdata %>%
  filter(Seasonal.adjustment=="Seasonally adjusted at annual rates",
         Levels.of.government %in% c("Federal general government",
                                     "Provincial and territorial general governments",
                                     "Local general governments",
                                     "Canada Pension Plan",
                                     "Quebec Pension Plan",
                                     "General governments"),
         Estimates %in% c("General governments revenue",
                          "Income taxes",
                          "From corporations and government business enterprises, liabilities",
                          "Taxes on products",
                          "Sales taxes",
                          "Licences, permits and fees",
                          "Contributions to social insurance plans",
                          "Taxes on production and imports",
                          "Current transfers to households",
                          "Subsidies",
                          "Goods and services tax (GST)",
                          "Excise taxes",
                          "Goods and Services Tax credit",
                          "Old age security",
                          "Canada Child Benefit",
                          "University Childcare Benefit",
                          "Child Tax Credit",
                          "Defence",
                          "Family and youth allowances",
                          "Canada Assistance Plan",
                          "General governments expenditure",
                          "Canada Health and Social Transfer",
                          "Canada Health Transfer",
                          "Canada Social Transfer",
                          "Gasoline and motive fuel taxes",
                          "Royalties",
                          "Interest and other investment income",
                          "Real property taxes",
                          "Social assistance",
                          "Sales of goods and services",
                          "Manufacturers' sales tax",
                          "Taxation agreements",
                          "Custom import duties",
                          "Employment Insurance benefits",
                          "Remitted profits of government business enterprises",
                          "Current transfers from general governments",
                          "Current transfers to general governments",
                          "General governments surplus or deficit",
                          "Interest on debt")) %>%
  select(Ref_Date,Levels.of.government,Estimates,Value) %>%
  mutate(Estimates=case_when(
    Estimates=="From corporations and government business enterprises, liabilities" ~ "Corporate Income Taxes",
    Estimates=="Income taxes" ~ "Personal Income Taxes",
    Estimates=="General governments surplus or deficit" ~ "Surplus or deficit",
    Estimates=="General governments revenue" ~ "Total revenue",
    Estimates=="General governments expenditure" ~ "Total expenditure",
    TRUE ~ as.character(Estimates)
  ))
cpidata<-getTABLE("18100004")
cpi<-cpidata %>%
  filter(Products.and.product.groups=="All-items",
         GEO=="Canada") %>%
  select(Ref_Date,CPI=Value)

# Add program expenditures
revexp<-revexp %>%
  rbind(
    revexp %>%
      filter(Estimates %in% c("Interest on debt","Total expenditure")) %>%
      spread(Estimates,Value) %>%
      mutate(Value=`Total expenditure`-`Interest on debt`,
             Estimates="Total program spending") %>%
      select(Ref_Date,Levels.of.government,Estimates,Value)
  )

# Merge the Data Together to Construct the Several Normalizations
maindata<-revexp %>%
  left_join(cpi,by="Ref_Date") %>%
  left_join(pop,by="Ref_Date") %>%
  left_join(gdp,by="Ref_Date") %>%
  group_by(Levels.of.government,Estimates) %>%
  mutate(percap=1000000*Value/Pop,
         nominal=Value,
         gdpshare=Value/GDP,
         real=Value*CPI[n()]/CPI,
         realpercap=1000000*real/Pop,
         Date=as.Date(Ref_Date)) %>%
  select(Date,Levels.of.government,Estimates,
         percap,nominal,gdpshare,real,realpercap) %>%
  gather(Normalization,Value,-Date,-Levels.of.government,-Estimates) %>%
  mutate(Normalization=case_when(
    Normalization=="real" ~ paste0("Millions (Real $ ",max(revexp$Ref_Date),")"),
    Normalization=="realpercap" ~ paste0("Dollars per Capita (Real $ ",max(revexp$Ref_Date),")"),
    Normalization=="nominal" ~ "Millions (Nominal $)",
    Normalization=="percap" ~ "Dollars per Capita (Nominal)",
    Normalization=="gdpshare" ~ "Share of GDP",
  )) %>%
  ungroup() %>%
  mutate(Value=ifelse(Value==0,NA,Value)) %>%
  arrange(Date,Levels.of.government,Estimates,Normalization)

# Overwrite the Google Sheet. Run gs4_auth() at some point first on a new system; login using FON credentials
gs4_auth()
sheet_write(maindata,ss="174l05aIIAwLlCGlB8-WM_kW7R0Dm0AYAbPRnDJ_7pIk",sheet="maindata")

# Create a CSV File
write.csv(maindata,"qtr_revexp_data.csv",row.names = F)
