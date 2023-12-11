# Data Download Links

## Worldbank
pop : https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=xml
GDP : https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=xml
## UN
Pop : https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv
GDP : https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv


## The above links were coppied from:

### WorldBank
Pop: https://data.worldbank.org/indicator/SP.POP.TOTL
GDP: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?most_recent_year_desc=false

### Data UN
https://data.un.org/

# API'S For getting GDP per country

## UN one 

http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=Indicator_Code:NY.GDP.MKTP.PP.CD;Country_Code:BEL,BLZ,BRB&DataMartId=WDI&Format=csv&c=2,4,5&s=Country_Name:asc,Year:desc

genereated from: http://data.un.org/Data.aspx?q=gdp&d=WDI&f=Indicator_Code%3aNY.GDP.MKTP.PP.CD
## Worldbank
Example for austrilia (aus)
https://api.worldbank.org/v2/country/aus/indicator/NY.GDP.MKTP.CD?date=2000

# API for getting population

## UN one

Curl query for some countries:

https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv
## WorldBank
Example for austrilia (aus)
https://api.worldbank.org/v2/country/aus/indicator/SP.POP.TOTL?date=2000

# MISC
## API Indicators

Population: SP.POP.TOTL
GDP:        NY.GDP.MKTP.CD
