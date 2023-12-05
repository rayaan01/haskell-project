# haskell-project


-- Population data  https://data.worldbank.org/indicator/SP.POP.TOTL
-- GDP Data https://databank.worldbank.org/source/world-development-indicators

## Indicators
=======
-- country ID | Country name | Contient | Capital
-- Country name | Population | GDP

Population: SP.POP.TOTL
GDP:        NY.GDP.MKTP.CD

## API For getting GDP per country

### UN one (it works)

http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=Indicator_Code:NY.GDP.MKTP.PP.CD;Country_Code:BEL,BLZ,BRB&DataMartId=WDI&Format=csv&c=2,4,5&s=Country_Name:asc,Year:desc

genereated from: http://data.un.org/Data.aspx?q=gdp&d=WDI&f=Indicator_Code%3aNY.GDP.MKTP.PP.CD

Example for austrilia (aus)
https://api.worldbank.org/v2/country/aus/indicator/NY.GDP.MKTP.CD?date=2000

## API for getting population

### UN one (it works)

Curl query for some countries:

https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv

Example for austrilia (aus)
https://api.worldbank.org/v2/country/aus/indicator/SP.POP.TOTL?date=2000
