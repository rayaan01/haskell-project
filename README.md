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

```shell
curl 'http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=variableID:12&DataMartId=PopDiv&Format=csv&c=2,4,6,7&s=_crEngNameOrderBy:asc,_timeEngNameOrderBy:desc,_varEngNameOrderBy:asc'   -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7'   -H 'Accept-Language: en-IN,en;q=0.9'   -H 'Connection: keep-alive'   -H 'Cookie: ASP.NET_SessionId=jttf5o55iaicce55orl0kxei'   -H 'Referer: http://data.un.org/Data.aspx?q=population&d=PopDiv&f=variableID%3a12'   -H 'Upgrade-Insecure-Requests: 1'   -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36'   --compressed   --insecure --output lo```

Generated from: http://data.un.org/Data.aspx?q=population&d=PopDiv&f=variableID%3a12

Example for austrilia (aus)
https://api.worldbank.org/v2/country/aus/indicator/SP.POP.TOTL?date=2000
