# Build 

`stack build`

# Run

`stack run`

# Documentation

```shell
stack haddock --haddock-arguments --quickjump
```

And then open an http server at the resulting folder. Below example given with python3 http.server

`python3 -m http.server 8080 -d .stack-work/install/x86_64-linux/c5766df4c7e86cbb5f0683ffa849d218a0af6c1bed70a74fe36d7c3d91d9ed4c/9.4.7/doc/`

## UN Data URLs
GDP URL : https://data.un.org/_Docs/SYB/CSV/SYB66_230_202310_GDP%20and%20GDP%20Per%20Capita.csv
Population URL : https://data.un.org/_Docs/SYB/CSV/SYB66_1_202310_Population,%20Surface%20Area%20and%20Density.csv

## World Bank Data URLs
GDP URL : https://api.worldbank.org/v2/en/indicator/NY.GDP.MKTP.CD?downloadformat=csv
Population URL : https://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv
