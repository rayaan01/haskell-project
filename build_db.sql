-- | Dropping the databases if already exists
DROP TABLE IF EXISTS POPULATION;
DROP TABLE IF EXISTS GDP;

-- | Creating the population data base table
CREATE TABLE POPULATION (
    countryID INTEGER PRIMARY KEY,
    countryNameP TEXT,
    capital TEXT,
    pop2010 INTEGER,
    pop2015 INTEGER,
    pop2021 INTEGER
);

-- | Creating the GDP database table
CREATE TABLE GDP (
    countryNameG TEXT PRIMARY KEY,
    gdp2010 FLOAT,
    gdp2015 FLOAT,
    gdp2021 FLOAT
);

-- | Testing if working.
INSERT INTO POPULATION (countryID, countryNameP, capital, pop2010, pop2015, pop2021)
VALUES (1, 'India', 'Delhi', 1100000, 1200000, 1300000);

INSERT INTO GDP (countryNameG, gdp2010, gdp2015, gdp2021)
VALUES ('India', 600.0, 700.0, 800.0);

INSERT INTO POPULATION (countryID, countryNameP, capital, pop2010, pop2015, pop2021)
VALUES (2, 'China', 'Beijing', 110120000, 1200000, 1300000);

INSERT INTO GDP (countryNameG, gdp2010, gdp2015, gdp2021)
VALUES ('China', 6050.0, 7050.0, 8050.0);
