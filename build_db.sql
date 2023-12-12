DROP TABLE IF EXISTS POPULATION;
DROP TABLE IF EXISTS GDP;

CREATE TABLE POPULATION (
    countryID INTEGER PRIMARY KEY,
    countryNameP TEXT,
    capital TEXT,
    pop2005 INTEGER,
    pop2010 INTEGER,
    pop2015 INTEGER,
    pop2020 INTEGER
);

CREATE TABLE GDP (
    countryNameG TEXT PRIMARY KEY,
    gdp2005 FLOAT,
    gdp2010 FLOAT,
    gdp2015 FLOAT,
    gdp2020 FLOAT
);

INSERT INTO POPULATION (countryID, countryNameP, capital, pop2005, pop2010, pop2015, pop2020)
VALUES (1, 'India', 'Delhi', 1000000, 1100000, 1200000, 1300000);

INSERT INTO GDP (countryNameG, gdp2005, gdp2010, gdp2015, gdp2020)
VALUES ('India', 500.0, 600.0, 700.0, 800.0);

INSERT INTO POPULATION (countryID, countryNameP, capital, pop2005, pop2010, pop2015, pop2020)
VALUES (2, 'China', 'BC', 10001200, 110120000, 1200000, 1300000);

INSERT INTO GDP (countryNameG, gdp2005, gdp2010, gdp2015, gdp2020)
VALUES ('China', 5050.0, 6050.0, 7050.0, 8050.0);


Drop existing tables if they exist
