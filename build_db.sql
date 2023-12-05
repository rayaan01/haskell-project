DROP TABLE IF EXISTS POPULATION;
DROP TABLE IF EXISTS GDP;


CREATE TABLE POPULATION (
       countryID INTEGER PRIMARY KEY,
       countryNameP TEXT,
       continent TEXT,
       capital TEXT
       );

CREATE TABLE GDP (
       countryNameG TEXT PRIMARY KEY,
       totalPopulation INTEGER,
       gdp FLOAT
       );


INSERT INTO POPULATION (countryID,countryNameP,continent,capital)
VALUES (1,'India','Asia','Delhi');

INSERT INTO GDP (countryNameG,totalPopulation,gdp)
VALUES ('India','1000','500');