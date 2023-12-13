DROP TABLE IF EXISTS gdp_rec;
CREATE TABLE gdp_rec (
    cid INT PRIMARY KEY,
    countryNameG TEXT,
    year INT,
    gdp FLOAT
);


INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (1, 'India' , 2010, 500.0);
INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (2, 'India' , 2015, 600.0);
INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (3, 'India' , 2020, 700.0);


INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (4, 'China' , 2010, 800.0);
INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (5, 'China' , 2015, 3400.0);
INSERT INTO gdp_rec (cid, countryNameG, year, gdp) VALUES (6, 'China' , 2020, 24240.0);

DROP TABLE IF EXISTS gdp;
CREATE TABLE gdp (
    countryNameP TEXT PRIMARY KEY,
    y2010 FLOAT,
    y2015 FLOAT,
    y2020 FLOAT
);

INSERT INTO gdp (countryNameP, y2010, y2015, y2020) SELECT countryNameG,
       MAX(CASE WHEN year = 2010 THEN gdp END),
       MAX(CASE WHEN year = 2015 THEN gdp END), 
       MAX(CASE WHEN year = 2020 THEN gdp END)
FROM gdp_rec GROUP BY countryNameG;

SELECT * FROM gdp;
