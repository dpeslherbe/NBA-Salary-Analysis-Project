ALTER TABLE nbaplayers
ADD salarycap int;

ALTER TABLE nbaplayers
ADD salaryperc DEC(6,5);

SELECT Season, contracts FROM nbaplayers

SET SQL_SAFE_UPDATES=0;

UPDATE personal.nbaplayers
SET salarycap = CASE
when Season = '1990-91' then 11871000
when Season = '1991-92' then 12500000
when Season = '1992-93' then 14000000
when Season = '1993-94' then 15175000
when Season = '1994-95' then 15964000
when Season = '1995-96' then 23000000
when Season = '1996-97' then 24363000
when Season = '1997-98' then 26900000
when Season = '1998-99' then 30000000
when Season = '1999-00' then 34000000
when Season = '2000-01' then 35500000
when Season = '2001-02' then 42500000
when Season = '2002-03' then 40271000
when Season = '2003-04' then 43840000
when Season = '2004-05' then 43870000
when Season = '2005-06' then 49500000
when Season = '2006-07' then 53135000
when Season = '2007-08' then 55630000
when Season = '2008-09' then 58680000
when Season = '2009-10' then 57700000
when Season = '2010-11' then 58044000
when Season = '2011-12' then 58044000
when Season = '2012-13' then 58044000
when Season = '2013-14' then 58679000
when Season = '2014-15' then 63065000
when Season = '2015-16' then 70000000
when Season = '2016-17' then 94143000
when Season = '2017-18' then 99093000
when Season = '2018-19' then 101869000
when Season = '2019-20' then 109140000
end;

UPDATE personal.nbaplayers
SET salaryperc = contracts / salarycap;

SELECT salarycap, salaryperc FROM nbaplayers

SELECT names, Season, salaryperc, fg2mPerGame, pctFG2, fg3mPerGame, pctFG3, ftmPerGame, pctFT, drbPerGame, orbPerGame, astPerGame, blkPerGame, stlPerGame, tovPerGame, minutesPerGame, ratioOWS, ratioDWS, ratioOBPM, ratioDBPM, ratioVORP FROM nbaplayers
WHERE minutesPerGame > 24
AND countGames > 41
ORDER BY ptsPerGame DESC
