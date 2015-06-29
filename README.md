# NFL_RB_Combine
 
I analyze the relation between a running back's performance in the NFL scouting combine (https://en.wikipedia.org/wiki/NFL_scouting_combine) and subsequent performance in the NFL for the first four years of a players career, for 102 players drafted from 2002-2011. The physical tests of the scouting combine consists of six events: 40 yard dash, bench press (225 lbs repetitions), vertical jump, broad jump, 20 yard shuttle, and 3 cone drill. These events are designed to measure, among other things, a player's speed, strength, explosiveness, and agility, and are expected to correlate with a player's success in a league. The analysis shows that the 40 yard dash is the best predictor of performance out of the available variables.

The data is scraped from two websites - the scouting combine data is from http://nflcombineresults.com, and the players' statistics in the NFL are from http://www.pro-football-reference.com. There are some errors in the data, and some challenges to dealing with the data structure of the websites, so I am still working on making the code more generalizable.  

A write-up of the results can be seen here: https://www.reddit.com/r/nfl/comments/3bicvy/relation_between_scouting_combine_performance_and/

Code description:
"download data.r" scrapes the data from the two websites and cleans them into a format suitable for analysis.
"analysis.r" shows some basic plots of the data, imputes missing combine data, and runs a regression analysis and random forest on the data.
"multiplot.r" is a function I found online to help with plotting (attribution inside)
