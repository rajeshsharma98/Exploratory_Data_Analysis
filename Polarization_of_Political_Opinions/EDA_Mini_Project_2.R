setwd("~/Documents/EDA/Project Files")

library(magrittr)
library(tidyverse)
library(ggbiplot)
library(ggrepel)
library(plyr)

#reading all the files
members_1989 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1989/members.csv")
votes_1989 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1989/votes.csv")
members_1990 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1990/members.csv")
votes_1990 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1990/votes.csv")
members_1991 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1991/members.csv")
votes_1991 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1991/votes.csv")
members_1992 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1992/members.csv")
votes_1992 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1992/votes.csv")
members_1993 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1993/members.csv")
votes_1993 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1993/votes.csv")
members_1994 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1994/members.csv")
votes_1994 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1994/votes.csv")
members_1995 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1995/members.csv")
votes_1995 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1995/votes.csv")
members_1996 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1996/members.csv")
votes_1996 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1996/votes.csv")
members_1997 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1997/members.csv")
votes_1997 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1997/votes.csv")
members_1998 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1998/members.csv")
votes_1998 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1998/votes.csv")
members_1999 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1999/members.csv")
votes_1999 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/1999/votes.csv")
members_2000 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2000/members.csv")
votes_2000 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2000/votes.csv")
members_2001 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2001/members.csv")
votes_2001 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2001/votes.csv")
members_2002 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2002/members.csv")
votes_2002 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2002/votes.csv")
members_2003 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2003/members.csv")
votes_2003 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2003/votes.csv")
members_2004 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2004/members.csv")
votes_2004 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2004/votes.csv")
members_2005 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2005/members.csv")
votes_2005 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2005/votes.csv")
members_2006 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2006/members.csv")
votes_2006 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2006/votes.csv")
members_2007 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2007/members.csv")
votes_2007 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2007/votes.csv")
members_2008 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2008/members.csv")
votes_2008 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2008/votes.csv")
members_2009 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2009/members.csv")
votes_2009 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2009/votes.csv")
members_2010 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2010/members.csv")
votes_2010 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2010/votes.csv")
members_2011 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2011/members.csv")
votes_2011 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2011/votes.csv")
members_2012 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2012/members.csv")
votes_2012 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2012/votes.csv")
members_2013 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2013/members.csv")
votes_2013 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2013/votes.csv")
members_2014 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2014/members.csv")
votes_2014 = read.csv("/Users/ashish/Documents/EDA/Project Files/congress/2014/votes.csv")


#-------------------------------------------Section 1--------------------------------------------#
#joining votes and numbers 
#Year 1990
joined_1990 = join(members_1990, votes_1990, by = "id")
vote_1990 = joined_1990[,(-1:-6)]
member_1990 = joined_1990[,1:6]
#Year 2010
joined_2010 = join(members_2010, votes_2010, by = "id")
vote_2010 = joined_2010[,(-1:-6)]
member_2010 = joined_2010[,1:6]

voted_yes_1990 = (vote_1990 == "Yea")
member_1990$yes_fraction = rowMeans(voted_yes_1990)

voted_yes_2010 = (vote_2010 == "Yea")
member_2010$yes_fraction = rowMeans(voted_yes_2010)



#Function to change votes to numbers
recode_votes = function(vote) {
  if(is.na(vote)) {
    return(0)
  } else if(vote == "Yea") {
    return(1)
  } else if(vote == "Nay") {
    return(-1)
  } else {
    return(0)
  }
}

#converting votes in numbers to apply PCA
votes_numeric_1990 = apply(vote_1990, 1:2, recode_votes)
votes_numeric_2010 = apply(vote_2010, 1:2, recode_votes)




#MDS for 2010
senator_distances_2010 = dist(votes_numeric_2010, method = "euclidean")
senator_mds_2010 = cmdscale(senator_distances_2010, eig = TRUE)

ggplot(data.frame(eig = senator_mds_2010$eig, index = 1:length(senator_mds_2010$eig))) + geom_point(aes(x = index, y = eig)) + 
  labs(
    x = "Eigen Value Index",
    y = "Eigen Values",
    title = "Scree Plot for senator of year 2010"
  )

senator_points_2010 = senator_mds_2010$points[,1:2] %>% as_tibble %>%
  setNames(paste0("MDS", 1:2)) %>% mutate(
    name = rownames(votes_numeric_2010))
senator_points_2010$names = joined_2010$display_name
ggplot(senator_points_2010, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(color = members_2010$party), size = 2) + coord_fixed() + 
  labs(
    x = "MDS for first principal axis",
    y = "MDS for second principal axis",
    title = "MDS distances for senators of year 2010"
  ) + 
  geom_text_repel(aes(label = names))

#MDS for 1990
senator_distances_1990 = dist(votes_numeric_1990, method = "euclidean")
senator_mds_1990 = cmdscale(senator_distances_1990, eig = TRUE)

ggplot(data.frame(eig = senator_mds_1990$eig, index = 1:length(senator_mds_1990$eig))) + geom_point(aes(x = index, y = eig)) + 
  labs(
    x = "Eigen Value Index",
    y = "Eigen Values",
    title = "Scree Plot for senator of year 1990"
  )

senator_points_1990 = senator_mds_1990$points[,1:2] %>% as_tibble %>%
  setNames(paste0("MDS", 1:2)) %>% mutate(
    name = rownames(votes_numeric_1990))
senator_points_1990$names = joined_1990$display_name
ggplot(senator_points_1990, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(color = members_1990$party), size = 2) + coord_fixed() + 
  labs(
    x = "MDS for first principal axis",
    y = "MDS for second principal axis",
    title = "MDS distances for senators of year 2010"
  ) + 
  geom_text_repel(aes(label = names))

#-------------------------------------------Section 2--------------------------------------------#
mds_var = function(vote, member){
  
  # Step 1
  joined = join(member, vote, by = "id")
  democrat = joined[joined$party == "D"]
  republic = joined[joined$party == "R"]
  members_democrat = democrat[,1:6]
  members_republic = republic[,1:6]
  votes_democrat = democrat[,(-1:-6)]
  votes_republic = republic[,(-1:-6)]
  
  # Step 2
  
  vote_num_d = apply(votes_democrat, 1:2, recode_votes)
  vote_num_r = apply(votes_republic, 1:2, recode_votes)
  
  senator_dist_d = dist(vote_num_d, method = "euclidean")
  senator_mds_d = cmdscale(senator_dist_d, eig = TRUE)
  
  senator_dist_r = dist(vote_num_r, method = "euclidean")
  senator_mds_r = cmdscale(senator_dist_r, eig = TRUE)
  
  senator_variance_d = senator_mds_d$eig[1]/sum(senator_mds_d$eig)
  senator_variance_r = senator_mds_r$eig[1]/sum(senator_mds_r$eig)
  variance = as.data.frame(c(senator_variance_d, senator_variance_r))
}


mds_1989 = mds_var(votes_1989, members_1989)
mds_1990 = mds_var(votes_1990, members_1990)
mds_1991 = mds_var(votes_1991, members_1991)
mds_1992 = mds_var(votes_1992, members_1992)
mds_1993 = mds_var(votes_1993, members_1993)
mds_1994 = mds_var(votes_1994, members_1994)
mds_1995 = mds_var(votes_1995, members_1995)
mds_1996 = mds_var(votes_1996, members_1996)
mds_1997 = mds_var(votes_1997, members_1997)
mds_1998 = mds_var(votes_1998, members_1998)
mds_1999 = mds_var(votes_1999, members_1999)
mds_2000 = mds_var(votes_2000, members_2000)
mds_2001 = mds_var(votes_2001, members_2001)
mds_2002 = mds_var(votes_2002, members_2002)
mds_2003 = mds_var(votes_2003, members_2003)
mds_2004 = mds_var(votes_2004, members_2004)
mds_2005 = mds_var(votes_2005, members_2005)
mds_2006 = mds_var(votes_2006, members_2006)
mds_2007 = mds_var(votes_2007, members_2007)
mds_2008 = mds_var(votes_2008, members_2008)
mds_2009 = mds_var(votes_2009, members_2009)
mds_2010 = mds_var(votes_2010, members_2010)
mds_2011 = mds_var(votes_2011, members_2011)
mds_2012 = mds_var(votes_2012, members_2012)
mds_2013 = mds_var(votes_2013, members_2013)
mds_2014 = mds_var(votes_2014, members_2014)


democrat = c(mds_1989[1,],mds_1990[1,],mds_1991[1,],mds_1992[1,],mds_1993[1,],mds_1994[1,],mds_1995[1,],mds_1996[1,],mds_1997[1,],
             mds_1998[1,], mds_1999[1,], mds_2000[1,], mds_2001[1,], mds_2002[1,], mds_2003[1,],mds_2004[1,],mds_2005[1,],mds_2006[1,],
             mds_2007[1,], mds_2008[1,], mds_2009[1,], mds_2010[1,], mds_2011[1,], mds_2012[1,], mds_2013[1,], mds_2014[1,])

democrat= as.data.frame(democrat)
democrat$year = c(1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

republic = c(mds_1989[2,],mds_1990[2,],mds_1991[2,],mds_1992[2,],mds_1993[2,],mds_1994[2,],mds_1995[2,],mds_1996[2,],mds_1997[2,],
             mds_1998[2,], mds_1999[2,], mds_2000[2,], mds_2001[2,], mds_2002[2,], mds_2003[2,],mds_2004[2,],mds_2005[2,],mds_2006[2,],
             mds_2007[2,], mds_2008[2,], mds_2009[2,], mds_2010[2,], mds_2011[2,], mds_2012[2,], mds_2013[2,], mds_2014[2,])

republic= as.data.frame(republic)
republic$year = c(1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

colors = c("democrat distance" = "red", "republic distance" = "blue")
#plot for variance of democrats and republicans over the years
ggplot() + geom_line(aes(x = year, y = democrat, color = "democrat distance"),  data = democrat) + geom_point(aes(x = year, y = democrat, color = "democrat distance"),  data = democrat) +
  geom_line(aes(x = year, y = republic, color = "republic distance"),  data = republic) + geom_point(aes(x = year, y = republic, color = "republic distance"),  data = republic) + 
  labs(x = "Year",
       y = "Explained variance on first principal axis",
       title = "Variance of democratic and republican party for years 1989-2014",
       color = "Legend") +
  scale_color_manual(values = colors)


diff = as.data.frame(republic$republic - democrat$democrat)
names(diff)[1] = "diff_dist"
diff$year = republic$year
ggplot(aes(y = diff_dist, x = year), data = diff) + geom_line(aes(y = diff_dist, x = year), color = "green", data = diff) + geom_point(aes(y = diff_dist, x = year), color = "black", data = diff) + geom_smooth(method = "loess") + 
  ggtitle("Difference between variances of both the parties for years 1989-2014") + xlab("Year") + ylab("Difference of variance between republican and democratic party")

#-------------------------------------------Section 3--------------------------------------------#
mds_analysis = function(vote, member){
  
  # Step 1
  joined = join(member, vote, by = "id")
  members = joined[,1:6]
  votes = joined[,(-1:-6)]
  
  # Step 2
  vote_num = apply(votes, 1:2, recode_votes)
  
  senator_dist = dist(vote_num, method = "euclidean")
  senator_mds = cmdscale(senator_dist, eig = TRUE)
  
  senator_points = senator_mds$points[,1:2] %>% as_tibble %>%
    setNames(paste0("MDS", 1:2)) %>% mutate(
      name = rownames(vote_num))
  joined$mds = senator_points$MDS1
  # mean_all = joined %>% group_by(party) %>% summarize(mean(mds1))
  joined
}

mds_mean = function(vote, member){
  m = mds_analysis(vote, member)
  t = tapply(m$mds, m$party, mean, na.rm = T)
  if (t[1]>0){
    t = -1*t
  }
  t
}

mds_1989 = mds_mean(votes_1989, members_1989)
mds_1990 = mds_mean(votes_1990, members_1990)
mds_1991 = mds_mean(votes_1991, members_1991)
mds_1992 = mds_mean(votes_1992, members_1992)
mds_1993 = mds_mean(votes_1993, members_1993)
mds_1994 = mds_mean(votes_1994, members_1994)
mds_1995 = mds_mean(votes_1995, members_1995)
mds_1996 = mds_mean(votes_1996, members_1996)
mds_1997 = mds_mean(votes_1997, members_1997)
mds_1998 = mds_mean(votes_1998, members_1998)
mds_1999 = mds_mean(votes_1999, members_1999)
mds_2000 = mds_mean(votes_2000, members_2000)
mds_2001 = mds_mean(votes_2001, members_2001)
mds_2002 = mds_mean(votes_2002, members_2002)
mds_2003 = mds_mean(votes_2003, members_2003)
mds_2004 = mds_mean(votes_2004, members_2004)
mds_2005 = mds_mean(votes_2005, members_2005)
mds_2006 = mds_mean(votes_2006, members_2006)
mds_2007 = mds_mean(votes_2007, members_2007)
mds_2008 = mds_mean(votes_2008, members_2008)
mds_2009 = mds_mean(votes_2009, members_2009)
mds_2010 = mds_mean(votes_2010, members_2010)
mds_2011 = mds_mean(votes_2011, members_2011)
mds_2012 = mds_mean(votes_2012, members_2012)
mds_2013 = mds_mean(votes_2013, members_2013)
mds_2014 = mds_mean(votes_2014, members_2014)

democrat = c(mds_1989[1],mds_1990[1],mds_1991[1],mds_1992[1],mds_1993[1],mds_1994[1],mds_1995[1],mds_1996[1],mds_1997[1],
             mds_1998[1], mds_1999[1], mds_2000[1], mds_2001[1], mds_2002[1], mds_2003[1],mds_2004[1],mds_2005[1],mds_2006[1],
             mds_2007[1], mds_2008[1], mds_2009[1], mds_2010[1], mds_2011[1], mds_2012[1], mds_2013[1], mds_2014[1])

democrat= as.data.frame(democrat)
democrat$year = c(1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

republic = c(mds_1989[length(mds_1989)],mds_1990[length(mds_1990)],mds_1991[length(mds_1991)],mds_1992[length(mds_1992)],mds_1993[length(mds_1993)],mds_1994[length(mds_1994)],mds_1995[length(mds_1995)],mds_1996[length(mds_1996)],mds_1997[length(mds_1997)],
             mds_1998[length(mds_1998)], mds_1999[length(mds_1999)], mds_2000[length(mds_2000)], mds_2001[length(mds_2001)], mds_2002[length(mds_2002)], mds_2003[length(mds_2003)],mds_2004[length(mds_2004)],mds_2005[length(mds_2005)],mds_2006[length(mds_2006)],
             mds_2007[length(mds_2007)], mds_2008[length(mds_2008)], mds_2009[length(mds_2009)], mds_2010[length(mds_2010)], mds_2011[length(mds_2011)], mds_2012[length(mds_2012)], mds_2013[length(mds_2013)], mds_2014[length(mds_2014)])

republic= as.data.frame(republic)
republic$year = c(1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

members = c(members_1989$display_name, members_1990$display_name, members_1991$display_name, members_1992$display_name, members_1993$display_name, members_1994$display_name, members_1995$display_name, members_1996$display_name, members_1997$display_name, members_1998$display_name,
            members_1999$display_name, members_2000$display_name, members_2001$display_name, members_2002$display_name, members_2003$display_name, members_2004$display_name, members_2005$display_name, members_2006$display_name, members_2007$display_name, members_2008$display_name, 
            members_2009$display_name, members_2010$display_name, members_2011$display_name, members_2012$display_name, members_2013$display_name, members_2014$display_name)
max(members)

mds_1989 = mds_analysis(votes_1989, members_1989)
mds_1990 = mds_analysis(votes_1990, members_1990)
mds_1991 = mds_analysis(votes_1991, members_1991)
mds_1992 = mds_analysis(votes_1992, members_1992)
mds_1993 = mds_analysis(votes_1993, members_1993)
mds_1994 = mds_analysis(votes_1994, members_1994)
mds_1995 = mds_analysis(votes_1995, members_1995)
mds_1996 = mds_analysis(votes_1996, members_1996)
mds_1997 = mds_analysis(votes_1997, members_1997)
mds_1998 = mds_analysis(votes_1998, members_1998)
mds_1999 = mds_analysis(votes_1999, members_1999)
mds_2000 = mds_analysis(votes_2000, members_2000)
mds_2001 = mds_analysis(votes_2001, members_2001)
mds_2002 = mds_analysis(votes_2002, members_2002)
mds_2003 = mds_analysis(votes_2003, members_2003)
mds_2004 = mds_analysis(votes_2004, members_2004)
mds_2005 = mds_analysis(votes_2005, members_2005)
mds_2006 = mds_analysis(votes_2006, members_2006)
mds_2007 = mds_analysis(votes_2007, members_2007)
mds_2008 = mds_analysis(votes_2008, members_2008)
mds_2009 = mds_analysis(votes_2009, members_2009)
mds_2010 = mds_analysis(votes_2010, members_2010)
mds_2011 = mds_analysis(votes_2011, members_2011)
mds_2012 = mds_analysis(votes_2012, members_2012)
mds_2013 = mds_analysis(votes_2013, members_2013)
mds_2014 = mds_analysis(votes_2014, members_2014)

dist_Wyden = as.data.frame(c(mds_1996$mds[mds_1996$display_name == "Wyden (D-OR)"],
                             mds_1997$mds[mds_1997$display_name == "Wyden (D-OR)"],
                             mds_1998$mds[mds_1998$display_name == "Wyden (D-OR)"],
                             mds_1999$mds[mds_1999$display_name == "Wyden (D-OR)"],
                             mds_2000$mds[mds_2000$display_name == "Wyden (D-OR)"],
                             mds_2001$mds[mds_2001$display_name == "Wyden (D-OR)"],
                             mds_2002$mds[mds_2002$display_name == "Wyden (D-OR)"],
                             mds_2003$mds[mds_2003$display_name == "Wyden (D-OR)"],
                             mds_2004$mds[mds_2004$display_name == "Wyden (D-OR)"],
                             mds_2005$mds[mds_2005$display_name == "Wyden (D-OR)"],
                             mds_2006$mds[mds_2006$display_name == "Wyden (D-OR)"],
                             mds_2007$mds[mds_2007$display_name == "Wyden (D-OR)"],
                             mds_2008$mds[mds_2008$display_name == "Wyden (D-OR)"],
                             mds_2009$mds[mds_2009$display_name == "Wyden (D-OR)"],
                             mds_2010$mds[mds_2010$display_name == "Wyden (D-OR)"],
                             mds_2011$mds[mds_2011$display_name == "Wyden (D-OR)"],
                             mds_2012$mds[mds_2012$display_name == "Wyden (D-OR)"],
                             mds_2013$mds[mds_2013$display_name == "Wyden (D-OR)"],
                             mds_2014$mds[mds_2014$display_name == "Wyden (D-OR)"]))

names(dist_Wyden)[1] = "dist"
dist_Wyden$year = c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
dist_Wyden$dist = ifelse(dist_Wyden$dist > 0, -1*dist_Wyden$dist, dist_Wyden$dist)


colors = c("democrat distance" = "pink", "republic distance" = "green", "wyden distance" = "red")
#plot for distances of democrats and republicans over the years
ggplot() + geom_line(aes(x = year, y = democrat, color = "democrat distance"),  data = democrat) + 
  geom_line(aes(x = year, y = republic, color = "republic distance"),  data = republic) + 
  geom_point(aes(x = year, y = dist, color = "wyden distance"),  data = dist_Wyden) +
  labs(x = "Year",
       y = "MDS distance on first principal axis",
       title = "MDS of principal axis of Senator compared with average distance of both parties",
       color = "Legend") +
  scale_color_manual(values = colors)

difference_senator_with_same_party = diff = as.data.frame(dist_Wyden$dist - democrat$democrat[8:26])
names(difference_senator_with_same_party)[1] = "diff_dist_senator"
difference_senator_with_same_party$year = dist_Wyden$year
ggplot(aes(y = diff_dist_senator, x = year), data = difference_senator_with_same_party ) + geom_line(aes(y = diff_dist_senator, x = year), color = "green", data = difference_senator_with_same_party) + geom_point(aes(y = diff_dist_senator, x = year), color = "black", data = difference_senator_with_party) + geom_smooth(method = "loess") + 
  labs(x = "Year",
       y = "Difference of MDS distance on principal axis",
       title = "Difference of MDS distance of senator and average distance of his party",
  )

difference_senator_with_diff_party = diff = as.data.frame(dist_Wyden$dist - republic$republic[8:26])
names(difference_senator_with_diff_party)[1] = "diff_dist_senator"
difference_senator_with_diff_party$year = dist_Wyden$year
ggplot(aes(y = diff_dist_senator, x = year), data = difference_senator_with_diff_party ) + geom_line(aes(y = diff_dist_senator, x = year), color = "green", data = difference_senator_with_diff_party) + geom_point(aes(y = diff_dist_senator, x = year), color = "black", data = difference_senator_with_diff_party) + geom_smooth(method = "loess") + 
  labs(x = "Year",
       y = "Difference of MDS distance on principal axis",
       title = "Difference of MDS distance of senator and average distance of other party",
  )