Exploration of Seattle Public Library Checkout Data
=======================

Summary
-------------
Public libraries have done great work to stay relevant even as information and books have become more digitally available. But how have our reading habits changed? Using datasets provided by the Seattle Public Library (SPL) of physical and digital items checked out each month since January 2006, I explored changes in item types and genres most checked out over time. The datasets provided were sourced from the library system as well as multiple media vendors for digital items, which introduced variances in title and author names. 

An interactive shiny app was created to visualize yearly and monthly trends in checkouts by usage class or material type and to explore the top book (physical/ebook/audio) checkouts per year by usage class with their subjects visualized in a word cloud. Best viewed in fullscreen mode.

![Capture1](https://user-images.githubusercontent.com/42395526/57702126-0321f200-7623-11e9-9ed9-e06fd12ce50b.PNG)

![image](https://user-images.githubusercontent.com/42395526/57702006-bfc78380-7622-11e9-917f-cd19bbf46294.png)





Motivation
-------------
A love of libraries and reading have long been a part of my life. As time has gone by, I have seen my own interactions at my local library and the types of books I check out change—primarily due to time constraints and accessibility. In conversations with family and friends, they also seem to be utilizing ebooks and audiobooks with greater frequency. With data available from a large public library, I was curious to see if these trends are also reflected in what is actually checked out from the Seattle Public Library.

Data Question
-------------
What changes can we see in reading materials checked out from the Seattle Public Library since January 2006 in terms of media type (physical, ebook, audiobook) as well as subject matter/genre? 

How some others have explored this data:
1.	https://www.kaggle.com/seattle-public-library/seattle-library-checkout-records/
2.	https://steveriffe.co.uk/seattle-public-library
3.	https://towardsdatascience.com/how-and-when-people-use-the-public-library-1b102f58fd8a


Data Sources
------------
I used the publicly available data for Seattle Public Library’s checkouts (available starting April 2005) provided through Seattle’s Open Data program. 

* Checkouts by Title: includes physical and electronic items and is aggregated by month. (updated 1/06/2019)
https://data.seattle.gov/Community/Checkouts-by-Title/tmmm-ytt6

* FAQ: https://data.seattle.gov/api/views/tmmm-ytt6/files/d37b9edc-c56f-46e4-aaea-cb882230cf3a?download=true&filename=Checkouts%20by%20Title%20FAQs.pdf
