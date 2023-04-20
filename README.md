# Sentiment Analysis over Ukraine vs Russian War

The dataset I used was publicly published from Kaggle. 
Ukraine Conflict Twitter Dataset (Kaggle):
https://www.kaggle.com/datasets/bwandowando/ukraine-russian-crisis-twitter-dataset-1-2-m-rows

The main file I have used to analyze was: 
1. Russia_invade.csv

Data Descriptions:
Number of columns;29 and rows;170835. 
Columns are consisted with date, twitter id, content, number of likes, number of retweets and so on.

Before I investigate the dataset, I raised with a couple of questions:
1.	Could people predict the Ukraine-Russian War?
a.	Reliability of the sources/news
2.	Sentimental Analysis of the Ukraine and Russian pre-post war

Explanation of the analysis:
1. The reason I got this Twitter dataset was to analyze if the people could predict/expect the war to be happened before the war actually happened. Therefore, I filtered down the contents of Twitter that were posted before "2022-02-24" and found the most frequent words to see which topics are highly related to the war's occurance.
 
2. I filtered the content that contains the word “will invade/invasion/will be invading”. I counted the number of Tweets, how it incremented weekly before the war. Plus, I got the average of likes, retweets, quotes, reply counts and reviewed the flucuation of the rates to see how people were in an agreement to expect the war to be occured. 

3. I wanted to observe the sources/news the people were referring to each time they post the war-related tweets, so I got the top liked https and searched if they were reliable resources.

4. I calculated the sentiment analysis of people toward the Ukraine and Russia pre and post war to see how the war affected the people’s sentiment on both countries. Used NRC and Afinn packages to see the difference. 
