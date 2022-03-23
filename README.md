# Hacker News API Test

## Overview
The goal of this project is to print the top 30 stories from Hacker News and display 
the top 10 commenter names of these stories with the total number of comments that they posted (only for these 30 stories)

## How to use it
Running the main function in Main.hs will return the top 30 stories along with their top 10 commenters that will be displayed with the total number of comments that they posted (only for these 30 stories)

## Possible optimization
On the 23/03/2022, the code goes through the direct comments of top stories (commented directly on the story) and their sub comments (commented on the direct comment). However, the comments on an inferior level (comments of sub comments) are not taken into account because I don't see how it would be possible for the moment to do this and keep the algorithm efficient.
