# Hacker News API Test

## Overview
The goal of this project is to print the top 30 stories from Hacker News and display 
the top 10 commenter names of these stories with the total number of comments that they posted (only for these 30 stories)

## How to use it
Running the main function in Main.hs will return the top 30 stories along with their top 10 commenters that will be displayed with the total number of comments that they posted (only for these 30 stories)

## Possible optimization
With my current level, I was not able to code two functionnalities the way I would have liked to do it.
-  For getTitlesFromItems, getCommentsFromItems, and getAuthorsFromComments, a map solution can be implemented I thinnk to automatically get the element of Item more properly.
-  The solution to access every level of comments can be optimized in one function in my opinion instead of duplicating several lines, but i do not see how.
