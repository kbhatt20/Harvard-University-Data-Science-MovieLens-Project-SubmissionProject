# Harvard-University-Data-Science-MovieLens-Project-SubmissionProject
Project about Harvard-University-Data-Science-MovieLens Project Submission

# Introduction
One of the major families of applications of machine learning in the information technology sector is the ability to make recommendations of items to potential users or customers. In year 2006, Netflix has offered a challenge to data science community. The challenge was to improve Netflix???s in house software by 10% and win $1M prize.

This capstone project is based on the winner???s team algorithm and is a part of the course HarvardX:??PH125.9x Data Science: Capstone project. The Netflix data is not freely available so an open source dataset from movieLens ‘10M version of the MovieLens dataset’??has been used. The aim of this project is to develop a machine learning algorithm using the inputs in one subset to predict movie ratings in the validation set. Several machine learning algorithm has been used and results have been compared to get maximum possible accuracy in prediction.

This report contains problem definition, data ingestion, exploratory analysis, modeling and data analysis, results and concluding remarks and have been written in that order.

# Problem Defnition
This capstone project on ’Movie recommendation system??? predicts the movie rating by a user based on users past rating of movies. The dataset used for this purpose can be found in the following links

[MovieLens 10M dataset] https://grouplens.org/datasets/movielens/10m/
[MovieLens 10M dataset - zip file] http://files.grouplens.org/datasets/movielens/ml-10m.zip
The challenge is not so easy given that there are many different type of biases present in the movie reviews. It can be different social, psychological, demographic variations that changes the taste of every single users for a given particular movie. However the problem can still be designed to tackle major biases which can be expressed via mathematical equations relatively easily. The idea here is to develop a model which can effectively predict movie recommendations for a given user without our judgement being impaired due to different biases. In the algorithm, the prevalences can be suppressed using some clever mathematical tricks. This will become clear as we follow this document.


# Harvard-University-Data-Science-MovieLens Project Submission - This project has three files:

1. A script in R format that generates your predicted movie ratings and RMSE score.
2. A report in R Markdown document. (Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents)
3. A report in pdf format which is generated from R Markdown document.
