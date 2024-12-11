# CYO Project Submission for HarvardX PH125.9x - League of Legends Wins Prediction

The CYO (Choose Your Own) project is part of the HarvardX PH125.9x Capstone course.

For this project, we need to apply the knowledge gained in all previous course and use machine learning techniques that go beyond standard linear regression to analyze and predict variables from a publicly available dataset.

Our CYO project is based on the ["League of Legends Diamond Ranked Games (10 min)"](https://www.key2stats.com/data-set/view/1596) dataset available in the [Key2Stats](https://www.key2stats.com/) Learning platform. This dataset contains the first 10min. stats of approx. 10k ranked games (solo queue) from a high ELO (Diamond I to Master ). Players have roughly the same level. Each game is unique.

Our goal is to develop and train a machine learning model capable of predicting the wins for the blue team which are stored in the column blueWin of the dataset.

Several models will be trained on the dataset which contains the first 10 min of around 10,000 games played by elite players. We will evaluate 39 features and although the typical League of Legends game can last somewhere between 20 minutes to several hours, the first 10 minutes should give us enough insight on the winning strategy of players in the early phase of the game. The performance of the models chosen to be trained will be evaluated on their accuracy, sensitivity, specificity and precision. The best scoring model will be fine-tuned further in order to increase its accuracy

## Files included

1.  cyo.R includes the downloading and construction of the data sets, the prediction script that develops the model.

2.  cyo.Rmd contains the final report markdown code, text and figures.

3.  cyo.pdf is the final report.
