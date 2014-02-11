## Amnesty International, Urgent Action (aiua) analysis
## Testing linear/logistic regression models with regularization (lasso) for predicting Urgent Action *follow-up*
---
### aiua_categories.R: 
- Summarizes urgent action *category* incidence with wordcloud and category incidence
- i.e. shows overall frequencies of categories

### aiua_logreg_fu_lasso.R
- Given a category e.g. legal concern
	- Builds Corpus + DocumentTermMatrix for all UAs of given category
	- Runs logistic regression with 10-fold cross-validation to choose lambda (alpha=1 -> lasso) 
	- Performance: mis-classification of whether an initial UA was followed up (<- requested outcome by AI)
    - e.g. Category 'Freedom of Expression': miss-classification : 0.21 on test data (all ~ 0.12 w/ stemming)
- Output into figures/{category_name}
