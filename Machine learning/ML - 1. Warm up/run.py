import numpy  as np
import pandas as pd

from sklearn.model_selection import train_test_split, RandomizedSearchCV

#classifiers
from sklearn.ensemble  import RandomForestClassifier, ExtraTreesClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC, LinearSVC

#scores
from sklearn.metrics import accuracy_score, make_scorer

#selectors
from sklearn.feature_selection import SelectKBest, SelectFromModel, RFECV
#select strategies
from sklearn.feature_selection import chi2, f_classif, mutual_info_classif

np.random.seed (163)

def write_to_file (classification):
	pd.DataFrame (data = classification, index = range (1, 6634, 2), columns = ['class'])\
			   . to_csv ("answer.csv", header = True, index_label = 'id')

labels = ['A', 'B', 'C', 'D', 'E', 'F', 'G']
translate = dict (zip (labels, range (len (labels))))
			   
#testing part
test_input = pd.read_csv ("./input/test.csv", index_col=0)
test_input = test_input.values [:, :27]

for i in range (0, test_input.shape [0]):
    for j in range (20, test_input.shape [1]):
        test_input [i, j] = translate [test_input [i, j]]
			   
#training part
train_input = pd.read_csv ("./input/train.csv", index_col = 0)

train_data   = train_input.values [:, :27]
train_target = train_input.values [:, -1]

for i in range (0, train_data.shape [0]):
    for j in range (20, train_data.shape [1]):
        train_data [i, j] = translate [train_data [i, j]]

train_data, test_data, train_target, test_target =\
	train_test_split (train_data, train_target, test_size = 0.2)

classifiers = [
	{
		"name": "K Nearest Neighbors",
		"instance": KNeighborsClassifier,
		"parameters": {
			"algorithm": ["auto", "ball_tree", "kd_tree", "brute"],
			"metric": ["cityblock", "euclidean", "l1", "l2", "manhattan", "minkowski"],
			"n_neighbors": range (2, 15),
			"leaf_size": range (5, 50)
		}
	}, {
		"name": "Random Forest",
		"instance": RandomForestClassifier,
		"parameters": {
			"n_estimators": range (10, 100, 10),
			"criterion": ["gini", "entropy"],
			"min_samples_split": range (2, 20)
		}
	}, {
		"name": "Support Vectors",
		"instance": SVC,
		"parameters": {
			"kernel": ["linear", "rbf", "poly", "sigmoid"],
			"shrinking": [True, False],
			"degree": range (1, 5)
		}
	}
]

scorer = make_scorer (accuracy_score)

print ("Preparing feature selectors")
feature_selectors = []

print ("... based on `Recursive Feature Elimination`")
kernels = ["linear"]

for i in kernels:
	try:
		name = "`Recursve Feature Elimination` selector (estimator=SVC, kernel=" + i + ")"
		classifier = SVC (kernel = i, gamma = "auto")
		instance = RFECV (estimator = classifier, cv = 10)\
				 . fit (train_data, train_target)
		feature_selectors += [{"name": name, "instance": instance}]
	except RuntimeError:
		pass

print ("... based on `K Best`")
k_best_select_strategies = [
	{"name": "Chi-squared",        "instance": chi2}, 
	{"name": "F-value",            "instance": f_classif}, 
	{"name": "Mutual information", "instance": mutual_info_classif}
]

for i in range (1, 20 + 1, 1):
	for j in range (len (k_best_select_strategies)):
		strategy = k_best_select_strategies [j]
		name = "`K Best` selector (strategy=" + strategy ['name'] + ", k=" + str(i) + ")"
		try:
			instance = SelectKBest (k = i).fit (train_data, train_target)
			feature_selectors += [{"name": name, "instance": instance}]
		except ValueError:
			pass
			
print ("... based on `Extra Trees Classifier`")
for i in range (50, 100 + 1, 5):
	name = "Selector of model `Extra Trees Classifier` (n_estimators=" + str (i) + ")"
	classifier = ExtraTreesClassifier (n_estimators = i).fit (train_data, train_target)
	instance = SelectFromModel (classifier, prefit = True)
	feature_selectors += [{"name": name, "instance": instance}]

print ("\nPrepare for processing learning")
best_set = [0, None, None]	
for i in range (len (classifiers)):
	for j in range (len (feature_selectors)):
		classifier, selector = classifiers [i], feature_selectors [j]
		print ("Processing (classifier=" + classifier ['name']\
			+ ", feature_selector=" + selector ['name'] + ") ")
			
		prepared_train_data = selector ['instance'].transform (train_data)
		configuration = RandomizedSearchCV (classifier ['instance'] (), classifier ['parameters'], 
											scoring = scorer, cv = 5)\
					  . fit (prepared_train_data, train_target)
		
		scores = scorer (configuration, prepared_train_data, train_target)
		
		prepared_test_data = selector ['instance'].transform (test_data)
		test_score = scorer (configuration, prepared_test_data, test_target)
		
		print ("  Result " + str (test_score) + " scores (best=" + str (best_set [0]) + ")")
		if test_score > best_set [0]:
			print ("  Configuration is used as new best one")
			best_set = [test_score, classifier, selector]
			
			prepared_test_data = selector ['instance'].transform (test_input)
			write_to_file (configuration.predict (prepared_test_data))
			
		print ("")
		
		"""
		folder = RepeatedKFold (n_splits = 10, n_repeats = 3)
		for data_index, target_ind in folder.split (train_data):
			data_train, target_train, data_check, target_check =\
				train_data[data_index], train_target[data_index], train_data[target_ind], train_target[target_ind]
			classifier.fit (selector.transform (data_train), target_train)
		"""

if (best_set [0] == 0):
	print ("Best configuration not found")
	exit ()
	
print ("Best configuration found (score=" + str (best_set [0]) + ", classifier=" + best_set [1]['name']\
	+ ", selector=" + best_set [2]['name'] + ")")