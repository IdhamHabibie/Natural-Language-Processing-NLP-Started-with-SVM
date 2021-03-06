# Import the Libraries
import seaborn as sns
import matplotlib.pyplot as plt

# Import the Dataset
train_data = pd.read_csv("train.csv")
test_data = pd.read_csv("test.csv")
sample = pd.read_csv("sample_submission.csv")

# Explarotory Data Analysis
def eda(input_data):
    print("The shape of this data = ", input_data.shape)
    print("The detail information of this data", input_data.describe())
    type(input_data)
    print("The 5 top : ", input_data.head(5))
    print("Information: ", input_data.info)
# Create the Values
eda(train_data)
eda(test_data)
eda(sample)

# Printing the Value Counts
print(train_data.target.value_counts())

# Visualization of the dataset
def visualize(input_data, id):
        # Reading the Dataset
        true  = 0 
        false = 0 
        for i in input_data.iloc[:,id]:
            if(i==1):
                true +=1
            else:
                false +=1
        
        X = ["True","False"]
        y = [true, false]
        # Analysis information
        chart = sns.barplot(x = X, y = y)
        chart.set_xticklabels(labels = X, rotation=45)
        chart.set_title("The Training Data : Total Number of True & False ")
        chart.set(ylabel = "The total number of true and false")
        chart.set(xlabel = "The X-Axis")
# Create the Values
visualize(train_data, 4)

# Text Mining in Python
j = ""
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
for i in train_data["text"]:
    j = j + i

# Data Cleansing & Normalizing
token = word_tokenize(j)
token = [x.lower() for x in token]  # Using sequential python for 
stopwords = set(stopwords.words('english'))
filtered = [x for x in token if not x in stopwords] # Remove Stopwords
filtered = [x for x in filtered if x.isalpha()] # Remove Punctuation

# Avoiding the Stemming
porter = PorterStemmer()
stemmed = [porter.stem(word) for word in filtered]

# Lemmatization
from nltk.stem import WordNetLemmatizer
wordnet_lemmatizer = WordNetLemmatizer()

lemmatize = [wordnet_lemmatizer.lemmatize(x) for x in stemmed]

# Importing FreqDist library from nltk and passing token into FreqDist
from nltk.probability import FreqDist
fdist = FreqDist(lemmatize)
check_data = fdist.most_common(100)[1:10]

# Convert the data into x and y
x, y = zip(*check_data)
print(x)
print(y)

# Passing the string text into word tokenize 
x = list(x)
y = list(y)
sns.barplot(x,y)
plt.title("The Most Frequent words")
plt.show()

# WordCloud Generation
from wordcloud import WordCloud
# Display the generated image:
str1 = "  "
str2 = str1.join(x)
wordcloud = WordCloud().generate(str2)
plt.imshow(wordcloud, interpolation='bilinear')
plt.axis("off")
plt.show()


# Using Count Vectorizer
from sklearn.feature_extraction.text import CountVectorizer
from nltk.tokenize import RegexpTokenizer
token = RegexpTokenizer(r'[a-zA-Z0-9]+')
cv = CountVectorizer(lowercase=True,stop_words='english',ngram_range = (1,1),tokenizer = token.tokenize)

# Implementing the Feature Extraction to both Training Data and Test Data
text_counts= cv.fit_transform(train_data['text']) # Obtained from the training data
testing= cv.transform(test_data['text']) # Obtained from the dataset

# Differentiate the Training and Testing
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(
    text_counts, train_data['target'], test_size=0.25, random_state=1)



# Fitting SVM to the Training set
from sklearn.svm import SVC
from sklearn import metrics
classifier = SVC(kernel = 'linear', random_state = 0)
classifier.fit(X_train, y_train)
predicted = classifier.predict(X_test)
print("SVC Accuracy:",metrics.accuracy_score(y_test, predicted))


 # To do Grid Search 
parameters = [{'C': [1, 10, 100], 'kernel': ['linear']},
               {'C': [1, 10, 100], 'kernel': ['rbf']}]

 # Applying Grid Search to find the best model and the best parameters
from sklearn.model_selection import GridSearchCV
grid_search = GridSearchCV(estimator = classifier, param_grid = parameters, scoring ='accuracy', cv = 10, n_jobs=-1 )
grid_search = grid_search.fit(X_train, y_train)

 # Finding the best SVC
print(grid_search.best_estimator_)
print(grid_search.best_params_)
print(grid_search.best_score_)

# Implementing the Evalulation Method
from sklearn.metrics import f1_score
f1 = f1_score(y_test, predicted)
print("The f1 score is : ", f1)


# finding the best prediction
predicted = classifier.predict(testing)
print(predicted)
print(type(predicted))

predicted = pd.DataFrame({'target' : predicted})
# Generate the values
print(predicted.target.value_counts())

X = ["True","False"]
y = [predicted.target.value_counts()[0], predicted.target.value_counts()[1]]

# Analysis information
chart = sns.barplot(x = X, y = y)
chart.set_xticklabels(labels = X, rotation=45)
chart.set_title("The Testing Data : Total Number of True & False ")
chart.set(ylabel = "The total number of true and false")
chart.set(xlabel = "True vs False in Testing Data")

# Deploy into the sample submission
print(predicted)
sample = pd.read_csv("sample_submission.csv")

sample.target = predicted
print(sample)