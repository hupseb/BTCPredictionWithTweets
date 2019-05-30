'''
Created on Sep 16, 2018
@author: Sebastian Huppmann
'''
import nltk
from nltk.stem.lancaster import LancasterStemmer
import numpy as np
import tflearn
import tensorflow as tf
import random
import json
import unicodedata
import sys

# a table structure to hold the different punctuation used
tbl = dict.fromkeys(i for i in range(sys.maxunicode)
                    if unicodedata.category(chr(i)).startswith('P'))

# method to remove punctuations from sentences.
def remove_punctuation(text):
    return text.translate(tbl)

# initialize the stemmer
stemmer = LancasterStemmer()
# variable to hold the Json data read from the file
data = None

# read the json file and load the training data
with open('data_utf8.json', encoding='utf-8') as json_data:
    data = json.load(json_data)

# get a list of all categories to train for
#categories = list(data.keys())
categories = ['positive','negative']
words = []
# a list of tuples with words in the sentence and category name
docs = []

for twt in data:
     # remove any punctuation from the sentence
    tweet = ""
    tweet = remove_punctuation(twt["tweet"])
    # extract words from each sentence and append to the word list
    w = nltk.word_tokenize(tweet)
    words.extend(w)
    docs.append((w, twt["sentiment"]))
    
# stem and lower each word and remove duplicates
words = [stemmer.stem(w.lower()) for w in words]
words = sorted(list(set(words)))

# create our training data
training = []
output = []
# create an empty array for our output
output_empty = [0] * len(categories)

for doc in docs:
    # initialize our bag of words(bow) for each document in the list
    bow = []
    # list of tokenized words for the pattern
    token_words = doc[0]
    # stem each word
    token_words = [stemmer.stem(word.lower()) for word in token_words]
    # create our bag of words array
    for w in words:
        bow.append(1) if w in token_words else bow.append(0)

    output_row = list(output_empty)
    output_row[categories.index(doc[1])] = 1

    # our training set will contain a the bag of words model and the output row that tells
    # which catefory that bow belongs to.
    training.append([bow, output_row])

# shuffle our features and turn into np.array as tensorflow  takes in numpy array
random.shuffle(training)
training = np.array(training)

# trainX contains the Bag of words and train_y contains the label/ category
train_x = list(training[:, 0])
train_y = list(training[:, 1])

# reset underlying graph data
tf.reset_default_graph()
# Build neural network
net = tflearn.input_data(shape=[None, len(train_x[0])])
net = tflearn.fully_connected(net, 8)
net = tflearn.fully_connected(net, 8)
net = tflearn.fully_connected(net, len(train_y[0]), activation='softmax')
net = tflearn.regression(net)

# Define model and setup tensorboard
model = tflearn.DNN(net, tensorboard_dir='C:/logs')
# Start training (apply gradient descent algorithm)
model.fit(train_x, train_y, n_epoch=3000, batch_size=4, show_metric=True)
model.save('model.tflearn')
#model.load('./model.tflearn')

# a method that takes in a sentence and list of all words
# and returns the data in a form the can be fed to tensorflow
def get_tf_record(sentence):
    global words
    # tokenize the pattern
    sentence_words = nltk.word_tokenize(sentence)
    # stem each word
    sentence_words = [stemmer.stem(word.lower()) for word in sentence_words]
    # bag of words
    bow = [0]*len(words)
    for s in sentence_words:
        for i, w in enumerate(words):
            if w == s:
                bow[i] = 1

    return(np.array(bow))

# read the json file and load the training data
with open('pos_310_66_test.json', encoding='utf-8') as json_data:
    testsetpos = json.load(json_data)

print("---pos test set---")    
for twttest in testsetpos:
    print(categories[np.argmax(model.predict([get_tf_record(twttest["tweet"])]))])

# read the json file and load the training data
with open('neg_310_66_test.json', encoding='utf-8') as json_data:
    testsetneg = json.load(json_data)

print("---neg test set---")    
for twttest in testsetneg:
    print(categories[np.argmax(model.predict([get_tf_record(twttest["tweet"])]))])