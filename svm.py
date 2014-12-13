from sklearn import svm
import csv

Y = []
X = []
with open('threshold.csv','rU') as file:
        reader = csv.reader(file)
        rowNum = 0
        next(reader, None)
        for row in reader:
                Y.append(float(row[0]))
                X.append([float(row[11]), float(row[12]), float(row[13]), float(row[14]), \
                float(row[15]), float(row[16]), float(row[17])])

clf = svm.SVC()
clf.fit(X,Y)



print('done')
