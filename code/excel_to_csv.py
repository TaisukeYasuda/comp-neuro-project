import sys
import pandas as pd
import os

if len(sys.argv) < 2:
    print "usage: python excel-to-csv.py <file.xlsx>"
    sys.exit()

data = pd.read_excel(sys.argv[1], sheetname=None)
(root, ext) = os.path.splitext(sys.argv[1])
root += "/"

if os.path.exists(root):
    print ("directory %s already exists" % root)
    sys.exit()
else:
    os.mkdir(root)
    print root

for filename in data:
    data[filename].to_csv(root + filename + ".csv")
    print ("\t%s.csv" % filename)

sys.exit()
