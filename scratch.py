import csv

def createList(r1, r2):
    return [item for item in range(r1, r2+1)]

def setFilePath(path): #function oriented version of tool in progress
    return (path)

file = setFilePath(r'C:\Users\lmich\Desktop\density.csv')
out_file = setFilePath(r'C:\Users\lmich\Desktop\density_new.csv')

i = 0
col = createList(5,62)
years = createList(1961,2018)
out = []

with open(file, 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    for r in csv_reader:
        if i == 0:
            pass
        else:
            for n in col:
                if len(r[n]) == 0:
                    print (r[n])
                else:
                    out = out + [[r[0], years[n-5],r[n]]]

        i = i=1

    out = [['country', 'year','population_density']] + out

    with open(out_file, 'w') as new_file:
        csv_writer = csv.writer(new_file,delimiter=',')

        for r in out:
            csv_writer.writerow(r)