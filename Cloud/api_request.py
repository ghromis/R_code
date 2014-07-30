import requests
import json




with open('data.txt', 'w') as outfile:
    for i in range(1, 143):
        data = requests.get("https://api.angel.co/1/jobs?page="+str(i))
        outfile.write(data.text)

##with open('data.txt', 'w') as outfile:
##    data = requests.get("https://api.angel.co/1/jobs?page="+str(2))
##    outfile.write(data.text)

file = open("data.txt", 'r')
contents = file.read()
ind = -1
jobs = open("job_titles.txt", "w")
for e in contents:
    ind = ind +1
    ind = contents.find("title\"", ind)
    beg = ind+8
    end = contents.find('"', beg)
    if ind == -1:
        break
    jobs.write(contents[beg:end]+ "\n")
    print contents[beg:end]
jobs.close()
outfile.close()

