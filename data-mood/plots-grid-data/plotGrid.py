#!/usr/bin/python
# -*- coding: utf-8 -*-

from collections import defaultdict

import matplotlib.pyplot as plt


#### settings: indicore come dovrà essere formata la griglia sulla quale
#### verranno plottati i dati di ogni singolo partecipante
#### ad esempio, se ho 19 sogetti potrei plottare su 5 righe e 4 colonne
rows = 8
columns = 8

plt.rcParams['figure.figsize'] = (columns * 4, rows * 4)

participants_data = defaultdict(lambda: defaultdict(dict))

with open("mood.csv", "rb") as infile:
    infile.next()   # skip the header
    for line in infile:
        sid, group, pleasure_baseline, pleasure_post, arousal_baseline, arousal_post = line.strip().split(",")

        participants_data[group][int(sid)]["arousal_baseline"] = int(arousal_baseline)
        participants_data[group][int(sid)]["pleasure_baseline"] = int(pleasure_baseline)
        participants_data[group][int(sid)]["arousal_post"] = int(arousal_post)
        participants_data[group][int(sid)]["pleasure_post"] = int(pleasure_post)
        

group2coords = {
        "sad" : [-5, 0, 0, 0.5],
        "excited" : [0, 5, 0.5, 1]}

for group, subs_data in sorted(participants_data.items()):
    for si, (subject_id, sdata) in enumerate(sorted(subs_data.items())):
        plt.subplot(rows, columns, si + 1)

        # plottiamo la baseline
        plt.plot(sdata["pleasure_baseline"], sdata["arousal_baseline"], color = "g",  marker = "s")

        # plottiamo il post
        plt.plot(sdata["pleasure_post"], sdata["arousal_post"], color = "r",  marker = "^")
        
        plt.grid(True, ls = ":")

        plt.axvspan(*group2coords[group], facecolor = '0.8', alpha = 0.5)

        plt.xticks(range(-4,5)) 
        plt.yticks(range(-4,5)) 
        
        plt.xlim(-4.5, 4.5) 
        plt.ylim(-4.5, 4.5) 
        
        plt.axhline(y = 0, c = "k", lw = 1)
        plt.axvline(x = 0, c = "k", lw = 1)
        
        plt.xlabel("pleasure")
        plt.ylabel("arousal")
        
        plt.title('Subject id: {0}\nCondition: {1}'.format(str(subject_id), str(group)))
    
    plt.tight_layout()
    
    plt.savefig("plot_" + group + ".pdf")
    plt.show()