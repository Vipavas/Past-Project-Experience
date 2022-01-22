#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 27 21:59:18 2021

@author: kia
"""

from collections import defaultdict
import docx2txt
import io


class text:
    def __init__(self,string):
        self.string = string
        self.counter = 0
        self.temp = []
        self.temp2 = []
        
    def counttotal(self,num):
        for i in self.string:
            if i == num:
                self.counter += 1
        return self.counter
    
    def input1(self, inputstring, counter = 0):
        for i in inputstring:
            if i == '|':
                counter += 1
                
        # single(This algorithm below is used to find the prob of each given word)
        letter_list = inputstring.split("|")
        key = range(1,len(letter_list),1)
        prob = []
        temp = []
        temp2 = []
        
        for i in range(len(letter_list)):
            sb = '|' + letter_list[i] + '|'
            temp.append(sb)
        # we count each word and divide it by the number of the all words in corpus to find the prob of each word
            word_count = self.string.count(sb)
            prob.append(word_count/self.counter)
            sb2 = 'word(' +"\"" + letter_list[i] +"\""  + ',' + str(word_count/self.counter)+')'
            self.temp.append(sb2)    
            
        d = defaultdict(dict)
        for x,y,z in zip(key,letter_list,prob):
            d[x][y] = z
            
        d_dict = dict(d)
        
        # double(This algorithm below is used to find the prob of word sequence)
        wordlist_set = []
        prob2 = []
        for i in range(len(letter_list)-1): 
            if i != 0:
        # we find the joint prob of the word sequence by counting the adjacent word for ex. ประชาชน|ทุก and ทุก|ประชาชน and divided by the number of previous word
                word_search = '|' + letter_list[i-1] +'|' + letter_list[i] + '|'
                # word_sb is word search backward
                word_sb = '|' + letter_list[i] + '|' + letter_list[i-1] + '|'
                wordlist_set.append(letter_list[i-1] +',' + letter_list[i])
                
                word_count_1 = self.string.count(word_search)
                word_count_2 = self.string.count(word_sb)
                prob2.append((word_count_1+word_count_2)/
                                self.string.count(temp[i-1]))
                
                sb3 = 'pair(' +'\"'  + letter_list[i-1] +'\"' + ',' +'\"' + letter_list[i] + '\"' + ',' + str((word_count_1+word_count_2)/self.string.count(temp[i-1])) + ')'
                self.temp2.append(sb3)
        
        
        d2 = defaultdict(dict)
        for x,y,z in zip(key, wordlist_set, prob2):
            d2[x][y] = z
            
        d2_dict = dict(d2)
        
        
        return print(d_dict), print(d2_dict)
    
    def get_item(self):
        return self.temp, self.temp2
    

# we print the total number of word counting and the prob of bigram and word sequence, also we force the program to generate the result in .txt file for using in Prolog
text1 = docx2txt.process("CorpusTest.docx")         
initial1 = text(text1)         
initial_total = initial1.counttotal('|')

print(initial_total)
initial1.input1('เก็บ|ข้อมูล|จาก|แหล่ง|ที่|มี|')
item, item2 = initial1.get_item()
item = item[:len(item)-1]
    
with open('word.txt', 'w', encoding = 'utf-8') as f:
    for item in item:
        f.write("%s.\n" % item)
  
with open('word.txt', 'a', encoding = 'utf-8') as f:
    f.write("  \n")
    for item in item2:
        f.write("%s.\n" % item)