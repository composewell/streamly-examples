#!/bin/bash

awk 'BEGIN {
  srand();
  while (1) {
    line="";
    for (i=0; i<30; i++) {
      len=int(1+(-log(rand())*5));
      if (len > 15) len=15;
      for (j=0; j<len; j++)
        line=line sprintf("%c", 97+int(rand()*26));
      line=line " ";
    }
    print line;
  }
}' | head -c 500M > random-word-lines.txt
