# Summary
## p01
Simple parallel without shared data
## p02 (Terrible)
Parallel sum with fixed number of threads. It is terrible, 1) Not all computers has that number of processors, 2) processors availability might change, 3) Data might not be able to be divided to equal parts

U can divide the array to more pieces, but Java's threads are not cheap.(that's why there is ProjectLoom-> https://wiki.openjdk.java.net/display/loom/Main)
#p03
Recursive Divide&Conquer with ForkJoin
#TODO: p04 -> generalize p03; 3.5

# Note:
## Other implementations
+ OpenMP
+ .Net -> Task Parallel Library 

# todo:

His book didn't tell how to debug parallel programs too...