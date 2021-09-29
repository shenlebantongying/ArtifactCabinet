default:
	bison -d r2.y
	flex r2.l 
	gcc -lm lex.yy.c r2.tab.c -o  r2

dist: default clean

clean:
	rm -f r2.tab.c r2.tab.h y.tab.h y.tab.c lex.yy.c
 