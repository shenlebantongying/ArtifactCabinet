move(1,Fom_pole,To_pole,_) :-
    write('Move top disk from '),
    write(Fom_pole),
    write(' to '),
    write(To_pole),
    nl.
move(Height,Fom_pole,To_pole,With_pole) :-
    Height>1,
    M is Height-1,
    move(M,Fom_pole,With_pole,To_pole),
    move(1,Fom_pole,To_pole,_),
    move(M,With_pole,To_pole,Fom_pole).

/*
Usage:

?- move(3,a,b,c).

Move top disk from a to b
Move top disk from a to c
Move top disk from b to c
Move top disk from a to b
Move top disk from c to a
Move top disk from c to b
Move top disk from a to b

*/

/*
Equivalent to code in python

def move_tower(Height, from_pole, to_pole, with_pole):
    if Height >=1:
        print(Height,from_pole,to_pole, with_pole)
        move_tower(Height -1, from_pole, with_pole, to_pole)
        move_disk(from_pole, to_pole)
        move_tower(Height -1, with_pole, to_pole, from_pole)
        print("---")

def move_disk(fp,tp):
    print("moving disk from",fp,"to",tp)

move_tower(3, "A", "B", "C")

Output:
3 A B C
2 A C B
1 A B C
moving disk from A to B
---
moving disk from A to C
1 B C A
moving disk from B to C
---
---
moving disk from A to B
2 C B A
1 C A B
moving disk from C to A
---
moving disk from C to B
1 A B C
moving disk from A to B
---
---
---
*/
