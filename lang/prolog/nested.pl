%TODO: arbitrary order for predicates.
/*
Nest structure
*/

owns(slb, book(a, author(emily,jasey))).
owns(jwz, book(b, author(john,emily))).
owns(slb, book(c, author(emily,john))).

/*

Query, book owned by slb, and the _first_author is emily

?- owns(slb,book(X,author(emily,Y))).
X = a,
Y = jasey ;
X = c,
Y = john.
/*