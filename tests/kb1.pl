% Relationships
parent(john, alice).
parent(john, bob).
parent(mary, alice).
parent(mary, bob).
parent(alice, charlie).
parent(alice, diana).
parent(bob, emily).
parent(bob, frank).
parent(susan, george).
parent(susan, helen).
parent(george, ian).
parent(helen, julia).

% Sibling relationships
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

% Spouse relationships
spouse(john, mary).
spouse(bob, susan).
spouse(george, helen).

% Gender
male(john).
male(bob).
male(charlie).
male(frank).
male(george).
male(ian).
female(mary).
female(alice).
female(diana).
female(susan).
female(emily).
female(helen).
female(julia).

% Locations
lives_in(john, london).
lives_in(mary, london).
lives_in(alice, paris).
lives_in(bob, berlin).
lives_in(charlie, paris).
lives_in(diana, rome).
lives_in(emily, berlin).
lives_in(frank, london).
lives_in(george, new_york).
lives_in(helen, new_york).
lives_in(ian, tokyo).
lives_in(julia, tokyo).

% Hobbies
hobby(john, reading).
hobby(mary, painting).
hobby(alice, cooking).
hobby(bob, cycling).
hobby(charlie, swimming).
hobby(diana, photography).
hobby(emily, hiking).
hobby(frank, gaming).
hobby(george, traveling).
hobby(helen, gardening).
hobby(ian, coding).
hobby(julia, dancing).

% Preferences
likes(john, coffee).
likes(mary, tea).
likes(alice, wine).
likes(bob, beer).
likes(charlie, juice).
likes(diana, coffee).
likes(emily, tea).
likes(frank, soda).
likes(george, whiskey).
likes(helen, water).
likes(ian, milk).
likes(julia, lemonade).

% Workplaces
works_at(john, company_a).
works_at(mary, company_b).
works_at(alice, company_c).
works_at(bob, company_d).
works_at(charlie, company_e).
works_at(diana, company_f).
works_at(emily, company_g).
works_at(frank, company_h).
works_at(george, company_i).
works_at(helen, company_j).
works_at(ian, company_k).
works_at(julia, company_l).

% Rules
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

city_residents(City, Residents) :-
    findall(Person, lives_in(Person, City), Residents).

shared_hobby(Hobby, People) :-
    findall(Person, hobby(Person, Hobby), People).


some(X) :- male(X) ; female(X).

?- some(charlie).

?- (likes(X, coffee), works_at(X, company_a), hobby(X, reading); works_at(X, company_d)); works_at(X, company_b).

?- likes(X, coffee), works_at(X, company_a).

?- ancestor(X, charlie).

?- ancestor(X, charlie), female(X).
% X = alice ;
% X = john ;
% X = mary ;
% false.