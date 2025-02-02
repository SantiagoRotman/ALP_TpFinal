a(aa).
a(bb).
b(bb).


?- X = 'a='.
?- b \= X.
?- Y = b, X = a, b \= X, a \= Y, X \= Y.

?- k(h(g),Y) = k(X,t(k)).
% X = h(g)
% Y = t(k)
% yes

?- k(s(g), t(k)) = k(X,t(Y)).

% X = s(g)
% Y = k
% yes

?- loves(X,X) = loves(marcellus,mia).

?- a(X), b(X), aa \= X. 

?- x = x = x \= false.

?- X is (5-4).
?- X is (5-4+3).

?- (8+4) > (4+15).
