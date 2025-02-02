vertical(line(point(X,Y),point(X,Z))).

horizontal(line(point(X,Y),point(Z,Y))).

%?- vertical(line(point(1,1),point(1,3))).
% yes

%?- vertical(line(point(1,1),point(3,2))).
% no

?- horizontal(line(point(1,1),point(2,Y))).

% Y = 1 ; no

?- horizontal(line(point(2,3),P)).

% P = point(_1972,3) ; no