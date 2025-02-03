factorial(0, 1).
factorial(1, 1).
factorial(N, F) :- N > 0, N1 is N-1, factorial(N1, F1), F is (N*F1).

% div(X, Y, Q) is true if Q is the quotient of X divided by Y (integer division)
div(X, Y, Q) :-
    %print(div(X, Y, Q)),
    Y > 0,                        % Ensure that Y is positive
    div_helper(X, Y, 0, Q).       % Call the helper predicate with initial count 0

% Base case: when X is less than Y, the quotient is the current count
div_helper(X, Y, Q, Q) :-
    %print(div_helper_base(X, Y, Q, Q)),
    X < Y.

% Recursive case: subtract Y from X and increment the quotient
div_helper(X, Y, Acc, Q) :-
    %print(div_helper(X, Y, Acc, Q)),
    X >= Y,
    X1 is (X - Y),                 % Subtract Y from X
    Acc1 is (Acc + 1),             % Increment the quotient accumulator
    div_helper(X1, Y, Acc1, Q).  % Recur with the new values


?- factorial(8,F).

?- fibonacci(4, X).

?- div(8,2,Z).
