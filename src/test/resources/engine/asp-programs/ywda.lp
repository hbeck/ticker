y(T) :- w1d_a(T).

w1d_a(T) :- a(U), now(T), U >= T - 1, U <= T.

now(3).
a(1).

#show a/1.
#show y/1.
