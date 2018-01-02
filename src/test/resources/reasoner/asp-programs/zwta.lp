z(X) :- w2ta(U,T), X = U + 1.
w2ta(U,T) :- now(T), reach(U,T), a(U).
reach(U,T) :- now(T), U=T-2..T.

i(T) :- w1d_z(T).

w1d_z(T) :- z(U), now(T), U >= T - 1, U <= T.

now(1).
a(1).

#show a/1.
#show z/1.
#show i/1.
