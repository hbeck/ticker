x(T) :- w1b_a(T).

w1b_a(T) :- now(T), not spoil_w1b_a(T).
spoil_w1b_a(T) :- reach_w1b_a(U,T), not a(U).
reach_w1b_a(U,T) :- now(T), U=T-1..T.

now(2).
a(1).
a(2).

#show a/1.
#show x/1.
