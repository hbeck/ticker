% a :- w^1 d b

#program base.

%x.
%x :- not x.


#program signals(t).

#external b(t).


#program volatile(t).

#external now(t).

a(t) :- wd_b(t).


wd_b(t) :- b(t), now(t).
wd_b(t) :- b(t-1), now(t).

