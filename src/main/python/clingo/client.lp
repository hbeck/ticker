#program base.

#program signals(t, c).

#external b(t).

#program volatile(t, c).

#external now(t).
#external cnt(c).

a(t) :- wd_b(t).
wd_b(t) :- b(t), now(t).
wd_b(t) :- b(t-1), now(t).

