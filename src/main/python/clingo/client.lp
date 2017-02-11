#program volatile(t).

#external b_ext_at(t).
%#external b_ext_at(t+1).
%#external now(t).

b :- b_ext_at(t).
%b :- b_ext_at(t+1).

%b :- now(T), b_at(T).
%b :- now(T+1),b_at(T+1).

%b_at(t) :- now(t), b.
