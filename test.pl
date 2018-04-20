spoj([],L,L).
spoj([H|T1], L2, [H|T2]) :- spoj(T1,L2,T2).

sluc(L, [], L).
sluc([], L, L).
sluc([],[],[]).
sluc([X|XS], [Y|YS], [X|T]) :- X > Y, sluc(XS, [Y|YS], T).
sluc([X|XS], [Y|YS], [Y|T]) :- sluc([X|XS], YS, T).

serad([], []).
serad([H|T], SL) :- serad(T, SL), sluc([H], SL, SL).
