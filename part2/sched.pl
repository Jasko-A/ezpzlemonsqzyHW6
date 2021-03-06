/*np(grandcanyon,           [az,ut],     [hiking, camping]).
np(yosemite,              [ca],        [hiking, camping, rockclimbing]).
np(everglades,            [fl],        [swimming, camping]).
np(drytortugas,           [fl],        [swimming, snorkeling]).
np(lassen,                [ca],        [hiking, camping, skiing]).
np(deathvalley,           [ca,nv],     [hiking, warmup]).
np(ww2valorinthepacific,  [hi,ca,ak],  [historicalstudies]).
np(dinosaur,              [co,ut],     [excavating, hiking]).
np(bryce,                 [ut],        [camping, hiking]).
np(denali,                [ak],        [camping, hiking]).
np(zion,                  [ut],        [hiking, camping]).*/

np_names(N) :-
	np(N,_,_).

np_states(N,S) :-
	np(N,S,_).

np_activities(N,A) :-
	np(N,_,A).

np_names_not_yosemite(N) :-
	np_names(N),
	N \= 'yosemite'.

np_activities_yosemite(A) :-
	np_activities(N,A),
	N = 'yosemite'.

np_states_yosemite(S) :-
	np_states(N,S),
	N = 'yosemite'.

np_states_grandcanyon(S) :-
	np_states(N,S),
	N = 'grandcanyon'.

np_sorted_activities_yosemite(A):-
	np_activities(N,T),
	N = 'yosemite',
	sort(T,A).

np_single_state(N) :-
	np_states(N,S),
	length(S,1).

np_multi_state(N) :-
	np_states(N,S),
	length(S,X),
	X >= 2.

np_pair_names([N1, N2]) :-
	np_states(N1,S1),
	np_states(N2,S2),
	N1 @< N2,
	S1 = S2.

np_2_state_2_activities(N) :-
	np_states(N,S),
	length(S,2),
	np_activities(N,A),
	length(A,2).

np_12_states_1or(N) :-
	np_states(N,S),
	(
		length(S,1);
		length(S,2)
	).

	np_12_states_2wo(N) :-
		np_states(N,S),
		length(S,1).

	np_12_states_2wo(N) :-
		np_states(N,S),
		length(S,2).

np_camping_hiking_1or(N) :-
	np_activities(N,A),
	(A = [hiking, camping]; A = [camping, hiking]).

np_camping_hiking_2wo(N) :-
	np_activities(N,[hiking, camping]).


np_camping_hiking_2wo(N) :-
	np_activities(N,[camping, hiking]).


np_camping_hiking_sort(N) :-
	np_activities(N,A),
	length(A,2),
	sort(A,Z),
	Z = [camping, hiking].

insert(L,E,Z) :-
	append(L,[E],T),
	sort(T,Z).

butlast(L,Z) :-
	length(L,X),
	X = 1,
	Z = [].

butlast(L,Z) :-
	length(L,X),
	X >= 2,
	L = [H|T],
	butlast(T,D),
	Z = [H|D].

naaa(L,NAL,AL) :-
	L = [],
	NAL = [],
	AL = [].


naaa(L,NAL,AL) :-
	L = [H|T],
	naaa(T,NA,A),
	atom(H),
	AL = [H|A],
	NAL = NA.

naaa(L,NAL,AL) :-
	L = [H|T],
	naaa(T,NA,A),
	\+ atom(H),
	AL = A,
	NAL = [H|NA].
