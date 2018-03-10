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
	AL = [],!.


naaa(L,NAL,AL) :-
	L = [H|T],
	naaa(T,NA,A),
	atom(H),
	AL = [H|A],
	NAL = NA,!.

naaa(L,NAL,AL) :-
	L = [H|T],
	naaa(T,NA,A),
	\+ atom(H),
	AL = A,
	NAL = [H|NA],!.


splitlist(L,Left,Pivot,Right) :-
	L = [H|T],
	H = Pivot,
	Left = [],
	Right = T.

splitlist(L,Left,Pivot,Right) :-
	L = [H|T],
	H \= Pivot,
	splitlist(T,L1,Pivot,R1),
	Left = [H|L1],
	Right = R1.

split3list(L, Owner, Left, Pivot, Right) :-
	L = [SL|RL],
	SL = [_,_,H|[]],
	H = Owner,
	Pivot = SL,
	Right = RL,
	Left = [].

split3list(L, Owner, Left, Pivot, Right) :- %SL = sublist RL = Rest of List
	L = [SL|RL],
	SL = [_,_,H|[]],
	H \= Owner,
	split3list(RL, Owner, L1, Pivot, Right),
	Left = [SL|L1].

perm([],[]).

perm(L, PermL) :-
	select(E,L,R),
	perm(R,Q),
	PermL = [E|Q].

 permsub(L, PermL) :-
 	naaa(L,NAL,AL),
 	perm(L, PermL),
 	subtract(PermL,AL,X),
 	X = NAL.
 
fit1stRequest([Owner,Size], MemList, NewMemList) :-
	splitlist(MemList,Left,[H1,H2,z],Right),
	H2 > Size,
	NewSize is Size + H1,
	NewAddress is H2-Size,
	TempP = [[H1,Size,Owner],[NewSize,NewAddress,z]],
	append(TempP,Right,TempRight),
 	append(Left,TempRight, NewMemList).	

fit1stRequest([Owner,Size], MemList, NewMemList) :-
 	splitlist(MemList,Left,[H1,Size,z],Right),
 	TempP = [[H1,Size,Owner]],
 	
 	/*Pivot = [H1,H2,_|[]],
 	(
 		(	
 			X is (Size - H2),
 			X = 0,
 			TempP = [[H1,Size,Owner]]
 		);
 		(
 			X is (Size - H2),
 			X < 0,
 			P is Size + H1,
 			Q is H2 - Size,
 			TempP = [[H1,Size,Owner],[P,Q,z]]
 			
 		)

 	),*/
 	
 	append(TempP,Right,TempRight),
 	append(Left,TempRight, NewMemList).



fitRelease(Owner,MemList,NewMemList) :-
 	split3list(MemList,Owner,Left,Pivot,Right),!,
 	Pivot = [X,Y,_|[]],
 	(
 		(
 			last(Left,TempL),
 			TempL = [H1,H2,H3|[]],
 			H3 = z,
 			Right = [H|T],
 			H = [_,K2,K3|[]],
 			K3 = z,
 			NewSize is Y + H2 + K2,
 			NewPivot = [[H1,NewSize,z]],
 			delete(Right,H,DRight),
 			butlast(Left,NewLeft),
 			append(NewPivot,DRight,TempRight),
 			append(NewLeft,TempRight,NewMemList),!
 		);
 		(
 			last(Left,TempL),
 			TempL = [H1,H2,H3|[]],
 			H3 = z,
 			NewSize is Y + H2,
 			NewPivot = [[H1,NewSize,z]],
 			butlast(Left,NewLeft),
 			append(NewPivot,Right,TempRight),
 			append(NewLeft,TempRight,NewMemList),!

 		);

 		(
 			Right = [H|T],
 			H = [H1,H2,H3|[]],
 			H3 = z,
 			NewSize is Y + H2,
 			NewPivot = [[X,NewSize,z]],
 			append(NewPivot,T,TempRight),
 			append(Left,TempRight,NewMemList),!

 		);
 		(
 			NewPivot = [[X,Y,z]],
 			append(NewPivot,Right,TempRight),
 			append(Left,TempRight,NewMemList),!
 		)
 	).

fitanyRequest([Owner,Size],MemList,NewMemList) :-
	select([H1,Size,z],MemList,RemList),
	insert(RemList,[H1,Size,Owner],NewMemList).
	
fitanyRequest([Owner,Size],MemList,NewMemList) :-
	select([H1,Size2,z],MemList,RemList),
	Size2 > Size,
	NewSize is Size2 - Size,
	NewAddress is H1 + Size,
	NewElem = [H1,Size,Owner],
	NewAddon = [NewAddress,NewSize,z],
	insert(RemList,NewElem,W),
	insert(W,NewAddon,NewMemList).

fit1st([], MemList, MemList).

fit1st(RRList, MemList, NewMemList) :-
	RRList = [H|T],
	(
		(
			length(H,2),
			fit1stRequest(H,MemList,NewMemList2),!
		);
		(
			atom(H),
			fitRelease(H,MemList,NewMemList2),!
		)
		
	),
	fit1st(T,NewMemList2,NewMemList).

fitany([], MemList, MemList).

fitany(RRList,MemList,NewMemList) :-
	RRList = [H|T],
	(
		(
			length(H,2),
			fitanyRequest(H,MemList,NewMemList2)
		);
		(
			atom(H),
			fitRelease(H,MemList,NewMemList2),!
		)
		
	),
	fitany(T,NewMemList2,NewMemList).

fit1stTryHarder(RRList,MemList,NewRRList,NewMemList) :-
	fit1st(RRList,MemList,NewMemList),!,
	fail.

fit1stTryHarder(RRList, MemList, NewRRList, NewMemList) :-
	permsub(RRList,NewRRList),
	fit1st(NewRRList, MemList, NewMemList).

fitanyTryHarder(RRList,MemList,NewRRList,NewMemList) :-
	fitany(RRList,MemList,NewMemList),!,
	fail.

fitanyTryHarder(RRList, MemList, NewRRList, NewMemList) :-
	permsub(RRList,NewRRList),
	fitany(NewRRList, MemList, NewMemList).
	

	




	






 	



	
	