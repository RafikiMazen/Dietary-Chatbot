:-  [info_food_26258].
:-  [read_sentence_26355]. 

totalCal(1800).

welcome():-
	write("> Welcome to your personal assistant"),nl,
	write("> "),
	res(Input),
	list_butlast(Input,In),
	readInputTillQuitHelper(In,[],[]).
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		Q=[quit],
		reverse(PQ,QR),
		reverse(PR,RR),
		write("> "),
		quitHelper(QR,RR),
		write("Bye").
		
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		length(R,1),
		R=[RR],
		write("> "),
		write(RR),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,[Q|PQ],[R|PR]).
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R=["I", told, you, that, before],
		write("> "),
		write("I told you that before"),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,PQ,PR).
		
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R=["Nothing", from, what, i, know],
		write("> "),
		write("Nothing from what I know"),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,PQ,PR).	
readInputTillQuitHelper(Q,PQ,PR):- 
		\+isValid(Q),
		write("> "),
		write("I can not understand you"),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,PQ,PR).		
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R=["You", can, have, F, for, M],
		write("> "),
		write("You can have "),write(F),write(" for "),write(M),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,[Q|PQ],[R|PR]).	
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R=[F, "is", not, suitable, for, M],
		write("> "),
		write(F),write(" is not suitable for "),write(M),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,[Q|PQ],[R|PR]).			
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R = [C, "Calories"],
		write("> "),
		write(C),write(" Calories"),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,[Q|PQ],[R|PR]).	
readInputTillQuitHelper(Q,PQ,PR):- 
		isValid(Q),
		response(Q,PQ,PR,R),
		R = ["I", do, not, know],
		write("> "),
		write("I do not know"),nl,
		write("> "),
		res(Input),
		list_butlast(Input,In),
		readInputTillQuitHelper(In,PQ,PR).		
		
		




readInputTillQuit():-
			welcome().


quitHelper([],[]).			
quitHelper([HQ|TQ],[_|TR]):-
		HQ=[i,ate,F,for,M],
		write("You had "),write(F),write(" for "),write(M),nl,
		quitHelper(TQ,TR).
quitHelper([_|TQ],[HR|TR]):-
		HR=["You", can, have, F, for, M],
		write("You had "),write(F),write(" for "),write(M),nl,
		quitHelper(TQ,TR).
quitHelper([HQ|TQ],[HR|TR]):-
		HQ\=[i,ate,F,for,M],
		HR\=["You", can, have, F, for, M],
		quitHelper(TQ,TR).			
		
list_butlast([X|Xs], Ys) :-                 
   list_butlast_prev(Xs, Ys, X).           

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-  
   list_butlast_prev(Xs, Ys, X1).    			


		
isValid(Q):-
		Q=[how,many,calories,does,_,contain];
		Q=[what,does,_,contain];
		Q=[can,i,have,_,for,_];
		Q=[what,is,_];
		Q=[how,many,calories,do,i,have,left];
		Q=[what,kind,of,_,does,_,contain];
		Q=[is,_,a,_,in,_];
		Q=[what,can,i,have,for,_,that,contains,_];
		Q=[i,ate,_,for,_];
		Q=[i,do,not,eat,_];
		Q=[quit].
		
filterPropHelper(Relation,(A,B)):-
		prop(A,Relation,B).
filterProp(Relation,R):-
		setof(X,filterPropHelper(Relation,X),R).	
		
matchFirst(_,[],[]).		
matchFirst(T1,[(T1,X)|T],[R|Y]):-
		R=X-1,
		matchFirst(T1,T,Y).
matchFirst(T1,[(Z,X)|T],[R|Y]):-
		T1\=Z,
		R=X-0,
		matchFirst(T1,T,Y).
	
matchSecond(_,[],[]).		
matchSecond(T1,[(X,T1)|T],[R|Y]):-
		R=X-1,
		matchSecond(T1,T,Y).
matchSecond(T1,[(X,Z)|T],[R|Y]):-
		T1\=Z,
		R=X-0,
		matchSecond(T1,T,Y).

mergeHelper1([],Max-0,Max).
mergeHelper1(L,Max-X,Max):-
			L=[Max-H|T],
			mergeHelper1(T,Max-TR,Max),
			X is TR+H.
mergeHelper1(L,RT,Max):-
			L=[Z-_|T],
			Z\=Max,
			mergeHelper1(T,RT,Max).

mergeHelper3([],_,[]).
mergeHelper3([HL|TL],H,[R|L]):-
		mergeHelper1(H,R,HL),
		mergeHelper3(TL,H,L).
	
mergeHelper5([],Acc,Acc).		
mergeHelper5([H|T],Acc,R):-
	H=X-_,
	\+member(X,Acc),
	mergeHelper5(T,[X|Acc],R).
mergeHelper5([H|T],Acc,R):-
	H=X-_,
	member(X,Acc),
	mergeHelper5(T,Acc,R).	
	
mergeMatchLists(M1,M2,R):-
		append(M1,M2,L),
		mergeHelper5(L,[],AL),
		mergeHelper3(AL,L,R).

bestMatchesHelper([],-1).
bestMatchesHelper([_-X|T],X):-
		bestMatchesHelper(T,Maxtail),
		X>=Maxtail.
bestMatchesHelper([_-X|T],Maxtail):-
		bestMatchesHelper(T,Maxtail),
		X<Maxtail.

bestMatchesHelper1([],[],_).
bestMatchesHelper1(L,[H|RT],Max):-
			L=[H-Max|T],
			bestMatchesHelper1(T,RT,Max).
bestMatchesHelper1(L,RT,Max):-
			L=[_-Z|T],
			Z\=Max,
			bestMatchesHelper1(T,RT,Max).

bestMatches(L,R):-
	bestMatchesHelper(L,M),
	bestMatchesHelper1(L,R,M).	
bestMatchesMin(L,Min,R):-
	bestMatchesHelper1(L,R,Min).
	
	
foodCal(F,C):-
		\+prop(F,contain,_),
		prop(F,contain,C,cal).
foodCal(F,C):-
		prop(F,contain,_),
		filterProp(contain,L1),
		matchFirst(F,L1,R1),
		foodCalHelper(R1,C).
foodCalHelper([],0).		
foodCalHelper([H|T],C):-
			H=_-0, 
			foodCalHelper(T,C).
							
foodCalHelper([H|T],C):-
			H=E-1,
			foodCal(E,CF),
			foodCalHelper(T,CT),
			C is CF+CT.	
				

foodCalList([],0).
foodCalList(FL,C):-
		FL=[H|T],
		prop(H,_,_),
		foodCal(H,C1),
		foodCalList(T,C2),
		C is C1+C2.
foodCalList(FL,C2):-
		FL=[H|T],
		\+prop(H,_,_),
		foodCalList(T,C2).		

		
containPropF(X):-
	prop(X,_,_).
containPropL(X):-
		prop(_,_,X).
calcCalories(X,[],[],R):-
	totalCal(Y),
	foodCal(X,Z),
	R is Y-Z.
calcCalories(F,[HQ|TQ],[_|TR],C):- 
	HQ=[i,ate,X,for,_,.],
	foodCal(X,Z),
	calcCalories(F,TQ,TR,CT),
	C is CT-Z.

diffHelper(_,[],[],[]).	
diffHelper(Q,[Q|T],[H|TR],[H|R]):-
	diffHelper(Q,T,TR,R).
diffHelper(Q,[X|T],[_|TR],R):-
	X\=Q,
	diffHelper(Q,T,TR,R).	

getDiffAnswer(Q,PQ,PR,[H|T],R):-
		diffHelper(Q,PQ,PR,L1),
		member([H],L1),
		getDiffAnswer(Q,PQ,PR,T,R).
getDiffAnswer(Q,PQ,PR,[H|_],H):-
		diffHelper(Q,PQ,PR,L1),
		\+member([H],L1).		


mInsertion_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],X,X).
i_sort([HL-H|T],Accumulator,Sorted):-insert(HL-H,Accumulator,N),i_sort(T,N,Sorted).
insert(Z-X,[ZY-Y|T],[ZY-Y|NT]):-X<Y,insert(Z-X,T,NT).
insert(ZX-X,[ZY-Y|T],[ZX-X,ZY-Y|T]):-X>=Y.
insert(X,[],[X]).	
	
listOrderDesc(L,S):-
		mInsertion_sort(L,S).
foodFromHistory([],[]).
foodFromHistory([H|T],[X|R]):-
	(H=[i,ate,X,for,_];H=[you,can,have,X,for,_]),
	foodFromHistory(T,R).
foodFromHistory([H|T],R):-
	\+(H=[i,ate,_,for,_];H=[you,can,have,_,for,_]),
	foodFromHistory(T,R).	
	
getUnlikedIngredients([],[]).
getUnlikedIngredients([H|T],[X|R]):-
	H=[i,do,not,eat,X],
	getUnlikedIngredients(T,R).
getUnlikedIngredients([H|T],R):-
	\+H=[i,do,not,eat,_],
	getUnlikedIngredients(T,R).		
	

elminator(_,[],[]).
elminator(M,[H|T],[H-1|R]):-
	\+prop(H,not,M),
	elminator(M,T,R).
elminator(M,[H|T],R):-
	prop(H,not,M),
	elminator(M,T,R).	
incFI(_,[],[]).
incFI(FI,[E-O|T],[E-X|R]):-
	prop(E,contain,FI),
	X is O +1,
	incFI(FI,T,R).
incFI(FI,[E-O|T],[E-O|R]):-
	\+prop(E,contain,FI),
	incFI(FI,T,R).
incH(_,[],[]).
incH(FH,[H-O|T],[H-O|R]):-
	member(H,FH),
	incH(FH,T,R).
incH(FH,[H-O|T],[H-X|R]):-
	\+member(H,FH),
	X is O+1,
	incH(FH,T,R).	
incC(_,[],[]).
incC(RC,[H-O|T],[H-X|R]):-
	foodCal(H,C),
    C=<RC,
	X is O+1,
	incC(RC,T,R).
incC(RC,[H-O|T],[H-O|R]):-
	foodCal(H,C),
    C>RC,
	incC(RC,T,R).	
		
responseO(Q,PQ,PR,LR):-
	Q=[what,can,i,have,for,M,that,contains,FI],
	prop(_,_,FI),
	prop(_,_,M),
	setof(X,containHelper(X),R2),
	elminator(M,R2,L),
	incFI(FI,L,L1),
	getUnlikedIngredients(PQ,FH),
	incH(FH,L1,L2),
	append(PQ,PR,PQPR),
	foodFromHistory(PQPR,A),
	totalCal(T),
	foodCalList(A,C),
	RC is T-C,
	incC(RC,L2,L3),
	listOrderDesc(L3,LR).

%Q=[what,can,i,have,for,_,that,contains,_] (h)

	

response(Q,PQ,PR,[R]):-
    Q=[what,can,i,have,for,M,that,contains,FI],
	prop(_,_,FI),
	prop(_,_,M),
	responseO(Q,PQ,PR,L),
	bestMatchesMin(L,4,CR),
	length(CR,LL),
	LL>=1,
	getDiffAnswer(Q,PQ,PR,CR,R).	


response(Q,PQ,PR,["I",told,you,that,before]) :-
	Q=[what,can,i,have,for,M,that,contains,FI],		
	prop(_,_,FI),
  	prop(_,_,M),
	responseO(Q,PQ,PR,L),
	bestMatchesMin(L,4,CR),
	length(CR,LL),
	LL>=1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).		
response(Q,_,_,["I", do, not, know]) :-
		Q=[what,can,i,have,for,M,that,contains,FI],
		(\+ prop(_,_,M);(\+prop(FI,_,_))).
response(Q,PR,PQ,["Nothing",from,what,i,know]) :-
	Q=[what,can,i,have,for,M,that,contains,FI],		
	prop(_,_,FI),
  	prop(_,_,M),
	responseO(Q,PQ,PR,L),
	bestMatchesMin(L,4,CR),
	length(CR,LL),
	LL=0.
	


			
%[what,kind,of,FC,does,F,contain]
response(Q,_,_,["I",do,not,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	((\+ prop(_,_,FC));	(\+prop(F,_,_))).

response(Q,_,_,["Nothing",from,what,i,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatchesMin(L3,2,CR),
	length(CR,0).

response(Q,PQ,PR,[R]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatchesMin(L3,2,CR),
	length(CR,N),
	N >= 1,
	getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,["I",told,you,that,before]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatchesMin(L3,2,CR),
	length(CR,N),
	N >= 1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).
	
%Q=[how,many,calories,does,_,contain] (a)

response(Q,_,_,["I",do,not,know]) :-
	Q = [how,many,calories,does,F,contain],
	\+foodCal(F,_).
	
response(Q,_,_,[X,"Calories"]) :-
	Q = [how,many,calories,does,F,contain],
	foodCal(F,X).

	
%Q=[what,does,F,contain,?]; (b)

response(Q,_,_,["I",do,not,know]) :-
	Q = [what,does,F,contain],
	\+prop(F,contain,_).
	
response(Q,PQ,PR,[R]) :-
	Q = [what,does,F,contain],
	prop(F,contain,_),
	filterProp(contain,L1),
	matchFirst(F,L1,R1),
	bestMatchesMin(R1,1,CR),
	length(CR,N),
	N>=1,
	getDiffAnswer(Q,PQ,PR,CR,R).
	
response(Q,PQ,PR,["I",told,you,that,before]) :-
	Q = [what,does,F,contain],
	prop(F,contain,_),
	filterProp(contain,L1),
	matchFirst(F,L1,R1),
	bestMatchesMin(R1,1,CR),
	length(CR,N),
	N>=1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).	

	
%Q=[can,i,have,_,for,_,?]; (c)

response(Q,_,_,[F,"is",not,suitable,for,X]) :-
	Q = [can,i,have,F,for,X],
	prop(F,_,_),
	prop(F,not,X).

response(Q,_,_,["I",do,not,know]) :-
	Q = [can,i,have,F,for,_],
	\+prop(F,_,_).

response(Q,PQ,PR,["You", can, have, F, for, X]) :-
	Q = [can,i,have,F,for,X],
	prop(F,_,_),
	prop(F,not,Y),
	Y\=X,
    append(PQ,PR,PQPR),
	foodFromHistory(PQPR,L),
	totalCal(T),
	foodCalList(L,CFL),
	foodCal(F,CF),
	Z is T-CFL,
	CF=<Z.

response(Q,PQ,PR,["No"]) :-
	Q = [can,i,have,F,for,X],
	prop(F,_,_),
	prop(F,not,Y),
	Y\=X,
	 append(PQ,PR,PQPR),
	foodFromHistory(PQPR,L),
	totalCal(T),
	foodCalList(L,CFL),
	foodCal(F,CF),
	Z is T-CFL,
	CF>Z.

%Q=[what,is,_] (d)

response(Q,_,_,["I",do,not,know]) :-
	Q = [what,is,F],
	(   \+prop(F,_,_);\+prop(F,is,_)).
	
	
response(Q,PQ,PR,[R]) :-
	Q = [what,is,F],
	prop(F,_,_),
	prop(F,is,_),
	filterProp(is,L1),
	matchFirst(F,L1,R1),
	bestMatchesMin(R1,1,CR),
	length(CR,N),
	N>=1,
	getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,["I",told,you,that,before]) :-
	Q = [what,is,F],
	prop(F,_,_),
	prop(F,is,_),
	filterProp(is,L1),
	matchFirst(F,L1,R1),
	bestMatchesMin(R1,1,CR),
	length(CR,N),
	N>=1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).	
	
%Q=[how,many,calories,do,i,have,left]; (e)


response(Q,[],[],[R, "Calories"]) :-
		Q=[how,many,calories,do,i,have,left],
		totalCal(R).
response(Q,PQ,PR,[R, "Calories"]) :-
		Q=[how,many,calories,do,i,have,left],
		append(PQ,PR,PQPR),
		foodFromHistory(PQPR,L),
		totalCal(T),
		foodCalList(L,C),
		R is T-C.
response(Q,PQ,PR,["I", do, not, know]) :-
		Q=[how,many,calories,do,i,have,left],
		append(PQ,PR,PQPR),
		foodFromHistory(PQPR,L),
		\+foodCalList(L,_).




		
%Q=[what,kind,of,_,does,_,contain]; (f)


response(Q,_,_,["I", do, not, know]) :-
		Q=[what,kind,of,FT,does,F,contain],
		(\+ prop(_,_,FT);(\+prop(F,_,_))).

response(Q,_,_,["Nothing", from, what, i, know]) :-
		Q=[what,kind,of,FT,does,F,contain],
		prop(_,_,FT),
		prop(F,_,_),
		filterProp(is,L1),
		filterProp(contain,L2),
		matchSecond(FT,L1,R1),
		matchFirst(F,L2,R2),
		mergeMatchLists(R1,R2,L3),
		bestMatchesMin(L3,2,CR),
		length(CR,0).
response(Q,PQ,PR,[R]) :-
		Q=[what,kind,of,FT,does,F,contain],
		prop(_,_,FT),
		prop(F,_,_),
		filterProp(is,L1),
		filterProp(contain,L2),
		matchSecond(FT,L1,R1),
		matchFirst(F,L2,R2),
		mergeMatchLists(R1,R2,L3),
		bestMatchesMin(L3,2,CR),
		length(CR,X),
		X>=1,
		getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,["I",told,you,that,before]) :-
		Q=[what,kind,of,FT,does,F,contain],
		prop(_,_,FT),
		prop(F,_,_),
		filterProp(is,L1),
		filterProp(contain,L2),
		matchSecond(FT,L1,R1),
		matchFirst(F,L2,R2),
		mergeMatchLists(R1,R2,L3),
		bestMatchesMin(L3,2,CR),
		length(CR,X),
		X>=1,
		\+getDiffAnswer(Q,PQ,PR,CR,_).		
		
		



		
%Q=[is,_,a,_,in,_]; (g)

response(Q,_,_,["I", do, not, know]) :-
		Q=[is,F,a,FT,in,FC],
		(\+ prop(_,_,FT);(\+prop(F,_,_));(\+prop(FC,_,_))).
response(Q,_,_,["No"]) :-
		Q=[is,F,a,FT,in,FC],
		prop(F,_,_),
		prop(FC,_,_),
		prop(_,_,FT),
		prop(F,is,FT),
		\+prop(FC,contain,F).
		

response(Q,_,_,["Yes"]) :-
		Q=[is,F,a,FT,in,FC],
		prop(F,_,_),
		prop(FC,_,_),
		prop(_,_,FT),
		prop(F,is,FT),
		prop(FC,contain,F).
		


response(Q,_,_,["Ok"]):-
	Q=[i,ate,_,for,M],
	prop(_,_,M).
response(Q,_,_,["Ok"]):-
	Q=[i,do,not,eat,F],
	prop(F,_,_).
	
	
	



containHelper(X):-
		prop(X,contain,_).