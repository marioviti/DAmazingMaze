

adj(c(X,Y),X).

add(X,RR,[A,B|RR]):-
	X=p(A,B).




%%%this works!
	
add_sorted_Agenda(Child,[Curr|Rest],[Child,Curr|Rest]):-
	Child = c(Value1,_,_,_),
	Curr = c(Value2,_,_,_),
	Value1 =< Value2.

add_sorted_Agenda(Child,[],[Child]).

add_sorted_Agenda(Child,[Curr|Rest],[Curr|NewAgenda]):-
	Child = c(Value1,_,_,_),
	Curr = c(Value2,_,_,_),
	Value1 > Value2,
	add_sorted_Agenda(Child,Rest,NewAgenda).