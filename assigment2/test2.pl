add_sorted_Agenda(Child,[Curr|Rest],[Curr|Rest]):-
	Child = Curr.

add_sorted_Agenda(Child,[Curr|Rest],[Child,Curr|Rest]):-
	Child = Value1,
	Curr = Value2,
	Value1 =< Value2.

add_sorted_Agenda(Child,[],[Child]).

add_sorted_Agenda(Child,[Curr|Rest],[Curr|NewAgenda]):-
	Child = Value1,
	Curr = Value2,
	Value1 > Value2,
	add_sorted_Agenda(Child,Rest,NewAgenda).