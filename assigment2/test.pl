

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

%%add_to_Agenda(_,p(1,1),2,[],[],X).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Agenda,NewAgenda):-
	map_adjacent(Curr,Adj1,empty),
	map_distance(Adj1,Goal,H1), 
	F1 is CurrG+H1+1,	
	G1 is CurrG+1,
	\+ memberchk(c(F1,G1,Adj1,[Curr|Path_to_P]),Agenda), %% will leave it for now then optimize in added
	add_sorted_Agenda(c(F1,G1,Adj1,[Curr|Path_to_P]),Agenda,Add_one_Agenda),!,
	add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Add_one_Agenda,NewAgenda).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,NewAgenda,NewAgenda).

map_adjacent(p(1,1),p(1,2),empty).
map_adjacent(p(1,1),p(2,1),empty).
map_adjacent(p(1,1),p(0,1),empty).
map_distance(p(1,2),_,3).
map_distance(p(2,1),_,2).
map_distance(p(0,1),_,1).

///////////////////////////////////////BACKUP

solve_task_A_star(go(Goal),Cost):-
	agent_current_position(oscar,P),
	map_distance(P,Goal,H),
	Ginit is 0,
	DpthInit is 0,
	Finit is H,
	solve_task_A_star(go(Goal),c(Finit,Ginit,P,[]),[],DpthInit,Rpath,Cost,NewPos),
	reverse(R,[_Init|Path]),
	agent_do_moves(oscar,Path).

solve_task_A_star(go(p(Xgoal,Ygoal)),c(F,G,p(X,Y),Path_to_goal),Agenda,Dpth,[p(X,Y)|Path_to_goal],Cost,NewPos):-
	Xgoal = X,
	Ygoal = Y.

solve_task_A_star(go(Goal),Current,Agenda,Dpth,RR,Cost,NewPos):-
	Current=c(F0,G0,P0,Path_to_P0),
	add_to_Agenda(Goal,P0,G0,Path_to_P0,Agenda,NewAgenda),
	NewAgenda = [NewCurr|Rest],
	Dpth1 is Dpth+1,
	solve_task_A_star(go(Goal),NewCurr,Rest,Dpth,RR,Cost,NewPos).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Agenda,NewAgenda):-
	map_adjacent(Curr,Adj1,empty),
	map_distance(Adj1,Goal,H1), 
	F1 is CurrG+H1+1,	
	G1 is CurrG+1,
	\+ memberchk(c(F1,G1,Adj1,[Curr|Path_to_P]),Agenda), %% will leave it for now then optimize in added
	add_sorted_Agenda(c(F1,G1,Adj1,[Curr|Path_to_P]),Agenda,Add_one_Agenda),!,
	add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Add_one_Agenda,NewAgenda).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,NewAgenda,NewAgenda).

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