/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */

candidate_number(17655).

%% A*

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
	map_distance(Adj1,Goal,D1), 					
	\+ memberchk(c(CurrG+D1+1,CurrG+1,Adj1,[Curr|Path_to_P]),Agenda),
	add_sorted_Agenda(c(CurrG+D1+1,CurrG+1,Adj1,[Curr|Path_to_P]),Agenda,Add_one_Agenda),!,
	add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Add_one_Agenda,NewAgenda).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,NewAgenda,NewAgenda).

add_sorted_Agenda(Child,[Curr|Rest],[Child,Curr|Rest]):-
	Child = c(Value1,_,_,_),
	Curr = c(Value2,_,_,_),
	Value1 =< Value2.
<<<<<<< HEAD
=======

add_sorted_Agenda(Child,[],[Child]).
>>>>>>> 4fa6b5646778ac0b450b2d4f7e820ede0320d2b5

add_sorted_Agenda(Child,[Curr|Rest],[Curr|NewAgenda]):-
	Child = c(Value1,_,_,_),
	Curr = c(Value2,_,_,_),
	Value1 > Value2,
	add_sorted_Agenda(Child,Rest,NewAgenda).

%%%%%%%%%%%%% end A*

solve_task(Task,Cost):-
	agent_current_position(oscar,P),
	solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,	% prune choice point for efficiency
	reverse(R,[_Init|Path]),
	agent_do_moves(oscar,Path).

%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :- 
	achieved(Task,Current,RPath,Cost,NewPos).
solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
	Current = [c(F,P)|RPath],
	search(P,P1,R,C),
	\+ memberchk(R,RPath), % check we have not been here already
	D1 is D+1,
	F1 is F+C,
	solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos). % backtracking search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
	Current = [c(Cost,NewPos)|RPath],
	( Exit=none -> true
	; otherwise -> RPath = [Exit|_]
	).
achieved(find(O),Current,RPath,Cost,NewPos) :-
	Current = [c(Cost,NewPos)|RPath],
	( O=none    -> true
	; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
	).


search(F,N,N,1):-
	map_adjacent(F,N,empty).


%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = reset -> ailp_reset,shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([oscar,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([oscar,say,A])
	; R=[H|T]             -> show_response(H),show_response(T)
	; R=[]                -> true
	; otherwise           -> writes(['! ',R])
	).

writes(A):-
	( A=[]      -> nl
	; A=nl      -> nl
	; A=[H|T]   -> writes(H),writes(T)
	; A=term(T) -> write(T)
	; otherwise -> write(A)
	).

% callable(+Command, +Goal, ?Response)
callable(call(G),call(G),G).
callable(topup(S),agent_topup_energy(oscar,S),agent(topup)).
callable(energy,agent_current_energy(oscar,E),both(current_energy(E))).
callable(position,agent_current_position(oscar,P),both(current_position(P))).
callable(ask(S,Q),agent_ask_oracle(oscar,S,Q,A),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
