/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(17655).

%% shared strategies module %%

%% strategy may change by adapting the bound to particular situation
init_state:-
	(current_predicate(curr_state/2)-> retractall(status(_)),retractall(bound(_)),retractall(curr_state(_,_))
	),
	assert(status(normal)), assert(bound(20)), assert(curr_state([],[])). 

updatepos(Pos,Type):-
	curr_state(OracleList,ChargingList),(	
		Type = o(_)-> assert(curr_state([Pos|OracleList],ChargingList));
		Type = c(_)-> assert(curr_state(OracleList,[Pos|ChargingList]))
	),
	retract(curr_state(OracleList,ChargingList)).

%% just a test method
find_soultion:- 
	writeln('into find'),fail.

check_energy_switch:-
	agent_current_energy(oscar,E),status(S),bound(B),(	
		E<B ->(	
			S = normal-> assert(status(critical)), retract(status(normal));
			otherwise->find_soultion 
		);
		otherwise ->(	
			S = normal->find_soultion;
			otherwise-> assert(status(normal)), retract(status(critical))
		)
	).

%% debug utilities %%

print_state:-
	status(S), curr_state(OracleList,ChargingList), agent_current_energy(oscar,E),
	writeln('staus': S), writeln('current energy': E),
	writeln('OracleList': OracleList), writeln('ChargingList': ChargingList).

%% A* %%

solve_task_A_star(Goal,Cost):-
	agent_current_position(oscar,P),
	( 
		Goal = go(Goal_pos) -> map_distance(P,Goal_pos,H);
	 	Goal = find(Goal_pos) -> H is 0;
	 	Goal = random -> H is 0
	),
	Ginit is 0,
	DpthInit is 0,
	Finit is H,
	solve_task_A_star(Goal,c(Finit,Ginit,P,[]),[],DpthInit,RR,Cost,NewPos),!,
	reverse(RR,[_Init|R]),
	agent_do_moves(oscar,R).

solve_task_A_star(Goal,c(F,G,p(X,Y),Path_to_goal),Agenda,Dpth,[p(X,Y)|Path_to_goal],G,NewPos):-
	( 
		Goal = go(p(Xgoal,Ygoal)) -> Xgoal = X,Ygoal = Y;
	 	Goal = find(Goal_pos) -> map_adjacent(p(X,Y),_,Goal_pos);
	 	Goal = random -> 
	 		map_adjacent(p(X,Y),_,Goal_pos),
	 		(Goal_pos=o(_);Goal_pos=c(_)),
	 		(
	 			current_predicate(found/1)->
	 				\+found(Goal_pos),
	 				assert(found(Goal_pos));
	 			otherwise->
	 				assert(found(Goal_pos))
	 		) 				
	).

solve_task_A_star(Goal,Current,Agenda,Dpth,RR,Cost,NewPos):-
	Current=c(F0,G0,P0,Path_to_P0),
	add_to_Agenda(Goal,P0,G0,Path_to_P0,Agenda,NewAgenda),
	NewAgenda = [NewCurr|Rest],
	Dpth1 is Dpth+1,
	solve_task_A_star(Goal,NewCurr,Rest,Dpth,RR,Cost,NewPos).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Agenda,NewAgenda):-
	map_adjacent(Curr,Adj1,empty),
	( 
		Goal = go(Goal_pos) -> map_distance(Adj1,Goal_pos,H1);
	 	Goal = find(Goal_pos) -> H1 is 0;
	 	Goal = random -> H1 is 0 
	),
	\+ memberchk(Adj1,Path_to_P),
	F1 is CurrG+H1+1,	
	G1 is CurrG+1,
	(
		Goal = random -> 
			\+ memberchk(c(_,_,Adj1,_),Agenda)
		;
		otherwise -> 
			\+ memberchk(c(F1,G1,Adj1,[Curr|Path_to_P]),Agenda)
	),
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
callable(Task,solve_task_A_star(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
task(random).