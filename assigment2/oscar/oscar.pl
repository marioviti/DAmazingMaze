/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */

:-consult('wp.pl').
candidate_number(17655).

%% shared strategies module %%

%% strategy may change by adapting the bound to particular situation

:- dynamic
	found_internal_objects/1,found/1,bound/1,status/1,curr_state/2.

reset_bound:-
	retractall(bound(_)),assert(bound(25)).

init_state:-
	complete,assert(status(normal)),assert(bound(25)),assert(curr_state([],[])).

complete:-
	retractall(curr_state(_,_)),
	retractall(bound(_)),
	retractall(status(_)),
	retractall(pred_actor(_)),
	retractall(found(_)),		
	retractall(found_internal_objects(_)).

updatepos(Pos,Type):-
	curr_state(OracleList,ChargingList),
	(	
		Type = o(_)-> assert(curr_state([Pos|OracleList],ChargingList));
		Type = c(_)-> assert(curr_state(OracleList,[Pos|ChargingList]))
	),
	retract(curr_state(OracleList,ChargingList)).

deletepos(Pos,Type):-
	curr_state(OracleList,ChargingList),
	(	
		Type = o(_)-> subtract(OracleList, [Pos], NewOracleList), assert(curr_state(NewOracleList,ChargingList));
		Type = c(_)-> subtract(ChargingList, [Pos], NewChargingList), assert(curr_state(OracleList,NewChargingList))
	),
	retract(curr_state(OracleList,ChargingList)).

%% Use getNearest/2 gets the nearest Pos from a List of Pos

updateHeuristic(CurrP,[],[]).

updateHeuristic(CurrP,[First|Rest],[couple(First,D)|List]):-
	map_distance(CurrP,First,D),
	updateHeuristic(CurrP,Rest,List).

add_sorted(Child,[Curr|Rest],[Child,Curr|Rest]):-
	Child = couple(_,Value1),
	Curr = couple(_,Value2),
	Value1 =< Value2.

add_sorted(Child,[],[Child]).

add_sorted(Child,[Curr|Rest],[Curr|NewAgenda]):-
	Child = couple(_,Value1),
	Curr = couple(_,Value2),
	Value1 > Value2,
	add_sorted(Child,Rest,NewAgenda).

sort([],NewSortedPosList,NewSortedPosList).

sort([First|Rest],TemplList,SortedPosList):-
	add_sorted(First,TemplList,NewSortedPosList),
	sort(Rest,NewSortedPosList,SortedPosList).

getNearest(PosList,Target,Type):-
	agent_current_position(oscar,CurrP),
	updateHeuristic(CurrP,PosList,WeightedPosList),
	sort(WeightedPosList,[],SortedPosList),
	SortedPosList=[couple(Target,_)|Rest],
	deletepos(Target,Type).

%% debug utilities %%

%%debug(true).
debug(false).

debug_message(A):-
(
	debug(true)->writeln(A);
	debug(false)->true
).

print_state:-
(
	debug(true)->
		status(S), curr_state(OracleList,ChargingList), agent_current_energy(oscar,E),
		writeln('status': S), writeln('current energy': E),
		writeln('OracleList': OracleList), writeln('ChargingList': ChargingList);
	debug(false)->true
).

%% main strategy stub

start_solving(A):-
	init_state,find_solution,!,pred_actor(A),do_command([oscar,say,A]).

find_solution:-
	\+agent_current_energy(oscar,0),
	count_actors(1, ActorCount),
	status(S),
	debug_message('find'),
	(
		ActorCount=1->true;	
		S=normal->debug_message('find normal'),normal_strategy,!;
		S=critical->debug_message('find critic'),critical_strategy,!
	).

%% energy switch from normal to critical

check_energy_switch:-
	agent_current_energy(oscar,E),status(S),bound(B),(	
		E<B ->(	
			S = normal-> assert(status(critical)),retract(status(normal)),find_solution;
			otherwise->find_solution 
		);
		otherwise ->(	
			S = normal->find_solution;
			otherwise-> assert(status(normal)),retract(status(critical)),find_solution 
		)
	).

%% this should always return a o(_) or a c(_) position first

my_map_adjacent(CurrP,AdjPos,RetType):-
	map_adjacent(CurrP,AdjPos,RetType),
	\+found_internal_objects(RetType),
	(RetType=o(_);RetType=c(_)).

my_map_adjacent(CurrP,AdjPos,RetType):-
	map_adjacent(CurrP,AdjPos,RetType),RetType=empty.

%% critical status strategy

critical_strategy:-
	agent_current_position(oscar,CurrP),
	my_map_adjacent(CurrP,AdjPos,Type),!,
	curr_state(_,ChargingList),
	(
		Type=c(_)->agent_topup_energy(oscar, Type),assert(found_internal_objects(Type)),reset_bound;
		%%\+ChargingList=[]->getNearest(ChargingList,Target,c(_)),solve_task_A_star(go(Target),_);
		Type=o(_)->updatepos(CurrP,Type),solve_task_A_star(random,_);
		otherwise->solve_task_A_star(random,_)
	),
	check_energy_switch.


%% normal status strategy

normal_strategy:-
	agent_current_position(oscar,CurrP),
	my_map_adjacent(CurrP,AdjPos,Type),debug_message('normal_strategy adj':Type),!,
	curr_state(OracleList,_),(
		Type=o(_)->
			bound(B),
			agent_current_energy(oscar,E),
			Ask_oracle_cost is B + 10,(
				E<Ask_oracle_cost->retractall(bound(_)),assert(bound(Ask_oracle_cost)),updatepos(CurrP,Type);
				otherwise->find_identity(Type),assert(found_internal_objects(Type)),debug_message('asking oracle')
			);
		Type=c(_)->updatepos(CurrP,Type),solve_task_A_star(random,_);
		\+OracleList=[]->getNearest(OracleList,Target,o(_)),solve_task_A_star(go(Target),_);
		otherwise->solve_task_A_star(random,_),debug_message('notafailin')
	),
	check_energy_switch.

%% A* %%

test_assert:-
	test_assert(1).

test_assert(A):-
	A1 is A+1,A1<12,A1>0,assert(found(c(A))),assert(found(o(A))),test_assert(A1).

solve_task_A_star(Goal,Cost):-
	debug_message('A_star'),
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

acheived(Goal,c(F,G,p(X,Y),Path_to_goal),Agenda,Dpth,[p(X,Y)|Path_to_goal],G,NewPos):-
	( 	
		Goal = go(p(Xgoal,Ygoal)) -> Xgoal = X,Ygoal = Y;
	 	Goal = find(Goal_pos) -> map_adjacent(p(X,Y),_,Goal_pos);
	 	Goal = random -> 
	 		map_adjacent(p(X,Y),_,Goal_pos),
	 		(Goal_pos=o(_);Goal_pos=c(_)),
	 		\+found(Goal_pos),assert(found(Goal_pos))		
	).

solve_task_A_star(Goal,Current,Agenda,Dpth,RR,Cost,NewPos):-
	debug_message('A_star_recursive'),
	Current=c(F0,G0,P0,Path_to_P0),
	add_to_Agenda(Goal,P0,G0,Path_to_P0,Agenda,NewAgenda),
	NewAgenda = [NewCurr|Rest],
	Dpth1 is Dpth+1,(
		acheived(Goal,NewCurr,Rest,Dpth,RR,Cost,NewPos)->debug_message('acheived'),true;
		otherwise->solve_task_A_star(Goal,NewCurr,Rest,Dpth,RR,Cost,NewPos)
	).

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
			\+ memberchk(c(_,_,Adj1,_),Agenda);
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
