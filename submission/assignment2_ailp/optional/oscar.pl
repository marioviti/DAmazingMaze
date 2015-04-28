/*
 *      oscar.pl
 *
 *		Students edit this program to complete the assignment.
 */


candidate_number(10110).

:-consult(wp).	% Wikipedia stuff

:-dynamic
	used_internal_objects/1,found/1,bound/1,status/1,seen_pos/1.

init_state:-
	complete,assert(status(normal)),assert(bound(25)).

complete:-
	retractall(bound(_)),
	retractall(status(_)),
	retractall(pred_actor(_)),
	retractall(found(_)),		
	retractall(used_internal_objects(_)),
	retractall(seen_pos(_)).

reset_bound:-
	retractall(bound(_)),assert(bound(25)).

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
		status(S), my_agent(Agent), game_predicates:agent_current_energy(Agent,E),
		writeln('status': S), writeln('current energy': E);
	debug(false)->true
).

%% main strategy stub

start_solving(A):-
	complete,init_state,find_solution,!,pred_actor(A),my_agent(Agent),do_command([Agent,say,A]).

%% at the end of each jurney we check if we found the soultion

find_solution:-
	my_agent(Agent),
	\+game_predicates:agent_current_energy(Agent,0),
	count_actors(1, ActorCount),
	status(S),
	debug_message('find'),
	(
		ActorCount=1->true;	
		S=normal->debug_message('find normal'),normal_strategy,!;
		S=critical->debug_message('find critic'),critical_strategy,!
	).

%% energy switch from normal to critical depending on a dynamic bound

check_energy_switch:-
	my_agent(Agent),
	game_predicates:agent_current_energy(Agent,E),status(S),bound(B),(	
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
	\+used_internal_objects(RetType),
	(RetType=o(_);RetType=c(_)).

my_map_adjacent(CurrP,AdjPos,RetType):-
	map_adjacent(CurrP,AdjPos,RetType),RetType=empty.

%% critical status strategy

critical_strategy:-
	my_agent(Agent),
	game_predicates:agent_current_position(Agent,CurrP),
	my_map_adjacent(CurrP,AdjPos,Type),!,
	(
		Type=c(_)->game_predicates:agent_topup_energy(Agent, Type),assert(used_internal_objects(Type)),reset_bound;
		Type=o(_)->solve_task_A_star(find(c(_)),_);
		otherwise->solve_task_A_star(random,_)
	),
	check_energy_switch.

%% normal status strategy

normal_strategy:-
	my_agent(Agent),
	game_predicates:agent_current_position(Agent,CurrP),
	my_map_adjacent(CurrP,AdjPos,Type),!,debug_message('normal_strategy adj':Type),
	(
		Type=o(_)->
			bound(B),game_predicates:agent_current_energy(Agent,E),
			Ask_oracle_cost is B + 10,
			(
				E<Ask_oracle_cost->retractall(bound(_)),assert(bound(Ask_oracle_cost));
				otherwise->find_identity(Type),!,assert(used_internal_objects(Type)),debug_message('asking oracle')
			);
		Type=c(_)->solve_task_A_star(find(o(_)),_);
		otherwise->solve_task_A_star(random,_)
	),
	check_energy_switch.

%% Agenda based A* search %%

solve_task_A_star(Goal,Cost):-
	debug_message('A_star'),
	my_agent(Agent),
	query_world( agent_current_position, [Agent,P] ),
	( 
		Goal = go(Goal_pos) -> map_distance(P,Goal_pos,H);
	 	Goal = find(Goal_pos) -> H is 0;
	 	Goal = random -> H is 0, debug_message('random')
	),
	Ginit is 0,
	DpthInit is 0,
	Finit is H,
	solve_task_A_star(Goal,c(Finit,Ginit,P,[]),[],DpthInit,RR,Cost,NewPos),!,
	reverse(RR,[_Init|R]),(
		query_world( agent_do_moves, [Agent,R] )->true;
		otherwise->debug_message('go again'),solve_task_A_star(Goal,_)
	).


acheived(Goal,c(F,G,p(X,Y),Path_to_goal),Agenda,Dpth,[p(X,Y)|Path_to_goal],G,NewPos):-
	( 	
		Goal = go(p(Xgoal,Ygoal)) -> Xgoal = X,Ygoal = Y;
	 	Goal = find(Goal_pos) ->
	 		map_adjacent(p(X,Y),_,Goal_pos),\+used_internal_objects(Goal_pos);
	 	Goal = random -> 
	 		map_adjacent(p(X,Y),_,Goal_pos),(Goal_pos=o(_);Goal_pos=c(_)),
	 		\+found(Goal_pos),assert(found(Goal_pos))		
	),retractall(seen_pos(_)).

solve_task_A_star(Goal,Current,Agenda,Dpth,RR,Cost,NewPos):-
	Current=c(F0,G0,P0,Path_to_P0),
	add_to_Agenda(Goal,P0,G0,Path_to_P0,Agenda,NewAgenda),
	NewAgenda = [NewCurr|Rest],
	Dpth1 is Dpth+1,(
		acheived(Goal,NewCurr,Rest,Dpth,RR,Cost,NewPos)->debug_message('acheived'),true;
		otherwise->solve_task_A_star(Goal,NewCurr,Rest,Dpth,RR,Cost,NewPos)
	).

add_to_Agenda(Goal,Curr,CurrG,Path_to_P,Agenda,NewAgenda):-
	map_adjacent(Curr,Adj1,empty),
	\+seen_pos(Adj1),
	assert(seen_pos(Adj1)),
	( 
		Goal = go(Goal_pos) -> map_distance(Adj1,Goal_pos,H1);
	 	Goal = find(Goal_pos) -> H1 is 0;
	 	Goal = random -> H1 is 0 
	),
	\+ memberchk(Adj1,Path_to_P),
	\+ memberchk(c(_,_,Adj1,_),Agenda),
	F1 is CurrG+H1+1,	
	G1 is CurrG+1,
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

search(F,N,N,1):-
	map_adjacent(F,N,empty).

%%% command shell %%%

shell:-
	get_input(Input),
	handle_input(Input).

handle_input(Input):-
	( Input = stop -> true
	; Input = setup -> join_game(_A),handle_input(reset)
	; Input = status -> query_world(game_status,[S]),show_response(S),shell
	; Input = reset -> reset_game,start_game,shell
	; Input = whoami -> my_agent(A),show_response(A),shell
	; Input = [H|T] -> handle_input(H),handle_input(T),shell
	; callable(Input,G,R) -> ( call(G) -> show_response(R) ; show_response('This failed.') ),shell
	; otherwise -> show_response('Unknown command, please try again.'),shell
	).

% get input from user
get_input(Input):-
	write('? '),read(Input).

% show answer to user
show_response(R):-
	my_agent(Agent),
	( R=shell(Response)   -> writes('! '),writes(Response),writes(nl)
	; R=console(Response) -> term_to_atom(Response,A),do_command([Agent,console,A])
	; R=both(Response)    -> show_response(shell(Response)),show_response(console(Response))
	; R=agent(Response)   -> term_to_atom(Response,A),do_command([Agent,say,A])
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
callable(topup(S),(my_agent(Agent),query_world( agent_topup_energy, [Agent,S] )),agent(topup)).
callable(energy,(my_agent(Agent),query_world( agent_current_energy, [Agent,E] )),both(current_energy(E))).
callable(position,(my_agent(Agent),query_world( agent_current_position, [Agent,P] )),both(current_position(P))).
callable(ask(S,Q),(my_agent(Agent),query_world( agent_ask_oracle, [Agent,S,Q,A] )),A).
callable(Task,solve_task(Task,Cost),[console(Task),shell(term(Cost))]):-
	task(Task).

task(go(_Pos)).
task(find(_O)).	% oracle o(N) or charging station c(N)
