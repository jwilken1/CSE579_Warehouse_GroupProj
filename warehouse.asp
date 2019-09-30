%%%%%%%%%%%%%%%%%%%
% File: warehouse.asp: Warehouse Project
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sort and object declaration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For every init statement there is a location(Object, Value, Time) with time set to 0
location(O, V, 0) :- init(O, V).

% Plan Length/ Time (Needed?)
%time(1..n). 

#show location/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%
% state description
%%%%%%%%%%%%%%%%%%%%%%%%%%

% An object can only be in one place at any given time. 



% Only one of each type of object can be on a node at any given time. 



% There must be one picking station, and only one, for every order.  


% Order Fullfillment when orders at 0. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% effect and preconditions of action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% General Rules %%

% An object can only do a single action per given time slot. 


% Two similar objects that are adjacent cannot swap places (collision). 


% A robot cannot place a shelf on a highway node. 

%% Move %%
	%occurs(object(robot,'r'),move('dx','dy'),'t').
				%move, (0,1).
				%move, (1,0).
				%move, (0,-1).
				%move, (-1,0).

	% Target node much exist
	% Movement must be possible. 
	% Define Adjecentcy. ?

	% Define effect of movement
		% Define movement of shelf if robot is carrying it. 	 

%% Pickup %%
	%occurs(object(robot,'r'),pickup,'t').
	% Robot cannot be carrying a shelf already.
	% Shelf must be at pickup location.
	% If robot picks up a shelf, it should be at the same location as the shelf at the previous time slot. 

%%** Need to make a carries type effect for this. 
	 

%% Putdown %%
%occurs(object(robot,'r'),putdown,'t').
	% Robot Needs to be carrying a shelf at previous timeslot.
	

%% Deliver %%
%occurs(object(robot,'r'),deliver('o','i','u'),'t').
	% Robot needs to be carrying a shelf
	% Robot needs to be at station
	% Correct product needs to be at the correct picking station. 
	% Robot can only deliver the amount of product availible on shelf. 
	% Robot cannot deliver more then requested for order. 
	% At least one product must be delivered. 

	% Order lines need to be updated. 
	% Product quantity on shelf must be updated. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% domain independent axioms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% commonsense law of inertia
{location(O, V, T+1)} :- location(O, V, T), T<n.