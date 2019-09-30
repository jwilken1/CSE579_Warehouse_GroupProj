%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File: warehouse.asp: Warehouse Project												     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sort and object declaration														%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For every init statement there is a location(Object, Value, Time) with time set to 0
location(O, V, 0) :- init(O, V).

% Plan Length/ Time (Needed?)
time(1..n). 

%#show location/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state description															%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Only one of each type of object can be on a single node at any given time. 


% Objects that are adjacent cannot switch places. Fail if :- (Obj1 at Loc1, T) and (Obj2 at Loc2, T) and  (Obj1 at Loc2, T-1) and (Obj2 at Loc1, T-1), Adjacency Math, Obj1 != Obj2	
:- location(object(OB,OJ1),value(at(X1,Y1)),T), location(object(OB,OJ2),value(at(X2,Y2)),T), location(object(OB,OJ2),value(at(X1,Y1)),T-1), location(object(OB,OJ1),value(at(X2,Y2)),T-1), OJ1!=OJ2, |X1-X2|+|Y1-Y2|==1. 



% Order Fullfillment when orders at 0. 



% A robot cannot place a shelf on a highway node. 



% An object can only do a single action per given time slot.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% effect and preconditions of action													%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Overall definition of action
%occurs(O,A,T) :-  T=1-n.

%  An object can only do one action at a time.


%%%%%%%%%%%%%%%%% Move %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),move('dx','dy'),'t'). 

% Movements can be: deltaX, deltaY: (0,1), (1,0), (0,-1), or (-1,0). This ensures that movement is to an adjacent Node. 
%occurs(object(robot,R),move(0,1;1,0;0,-1;-1,0),T).  Think this is not needed and below will work.
action(move(0,1;1,0;0,-1;-1,0)).

% Target movement must be to a node in the warehouse. Fail if :- Movement, Initial Location, Not a node. 
:- occurs(object(robot,R),move(DX,DY),T), location(object(robot,R),value(at,pair(X1,Y1)), T-1), not location(object(node,_),value(at(X1+DX,Y1+DY)),T).

% Condition for swithcing places covered under State Description for all objects. 

% Define Movement for robot. New Location :- Movement, Initial Location
location(object(robot,R),value(at(X1+DX,Y1+DY)),T) :- occurs(object(robot,R),move(DX,DY),T), location(object(robot,R),value(at,pair(X1,Y1)), T-1).

	% Define Shelf being carried is moved as well. 


	% Movement must be possible. 


%%%%%%%%%%%%%%%%% Pickup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Denoted as occurs(object(robot,'r'),pickup,'t').

%Pickup can be: (pickup, time)
action(pickup).

% Robot cannot be carrying a shelf already.
:- occurs(object(robot,R),action(pickup,O),T), location(object(R,N),value(carries,O),T-1).

% Shelf must be at pickup location.


% If robot picks up a shelf, it should be at the same location as the shelf at the previous time slot. 


%%** Need to make a carries type effect for this. 
	 

%%%%%%%%%%%%%%%%% Putdown %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),putdown,'t').

% Action can be: (putdown, time)
action(putdown).
	
% Robot Needs to be carrying a shelf at previous timeslot.
:- occurs(object(robot,R),putdown,T), not location(object(robot,R),value(carries,_),T-1).

% Result of putdown: Not Carrying at T if :- putdown at T and carries at T-1
not location(object(robot,R),value(carries,O),T) :- occurs(object(robot,R),putdown,T), location(object(robot,R),value(carries,O),T-1).

%%%%%%%%%%%%%%%%% Deliver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%occurs(object(robot,'r'),deliver('o','i','u'),'t').
	% Robot needs to be carrying a shelf
	% Robot needs to be at station
	% Correct product needs to be at the correct picking station. 
	% Robot can only deliver the amount of product availible on shelf. 
	% Robot cannot deliver more then requested for order. 
	% At least one product must be delivered. 

	% Order lines need to be updated. 
	% Product quantity on shelf must be updated. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% domain independent axioms																												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% fluents are initially exogenous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%% uniqueness and existence of fluent values %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An object can only be in one place at any given time. 


% There must be one picking station, and only one, for every order. 


%%%%%%%%%%%%%%%%% actions are exogenous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%% commonsense law of inertia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{location(O, V, T+1)} :- location(O, V, T), T = 0..n-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show
#show occurs/3.



