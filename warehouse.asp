%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File: warehouse.asp: Warehouse Project												     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sort and object declaration														%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For every init statement there is a data(Object, Value, Time) with time set to 0
data(O, V, 0) :- init(O, V).
object(OB,ID) :- data(object(OB,ID),V,0).

highway(X,Y) :- init(object(highway,ID),value(at,pair(X,Y))). %%May be used later for expedience
node(X,Y) :- data(object(node,ID),value(at,pair(X,Y)), 0). %%May be used later for expedience
				
%at(object(OB,ID),loc(X,Y),T) :- data(object(OB,ID),value(at,pair(X,Y)),T), time(T).

% Product: i= product label, s= shelf located on, u = amount availible --> on(PID, Shelf, Units, Time)  Shouldnt need...for now
%on(ID, S, U, 0) :- data(object(product,ID),value(on,pair(S,U)),0). Shouldnt need...for now


% Plan Length/ Time (Needed?)
time(1..n). 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state description															%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Objects that are adjacent cannot switch places. Fail if :- (Obj1 at Loc1, T) and (Obj2 at Loc2, T) and  (Obj1 at Loc2, T-1) and (Obj2 at Loc1, T-1), Adjacency Math, Obj1 != Obj2	
:- data(object(OB,OJ1),value(at(X1,Y1)),T), data(object(OB,OJ2),value(at(X2,Y2)),T), data(object(OB,OJ2),value(at(X1,Y1)),T-1), data(object(OB,OJ1),value(at(X2,Y2)),T-1), OJ1!=OJ2, |X1-X2|+|Y1-Y2|==1. 



% Order Fullfillment when orders at 0. 



% A robot cannot place a shelf on a highway node. 
:- data(object(highway,ID),value(at,pair(X,Y)),T), data(object(robot,R),value(at,(X,Y)),T), occurs(object(robot,R),action(putdown,O),T), time(T).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% effect and preconditions of action													%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Overall definition of action
%occurs(O,A,T) :-  T=1-n.

% An object can only do a single action per given time slot.
:- {occurs(object(OB,ID),A,T)} > 1, object(OB,ID), time(T).


%%%%%%%%%%%%%%%%% Move %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),move('dx','dy'),'t'). 

% Movements can be: deltaX, deltaY: (0,1), (1,0), (0,-1), or (-1,0). This ensures that movement is to an adjacent Node. 
%occurs(object(robot,R),move(0,1;1,0;0,-1;-1,0),T).  Think this is not needed and below will work.
action(move(0,1;1,0;0,-1;-1,0)).
%move(0,1;1,0;0,-1;-1,0).

%% Movement must be possible.
% Target movement must be to a node in the warehouse. Fail if :- Movement, Initial Location, Not a node at new location. 
:- occurs(object(robot,R),action(move(DX,DY)),T), data(object(robot,R),value(at,pair(X1,Y1)), T-1), not data(object(node,_),value(at(X1+DX,Y1+DY)),T).

% Condition for swithcing places covered under State Description for all objects. 

% Define Movement for robot. New Location :- Movement, Initial Location
data(object(robot,R),value(at(X1+DX,Y1+DY)),T) :- occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X1,Y1)), T-1).

% Define Shelf being carried is moved as well : Shelf is at location X,Y if :- robot is at location X,Y and robot carries shelf at T-1.  
data(object(shelf,S),value(at,(X,Y)),T) :- data(object(robot,R),value(at,(X,Y)),T), data(object(robot,R),value(carries,S),T-1). 


%%%%%%%%%%%%%%%%% Pickup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Denoted as occurs(object(robot,'r'),pickup,'t').

%Pickup can be: (pickup)
action(pickup).

% Robot cannot be carrying a shelf already.
:- occurs(object(robot,R),action(pickup),T), data(object(R,N),value(carries,O),T-1).

% Shelf must be at pickup location.
:- occurs(object(robot,R),action(pickup),T), data(object(robot,R),value(at,(X,Y)),T-1), not data(object(shelf, _),value(at,(X,Y)),T-1).

% If robot picks up a shelf, it should be at the same location as the shelf at the previous time slot. 
:- occurs(object(robot,R),action(pickup),T), data(object(robot,R),value(at,(X,Y)),T-1), not data(object(shelf,_),value(at,(X,Y)),T-1).

%%** Need to make a carries type effect for this. 
data(object(robot,R),value(carries,S),T) :- occurs(object(robot,R),action(pickup),T), data(object(shelf,S),value(at,(X, Y)),T-1), data(object(robot,R),value(at,(X, Y)),T-1).	 

%%%%%%%%%%%%%%%%% Putdown %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),putdown,'t').

% Action can be: (putdown)
action(putdown).
	
% Robot Needs to be carrying a shelf at previous timeslot.
:- occurs(object(robot,R),action(putdown),T), not data(object(robot,R),value(carries,_),T-1).

% Result of putdown: Not Carrying at T if :- putdown at T and carries at T-1
not data(object(robot,R),value(carries,O),T) :- occurs(object(robot,R),action(putdown),T), data(object(robot,R),value(carries,O),T-1).

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
%1{data(object(OB,ID),value(at,pair(X,Y)),0):value(at,pair(X,Y))}1 :- object(OB,ID).
%:- {data(object(OB,_),value(at, (X, Y)),0)} > 1, object(OB,ID), data(object(node,_),value(at, (X, Y)),0).
%1{at(object(OB,ID),loc(X,Y),0):loc(X,Y)}1 :- object(OB,ID).

% Only 1 location for each object on init. 
:- {data(object(OB,ID),value(at,pair(X,Y)),0)} > 1, object(OB,ID).

% Only 1 pickingstation per order on init
:- {data(object(order,ID),value(pickingStation,S),0)} > 1, object(order,ID).


%%%%%%%%%%%%%%%%% uniqueness and existence of fluent values %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An object can only be in one place at any given time. 
%:- {data(object(OB,_),value(at,pair(X,Y)),0)} > 1, data(object(node,_), value(at,(X, Y)),0), init( object(OB,_), _). Could not get to work..abandoned. 
:- {data(object(OB,ID),value(at,pair(X,Y)),T)} > 1, object(OB,ID), T = 1..n.

% There must be one picking station, and only one, for every order. 
%:- not 1{data(object(order,O), value(pickingStation,P), T)}1, data(object(order, O), V, T), T=1..n. Could not get to work..abandoned. 
:- {data(object(order,ID),value(pickingStation,S),T)} > 1, object(order,ID), T = 1..n.


%%%%%%%%%%%%%%%%% actions are exogenous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{occurs(O,A,T)} :- occurs(O,A,T), T = 0..n-1.


%%%%%%%%%%%%%%%%% commonsense law of inertia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Object stays at location unless moved.
{data(O, V, T+1)} :- data(O, V, T), T = 0..n-1.

% Robot contiues to carry object. 
%{carries(object(robot,R),O,T+1)} :- carries(object(robot,R),O,T), T = 0..n-1. %% Got rid of carries object. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Show
#show occurs/3.
%#show data/3.
%#show object/2.
%#show at/3.
%#show value/2.
%#show on/4.
%#show err/3.
%#show highway/2.
%#show node/2.


