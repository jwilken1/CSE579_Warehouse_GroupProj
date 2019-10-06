%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File: warehouse.asp: Warehouse Project												     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sort and object declaration														%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For every init statement there is a data(Object, Value, Time) with time set to 0
data(O, V, 0) :- init(O, V).
object(OB,ID) :- data(object(OB,ID),V,0).

% Plan Length/ Time (Needed?)
time(1..n).
horizon(H) :- H = #max{ 0; T : occurs( _, _, T) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state description															%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Objects that are adjacent cannot switch places. Fail if :- (Obj1 at Loc1, T) and (Obj2 at Loc2, T) and  (Obj1 at Loc2, T-1) and (Obj2 at Loc1, T-1), Adjacency Math, Obj1 != Obj2	
:- data(object(OB,OJ1),value(at,pair(X1,Y1)),T), data(object(OB,OJ2),value(at,pair(X2,Y2)),T), data(object(OB,OJ2),value(at,pair(X1,Y1)),T-1), data(object(OB,OJ1),value(at,pair(X2,Y2)),T-1), OJ1!=OJ2, |X1-X2|+|Y1-Y2|==1. 

% No two objects of the same type can be on the same place (Collision)
:- data(object(OB,OJ1),value(at,pair(X,Y)),T), data(object(OB,OJ2),value(at,pair(X,Y)),T), OJ2!=OJ1.

% A robot cannot place a shelf on a highway node. 
:- data(object(highway,ID),value(at,pair(X,Y)),T), data(object(robot,R),value(at,pair(X,Y)),T), occurs(object(robot,R),putdown,T), time(T).

% Picking station / Node / Highway cannot move
data(object(pickingStation,PS),value(at,pair(X,Y)),T+1)  :- data(object(pickingStation,PS),value(at,pair(X,Y)),T), T=0..n-1. 
data(object(node,N),value(at,pair(X,Y)),T+1)  :- data(object(node,N),value(at,pair(X,Y)),T), T=0..n-1. 
data(object(highway,H),value(at,pair(X,Y)),T+1) :- data(object(highway,H),value(at,pair(X,Y)),T), T=0..n-1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% effect and preconditions of action													%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% An object can only do a single action per given time slot.
:- {occurs(object(OB,ID),A,T)} > 1, object(OB,ID), time(T).


%%%%%%%%%%%%%%%%% Move %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),move('dx','dy'),'t'). 

% Movements can be: deltaX, deltaY: (0,1), (1,0), (0,-1), or (-1,0). This ensures that movement is to an adjacent Node. 
action(move(0,1;1,0;0,-1;-1,0)).


%% Movement must be possible.
% Target movement must be to a node in the warehouse. Fail if :- Movement, Initial Location, Not a node at new location.
:- occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X1   ,Y1   )), T-1), not data(object(node,_),value(at,pair(X1+DX,Y1+DY)),T).

% Condition for swithcing places covered under State Description for all objects. 

%% Define Movement for robot. New Location :- Movement, Initial Location
data(object(robot,R),value(at,pair(X1+DX,Y1+DY)),T) :- occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X1,Y1)), T-1).
% No longer at old location
:- data(object(robot,R),value(at,pair(X1,Y1)),T-1), occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X2,Y2)),T), (X1,Y1)=(X2,Y2).

% Define Shelf being carried is moved as well : Shelf is at location X,Y if :- robot is at location X,Y and robot carries shelf at T-1.  
data(object(shelf,S),value(at,pair(X,Y)),T) :- data(object(robot,R),value(at,pair(X,Y)),T), data(object(robot,R),value(carries,object(shelf,S)),T-1). 

%% Testing robot Movement first %% Works
%:- not data(object(robot,1),value(at,pair(1,1)),H), horizon(H).
%:- not data(object(robot,2),value(at,pair(1,1)),H), horizon(H). % Doesn't work as expected. Coll node 1.
%:- not data(object(robot,2),value(at,pair(1,2)),H), horizon(H).
%:- not data(object(robot,2),value(at,pair(2,2)),H), horizon(H). % Doesn't work as expected. Dup node 2.


%%%%%%%%%%%%%%%%% Pickup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Denoted as occurs(object(robot,'r'),pickup,'t').

%Pickup can be: (pickup)
action(pickup).

% Robot cannot be carrying a shelf already.
:- occurs(object(robot,R),pickup,T), data(object(robot,R),value(carries,O),T-1).

% Shelf must be at pickup location of robot.
:- occurs(object(robot,R),pickup,T), data(object(robot,R),value(at,pair(X,Y)),T-1), not data(object(shelf, _),value(at,pair(X,Y)),T-1).

%%** Need to make a carries type effect for this. 
data(object(robot,R),value(carries,object(shelf,S)),T) :- occurs(object(robot,R),pickup,T), data(object(shelf,S),value(at,pair(X,Y)),T-1), data(object(robot,R),value(at,pair(X,Y)),T-1).

%% Testing pickup
%:- not data(object(robot,2),value(carries,object(shelf,3)),H), horizon(H).
%:- not data(object(robot,1),value(carries,object(shelf,2)),H), horizon(H).	 

%%%%%%%%%%%%%%%%% Putdown %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),putdown,'t').

% Action can be: (putdown)
action(putdown).
	
% Robot Needs to be carrying a shelf at previous timeslot.
:- occurs(object(robot,R),putdown,T), not data(object(robot,R),value(carries,_),T-1).

% Result of putdown: Not Carrying at T if :- putdown at T and carries at T-1
not data(object(robot,R),value(carries,O),T) :- occurs(object(robot,R),putdown,T), data(object(robot,R),value(carries,O),T-1).

%% Testing putdown
%:- not data(object(robot,2),value(carries,object(shelf,3)),T), T=2. %% Pickup and get ready to put down
%:- not data(object(robot,1),value(carries,object(shelf,2)),T), T=5. %% Pickup and get ready to put down
%:- not occurs(object(robot,1),putdown,7).
%:- not occurs(object(robot,2),putdown,4).

%%%%%%%%%%%%%%%%% Deliver %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Denoted as occurs(object(robot,'r'),deliver('o','i','u'),'t').
% data(object(order,1),value(line,pair(3,4)),0) 

% Action can be: deliver(order 'o', product 'i', units 'u'). U2 and U1 are used for max value that can be delivered due to size of order. 
action(deliver(O,I,U2)) :- data(object(order,O),value(line,pair(I,U1)),0),U2=1..U1.

% Robot needs to be carrying a shelf
:- not data(object(robot,R),value(carries,_),T-1), occurs(object(robot,R),deliver(O,I,U),T).

% Robot needs to be at station    
:- occurs(object(robot,R),deliver(O,I,U),T), data(object(robot,R),value(at,pair(X,Y)),T-1), not data(object(pickingStation,_),value(at,pair(X,Y)),T-1).

% Correct product needs to be at the correct picking station / Item.-- Having issues with this so addin more requirements to action effect for testing.
:- occurs(object(robot,R),deliver(O,I,U),T), data(object(robot,R),value(at,pair(X,Y)),T-1), data(object(pickingStation,P),value(at,pair(X,Y)),T-1), not data(object(order,O),value(pickingStation,P),T-1).
:- occurs(object(robot,R),deliver(O,I,U),T), data(object(robot,R),value(carries,object(shelf,S)),T-1), not data(object(product,I),value(on,pair(S,_)),T-1). 

% Robot can only deliver the amount of product availible on shelf. 
:- occurs(object(robot,R),deliver(O,I,U2),T), data(object(robot,R),value(carries,S),T-1), data(object(product,I),value(on,pair(S,U1)),T-1), U2>U1.

% Robot cannot deliver more then requested for order. 
:- occurs(object(robot,R),deliver(O,I,U2),T), data(object(order,O),value(line,pair(I,U1)),T-1), U1<U2.

% At least one product must be delivered. 
:- occurs(object(robot,R),deliver(O,I,0),T).

%% Effects of action
% Order lines need to be updated. 
data(object(order,O),value(line,pair(I,U1-U2)),T) :- occurs(object(robot,R),deliver(O,I,U2),T), 
								data(object(order,O),value(line,pair(I,U1)),T-1), 
								data(object(robot,R),value(at,pair(X,Y)),T-1), 
								data(object(order,O),value(pickingStation,P),T-1), 
								data(object(pickingStation,P),value(at,pair(X,Y)),T-1).

% Product quantity on shelf must be updated. 
data(object(product,I),value(on,pair(S,U1-U2)),T) :- occurs(object(robot,R),deliver(O,I,U2),T), data(object(robot,R),value(carries,S),T-1), data(object(product,I),value(on,pair(S,U1)),T-1), not U1<U2.

% Ensure old data is removed (Units cannot remain the same.)
:- occurs(object(robot,R),deliver(O,I,U),T), data(object(robot,R),value(carries,S),T-1), data(object(product,I),value(on,pair(S,U1)),T-1), data(object(product,I),value(on,pair(S,U2)),T), U1=U2.

%% Test Deliver
%:- not occurs(object(robot,1),deliver(1,3,1),10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% domain independent axioms																												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%% fluents are initially exogenous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Only 1 location for each object on init. 
:- {data(object(OB,ID),value(at,pair(X,Y)),0)} > 1, object(OB,ID).

% Only 1 pickingstation per order on init
:- {data(object(order,ID),value(pickingStation,S),0)} > 1, object(order,ID).


%%%%%%%%%%%%%%%%% uniqueness and existence of fluent values %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% An object can only be in one place at any given time. 
:- {data(object(OB,ID),value(at,pair(X,Y)),T)} > 1, object(OB,ID), time(T).

% There must be one picking station, and only one, for every order. 
:- {data(object(order,ID),value(pickingStation,S),T)} > 1, object(order,ID), time(T).

% Definition of carries
{data(object(robot,R),value(carries,object(shelf,S)),T+1)} :- data(object(robot,R),value(carries,object(shelf,S)),T), T = 0..n-1.


%%%%%%%%%%%%%%%%% actions are exogenous %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{occurs(object(OJ,ID),A,T)} :- object(OJ,ID), action(A), T=0..n.


%%%%%%%%%%%%%%%%% commonsense law of inertia %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Object stays at location unless moved.
{data(O, V, T+1)} :- data(O, V, T), T = 0..n-1.

% Robot cannot move w/o action
{data(object(robot,R),value(at,pair(X,Y)),T+1)} :- data(object(robot,R),value(at,pair(X,Y)),T), T = 0..n-1.
data(object(robot,R),value(at,pair(X,Y)),T+1) :- data(object(robot,R),value(at,pair(X,Y)),T), not occurs(object(robot,R),move(_,_),T+1), T = 0..n-1.

% Picking station cannot move w/o action
data(object(order,O),value(pickingStation,P),T+1)  :- data(object(order,O),value(pickingStation,P),T), T=0..n-1.

% Shelf cannot move w/o action
{data(object(shelf,S),value(at,pair(X,Y)),T+1)} :- data(object(shelf,S),value(at,pair(X,Y)),T), T = 0..n-1.
data(object(shelf,S),value(at,pair(X,Y)),T+1) :- data(object(shelf,S),value(at,pair(X,Y)),T), not data(object(robot,_),value(carries,object(shelf,S)),T+1), T = 0..n-1.


% Robot contiues to carry object. 
data(object(robot,R),value(carries,object(shelf,S)),T+1) :- data(object(robot,R),value(carries,object(shelf,S)),T), not occurs(object(robot,R),putdown,T+1), T = 0..n-1.

% Robot cannot carry object if it wasnt at last timestamp and pickup didnt occur
:- data(object(robot,R),value(carries,object(shelf,S)),T+1), not data(object(robot,R),value(carries,object(shelf,S)),T), not occurs(object(robot,R),pickup,T+1), T = 0..n-1.

% Shelf product value stays consistent
{data(object(product,I),value(on,pair(S,U)),T+1)} :- data(object(product,I),value(on,pair(S,U)),T), T = 0..n-1.
data(object(product,I),value(on,pair(S,U)),T+1) :- data(object(product,I),value(on,pair(S,U)),T), not data(object(robot,_),value(carries,object(shelf,S)),T), T = 0..n-1.
data(object(product,I),value(on,pair(S,U)),T+1) :- data(object(product,I),value(on,pair(S,U)),T), data(object(robot,R),value(carries,object(shelf,S)),T), not occurs(object(robot,R),deliver(_,I,_),T+1), T = 0..n-1.

%% Order remains consistent unless delivered
% Order Lines cannot move -- Had to do 3 checks in order to properly detect actions.
{data(object(order,O),value(line,pair(I,U)),T+1)} :- data(object(order,O),value(line,pair(I,U)),T), T = 0..n-1.
data(object(order,O),value(line,pair(I,U)),T+1) :- data(object(order,O),value(line,pair(I,U)),T), 
							data(object(order,O),value(pickingStation,P),T), 
							data(object(pickingStation,P),value(at,pair(X,Y)),T), 
							not data(object(robot,_),value(at,pair(X,Y)),T) , T = 0..n-1.
data(object(order,O),value(line,pair(I,U)),T+1) :- data(object(order,O),value(line,pair(I,U)),T), 
							data(object(order,O),value(pickingStation,P),T), 
							data(object(pickingStation,P),value(at,pair(X,Y)),T), 
							data(object(robot,R),value(at,pair(X,Y)),T),
							not occurs(object(robot,R),deliver(O,I,_),T+1), T = 0..n-1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% goal															                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Order Fullfillment when order lines are at 0. 
:- not data(object(order,O),value(line,pair(I,0)),H), init(object(order,O),value(line,pair(I,U))), horizon(H).

% Minimize time for max H
#minimize {H : horizon(H)}.

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
%#show ab/1.


