% For every init statement there is a data(Object, Value, Time) with time set to 0
data(O, V, 0) :- init(O, V).
object(O) :- data(object(O),V,0).

horizon(H) :- H = #max{ 0; T : occurs( _, _, T) }.
time(1..H) :-  horizon(H).

% Order Fullfillment when orders at 0. 
%:- not data(object(order,_),value(line,(_,0)),T), T=n.
:- not data(object(robot,1),value(at(2,4)),H), horizon(H).


% An object can only do a single action per given time slot.
%:- {occurs(object(OB,ID),A,T)} > 1, object(OB,ID), time(T).


move(0,1;1,0;0,-1;-1,0).
%occurs(object(OT, O), action(move(0,1;1,0;0,-1;-1,0)), T).
%occurs(object(robot,_),action(move(0,1;1,0;0,-1;-1,0)),_).
data(object(robot,R),value(at(X1+DX,Y1+DY)),T) :- occurs(object(robot,R),action(move(DX,DY)),T), data(object(robot,R),value(at,pair(X1,Y1)), T-1).
:- data(object(robot,R),value(at,pair(X,Y)), T-1), occurs(object(robot,R),action(move(_)),T), data(object(robot,R),value(at,pair(X,Y)), T).

%:- holds( object(robot, R), value(at,      (X, Y)), T-1); occurs(object(robot, R), action(move,        _), T  ).


%data(object(shelf,S),value(at,(X,Y)),T) :- data(object(robot,R),value(at,(X,Y)),T), data(object(robot,R),value(carries,S),T-1). 


%action(pickup).
%data(object(robot,R),value(carries,S),T) :- occurs(object(robot,R),action(pickup),T), data(object(shelf,S),value(at,(X, Y)),T-1), data(object(robot,R),value(at,(X, Y)),T-1).



%action(putdown).
%not data(object(robot,R),value(carries,O),T) :- occurs(object(robot,R),action(putdown),T), data(object(robot,R),value(carries,O),T-1).

{occurs(O,A,T)} :- object(O), action(A), time(T).
{data(O, V, T+1)} :- data(O, V, T), time(T).
{data(object(robot,R),value(carries,O),T+1)} :- data(object(robot,R),value(carries,O),T), time(T).

action(deliver(O,I,U2)) :- data(object(order,O),value(line,(I,U1)),0),U2=1..U1.
data(object(order,O),value(line,(I,U1-U2)),T) :- occurs(object(robot,R),action(deliver(O,I,U2)),T), data(object(order,O),value(line,(I,U1)),T-1), U2 < U1.
data(object(product,I),value(on,(S,U1-U2)),T) :- occurs(object(robot,R),action(deliver(_,I,U2)),T), data(object(robot,R),value(carries,S),T-1), data(object(product,I),value(on,(S,U1)),T-1), U1>U2.

#show data/3.
#show horizon/1.
#show occurs/3.

