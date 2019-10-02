% For every init statement there is a data(Object, Value, Time) with time set to 0
data(O, V, 0) :- init(O, V).
object(OJ,ID) :- data(object(OJ,ID),V,0).

%horizon(H) :- H = #max{ 0; T : occurs( _, _, T) }.
horizon(H) :- H = n.
time(0..H) :-  horizon(H).

% Order Fullfillment when orders at 0. 
%:- not data(object(order,_),value(line,(_,0)),T), T=n.
:- not data(object(robot,1),value(at,pair(1,1)),H), horizon(H).


% An object can only do a single action per given time slot.
%:- {occurs(object(OB,ID),A,T)} > 1, object(OB,ID), time(T).


action(move(0,1;1,0;0,-1;-1,0)).

%occurs(object(robot,1),move(0,1),1).

data(object(robot,R),value(at,pair(X1+DX,Y1+DY)),T) :- occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X1,Y1)), T-1).
:- data(object(robot,R),value(at,pair(X1,Y1)),T-1), occurs(object(robot,R),move(DX,DY),T), data(object(robot,R),value(at,pair(X2,Y2)),T), (X1,Y1)=(X2,Y2).


%data(object(shelf,S),value(at,(X,Y)),T) :- data(object(robot,R),value(at,(X,Y)),T), data(object(robot,R),value(carries,S),T-1). 


%action(pickup).
%data(object(robot,R),value(carries,S),T) :- occurs(object(robot,R),action(pickup),T), data(object(shelf,S),value(at,(X, Y)),T-1), data(object(robot,R),value(at,(X, Y)),T-1).



%action(putdown).
%not data(object(robot,R),value(carries,O),T) :- occurs(object(robot,R),action(putdown),T), data(object(robot,R),value(carries,O),T-1).

{occurs(object(OJ,ID),A,T)} :- object(OJ,ID), action(A), time(T).
{data(O, V, T+1)} :- data(O, V, T), time(T).

:- {data(object(OB,ID),value(at,pair(X,Y)),T)} > 1, object(OB,ID), time(T).

:- {occurs(object(OB,ID),A,T)} > 1, object(OB,ID), time(T).

#show data/3.
%#show horizon/1.
#show occurs/3.
%#show object/2.
#show action/1.
#show time/1.
